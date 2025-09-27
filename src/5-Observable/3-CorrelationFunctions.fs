// 3-CorrelationFunctions.fs
// Implements correlation functions based on the rigorous "Projected Correlator"
// theory from the Algebraic Observables Engine (AOE).
namespace E8.Observable

open System
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.Ace

/// Defines a local operator and its representation in the virtual basis of the environment.
type LocalOperator = {
    Name: string
    // A function that creates the matrix representation of the operator for a given dimension.
    ToVirtualMatrix: int -> MatrixF2
}

/// Contains the core logic for computing correlation functions algebraically.
module CorrelationComputation =

    /// Computes the fixed point eigenvector (or density matrix) of a projector matrix T.
    /// The fixed point space is the image (column space) of T.
    let private computeFixedPoint (transferMatrix: MatrixF2) (ops: IF2Operations) : MatrixF2 =
        let size = transferMatrix.Rows
        // For a projector T, the fixed point rho_fixed is the projector onto its image space.
        // In the simplest case of a single dominant eigenvector (rank=1), rho_fixed is |v><v|.
        // For a degenerate eigenspace (rank > 1), it's the projector onto that space.
        // The transfer matrix T itself is already the projector onto its image.
        // Therefore, for a projector, the fixed point density matrix is T itself, normalized.
        // In F2, normalization is non-trivial. We use the projector T directly,
        // as it correctly projects any state into the fixed-point subspace.
        transferMatrix

    /// Computes the parity of a two-point correlation function <O_i O_j>
    /// using the projector property of the transfer matrix (T^2=T).
    let computeProjectedCorrelation
        (environment: ACE_CTMRG.SymmetricEnvironment)
        (peps: Tensor4F2)
        (location1: int * int)
        (location2: int * int)
        (op1: LocalOperator)
        (op2: LocalOperator)
        (ops: IF2Operations) : ObservableValue =

        // 1. Build the transfer matrix T from the converged environment.
        let (transferMatrixData, size) = TransferMatrix.constructTransferMatrix environment peps ops

        // 2. Verify the projector hypothesis and get spectral info.
        let verification = ProjectorSpectral.verifyProjectorHypothesis transferMatrixData size ops
        let spectralInfo = ProjectorSpectral.getSpectralInfo verification
        let confidence = spectralInfo.MinimalPolynomialDegree

        // 3. Represent T as a MatrixF2 for algebraic manipulation.
        let t_matrix = MatrixF2(size, size, fun i j -> { Bits = transferMatrixData.[i * ((size+63)/64) + j/64].Bits >>> (j%64) &&& 1UL } |> F2.op_Explicit)

        // 4. Compute the fixed point density matrix rho_fixed from T.
        let rho_fixed = computeFixedPoint t_matrix ops

        // 5. Represent local operators in the virtual basis.
        let op1_matrix = op1.ToVirtualMatrix(size)
        let op2_matrix = op2.ToVirtualMatrix(size)

        // 6. Compute the correlation by evaluating the full, correct trace expression.
        // C(i,j) = Tr(rho_fixed * O_i * T^r * O_j), where r = |i-j|.
        // Since T is a projector, T^r = T for r >= 1.
        let distance = abs(fst location1 - fst location2) + abs(snd location1 - snd location2)

        let correlationMatrix =
            if distance = 0 then
                // This is a 1-point function: <O_i^2>.
                // In F2, for Pauli operators, O^2 = I. The expectation is Tr(rho_fixed * I) = Tr(rho_fixed).
                // Tr(rho_fixed) = Tr(T) = Rank(T).
                let m = MatrixF2(1,1) // Create a dummy matrix to hold the result
                m.[0,0] <- if spectralInfo.Degeneracy % 2 = 1 then F2.One else F2.Zero
                m
            else
                // This is a 2-point function: Tr(rho_fixed * O_i * T * O_j)
                let term1 = rho_fixed * op1_matrix
                let term2 = term1 * t_matrix
                let term3 = term2 * op2_matrix
                term3

        let correlationParity = correlationMatrix.Trace()

        {
            Value = CorrelationParity correlationParity
            AlgebraicConfidence = confidence
            ContributionCount = size * size
        }

/// Computes the two-point correlation function <O_i O_j>.
type ProjectedCorrelator() =
    interface IObservable with
        member _.Kind = TwoPointCorrelation(0,0)

        member this.Compute environment peps location (op1: LocalOperator) (op2: LocalOperator) ops =
            let pos1 = location.PrimaryPosition
            let pos2 =
                match location.SecondaryPositions with
                | head :: _ -> head
                | [] -> pos1

            CorrelationComputation.computeProjectedCorrelation environment peps pos1 pos2 op1 op2 ops

        member this.ComputeBatch environment peps locations (op1: LocalOperator) (op2: LocalOperator) ops =
            locations
            |> Array.Parallel.map (fun loc -> (this :> IObservable).Compute environment peps loc op1 op2 ops)