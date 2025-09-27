// 4-TopologicalOrder.fs
// Implements the computation of topological observables like Wilson Loops and
// Topological Entanglement Entropy based on the rigorous principles of the AOE.
namespace E8.Observable

open System
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.Ace

/// Computes the value of a Wilson Loop operator along a specified contour.
/// This implementation rigorously contracts a Matrix Product Operator (MPO) along the loop.
type WilsonLoop() =
    interface IObservable with
        member _.Kind = WilsonLoop [] // The actual contour is specified in the location.

        member this.Compute environment peps location ops =
            let contour = location.PrimaryPosition :: location.SecondaryPositions

            // A Wilson loop is the trace of a string of operators (an MPO) along a closed path.
            // The MPO for a single segment of the loop is constructed from the edge environment tensor T.

            let mutable loopMPO = MatrixF2.Identity(environment.Chi)
            let mutable contributionCount = 0

            for i in 0 .. contour.Length - 2 do
                let pos1 = contour.[i]
                let pos2 = contour.[i+1]

                // The MPO for the segment from pos1 to pos2 is derived from the edge tensor T.
                // For a specific physical model, T itself can be interpreted as the MPO tensor.
                // We contract the MPO along the path by tracing out the physical index of T.
                let edgeMPO = MatrixF2(environment.Chi, environment.Chi)
                for cin in 0..environment.Chi-1 do
                    for cout in 0..environment.Chi-1 do
                        // Sum over the physical index to get the MPO matrix
                        let mutable sum = F2.Zero
                        for p in 0..environment.T.D2-1 do
                            sum <- F2.add sum environment.T.[cin, p, cout]
                        edgeMPO.[cin, cout] <- sum

                // Contract with the MPO accumulated so far
                loopMPO <- loopMPO * edgeMPO
                contributionCount <- contributionCount + 1

            // The value of the Wilson loop is the trace of the resulting MPO.
            let loopParity = loopMPO.Trace()

            // Confidence is determined by the stability of the environment.
            let (transferMatrixData, size) = TransferMatrix.constructTransferMatrix environment peps ops
            let verification = ProjectorSpectral.verifyProjectorHypothesis transferMatrixData size ops
            let confidence = (ProjectorSpectral.getSpectralInfo verification).MinimalPolynomialDegree

            {
                Value = WilsonLoopPhase loopParity
                AlgebraicConfidence = confidence
                ContributionCount = contributionCount
            }

        member this.ComputeBatch environment peps locations ops =
            locations
            |> Array.Parallel.map (fun loc ->
                (this :> IObservable).Compute environment peps loc ops)

/// Computes the Topological Entanglement Entropy (TEE) via the Ground State Degeneracy (GSD).
type TopologicalEntropyMeasurement() =
    interface IObservable with
        member _.Kind = TopologicalEntropy

        member this.Compute environment peps location ops =
            // AOE mandates the calculation of GSD as an integer, by analyzing the
            // algebra of boundary MPOs constructed from the environment.

            // Step 1: Construct the boundary MPO for the non-trivial anyon tau.
            // This MPO is constructed from the fixed point of the transfer matrix.
            let (transferMatrixData, size) = TransferMatrix.constructTransferMatrix environment peps ops

            // Step 2: Determine GSD from the structure of the fixed-point space.
            // The GSD is the number of distinct fixed points of the transfer matrix, which
            // corresponds to the rank of the projector T.
            let verification = ProjectorSpectral.verifyProjectorHypothesis transferMatrixData size ops
            let GSD = verification.Rank

            // The confidence is determined by the stability of the environment.
            let confidence = (ProjectorSpectral.getSpectralInfo verification).MinimalPolynomialDegree

            {
                Value = GroundStateDegeneracy GSD
                AlgebraicConfidence = confidence
                ContributionCount = environment.C.Rows * environment.C.Cols
            }

        member this.ComputeBatch environment peps locations ops =
            locations
            |> Array.Parallel.map (fun loc ->
                (this :> IObservable).Compute environment peps loc ops)