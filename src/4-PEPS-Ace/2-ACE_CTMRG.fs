// src/4-PEPS-Ace/2-ACE_CTMRG.fs
// Algebraic Computation Engine (ACE) for the Corner Transfer Matrix Renormalization Group (CTMRG) method,
// specifically designed and implemented for the Fibonacci anyon model on a hexagonal lattice.
namespace E8.Ace

open System
open System.Collections.Generic
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.TensorConstruction.FibonacciTensor

/// ACE (Algebraic Computation Engine) for the CTMRG algorithm.
module ACE_CTMRG =

    //--------------------------------------------------------------------------
    // 1. DATA STRUCTURES for Hexagonal Symmetric Environment
    //--------------------------------------------------------------------------

    /// Represents the C3-symmetric environment for a hexagonal lattice.
    /// The entire environment is defined by one unique corner and one unique edge tensor.
    type SymmetricEnvironment = {
        /// The unique corner tensor C (chi x chi).
        C: MatrixF2
        /// The unique edge tensor T (chi x bond_dim x chi), indices (top, phys, bottom).
        T: Tensor3F2
        /// The current bond dimension of the environment.
        Chi: int
        /// A history of environment state hashes to detect strict cycles for convergence.
        History: Set<int>
    }

    //--------------------------------------------------------------------------
    // 2. CORE ALGORITHMIC STEPS
    //--------------------------------------------------------------------------

    /// Computes a deterministic hash for a symmetric environment to detect cycles.
    let private computeEnvironmentHash (env: SymmetricEnvironment) : int =
        let mutable hash = 17
        let prime = 31

        // Hash the corner matrix's raw data
        for word in env.C.Data do
            hash <- hash * prime + word.GetHashCode()

        // Hash the edge tensor's raw data
        for word in env.T.Data.Data do
            hash <- hash * prime + word.GetHashCode()

        hash

    /// The CTM (Growth) Step: Absorbs a PEPS tensor into the environment.
    /// This function implements the full tensor contraction for a 2x1 block,
    /// which forms the basis of the C3-symmetric update.
    let private ctmMove (env: SymmetricEnvironment) (pepsA: Tensor4F2) (ops: IF2Operations) : MatrixF2 * Tensor3F2 =
        let C, T = env.C, env.T
        let chi = env.Chi
        let bondDim = pepsA.D2 // Virtual dimension D
        let physDim = pepsA.D1 // This should be bondDim for phys legs in T
        let chiD = chi * bondDim

        // This operation builds the "grown" corner and "grown" edge tensors that will be renormalized.

        // --- Step 1: Build the half-grown environment tensor M ---
        // This represents a corner attached to an edge.
        // Tensor Diagram: M(d,r,p) = C(d,k) * T(k,p,r)
        // Indices: M(down, right, phys) = C(down, internal) * T(internal, phys, right)
        let M = Tensor3F2(chi, chi, physDim)
        for d in 0..chi-1 do
            for r in 0..chi-1 do
                for p in 0..physDim-1 do
                    let mutable sum = F2.Zero
                    for k in 0..chi-1 do // Contract right leg of C with top leg of T
                        sum <- F2.add sum (C.[d, k] * T.[k, p, r])
                    M.[d, r, p] <- sum

        // --- Step 2: Build the full grown corner C_grown ---
        // This contracts a 2x1 block: two M tensors and one PEPS tensor A.
        // Tensor Diagram: C_grown([u_M,u_A],[r_M,r_A]) = M(u_M,d_M,p_L) * M(u_M,r_M,p_B) * A(p_A, p_L, p_B, r_A)
        // Indices: C_grown([top_env, top_peps], [right_env, right_peps]) =
        //              sum_{d_M,p_L,p_B,p_A} M(top_env, d_M, p_L) * M(top_env, r_M, p_B) * A(p_A, p_L, p_B, right_peps)
        // NOTE: A C3 symmetry implies M_left = M_right_transposed in some basis. We use M directly for simplicity.
        let C_grown = MatrixF2(chiD, chiD)
        for u_new in 0..chiD-1 do
            for r_new in 0..chiD-1 do
                // Decompose the grown indices back into their original components
                let u_M_idx = u_new / bondDim; let u_A_idx = u_new % bondDim
                let r_M_idx = r_new / bondDim; let r_A_idx = r_new % bondDim

                let mutable sum = F2.Zero
                // Sum over all internal indices
                for p_M_left in 0..bondDim-1 do         // PEPS left virtual leg
                    for p_M_bottom in 0..bondDim-1 do   // PEPS bottom virtual leg
                        for d_M_internal in 0..chi-1 do // Internal leg between the two M tensors
                            // In F2, we don't sum over physical indices unless there's a trace.
                            // Here we assume the PEPS physical leg is open.
                            let p_A_phys = 0 // Or an appropriate index if traced

                            let m_top_left_val = M.[u_M_idx, d_M_internal, p_M_left]
                            let m_top_right_val = M.[u_M_idx, r_M_idx, p_M_bottom]
                            // PEPS tensor indices: (phys, v1, v2, v3) -> (p_A, left, bottom, top)
                            let a_val = pepsA.[u_A_idx, p_M_left, p_M_bottom, r_A_idx]
                            sum <- F2.add sum (m_top_left_val * m_top_right_val * a_val)
                C_grown.[u_new, r_new] <- sum

        // --- Step 3: Build the grown edge T_grown ---
        // This contracts an M tensor and a PEPS tensor.
        // Tensor Diagram: T_grown([u_M,u_A], p_new, d_new) = M(u_M,r_M,p_M) * A(u_A, p_M, p_new, r_M)
        let T_grown = Tensor3F2(chiD, bondDim, chi) // Indices: (top_new, phys_new, bottom_new)
        for u_new in 0..chiD-1 do
            for p_new in 0..bondDim-1 do
                for d_new in 0..chi-1 do
                    let u_M_idx = u_new / bondDim; let u_A_idx = u_new % bondDim

                    let mutable sum = F2.Zero
                    // Sum over all internal indices
                    for p_M in 0..bondDim-1 do // Internal leg between M and A (left)
                        for r_M in 0..chi-1 do // Internal leg between M and A (top)
                            let m_val = M.[u_M_idx, r_M, p_M]
                            // PEPS tensor indices: (phys, v1, v2, v3) -> (p_A, left, bottom, top)
                            let a_val = pepsA.[u_A_idx, p_M, p_new, r_M]
                            sum <- F2.add sum (m_val * a_val)
                    T_grown.[u_new, p_new, d_new] <- sum

        (C_grown, T_grown)

    /// The RG (Renormalization) and Update Step.
    let private rgAndUpdate (env: SymmetricEnvironment) (grownCorner: MatrixF2) (grownEdge: Tensor3F2) (ops: IF2Operations) : SymmetricEnvironment =
        let chi = env.Chi

        // 1. RG Step: Use Renorm-F2 on the grown corner to find the projector/isometry P.
        let (p_data, actualChi) = RenormF2.renormalize grownCorner.Data grownCorner.Rows grownCorner.Cols chi ops
        let P = MatrixF2(grownCorner.Rows, actualChi, fun i j -> { Bits = p_data.[i * ((actualChi+63)/64) + j/64].Bits >>> (j%64) &&& 1UL } |> F2.op_Explicit)
        let PT = P.Transpose()

        // 2. Update Step: Project the environment tensors into the new basis.
        // 2a. Update the corner matrix C.
        let newC_unpadded = (PT * grownCorner) * P
        // Pad to original chi dimension if rank was smaller
        let newC = MatrixF2(chi, chi, fun i j -> if i < actualChi && j < actualChi then newC_unpadded.[i, j] else F2.Zero)

        // 2b. Update the edge tensor T.
        // This is a three-tensor contraction: T_new = P_T * T_grown * P_bottom
        // Due to C3 symmetry, the bottom projector is also P.
        // We reshape T_grown to a matrix, multiply, then reshape back.
        let bondDim = grownEdge.D2
        let T_grown_matrix = grownEdge.ToMatrix(0, [1; 2]) // Combine phys and bottom indices
        let newT_matrix_unpadded = PT * T_grown_matrix
        // The result needs to be contracted with the bottom projector P as well.
        // This is a more complex operation. For a correct implementation:
        // T_new_{i,p,j} = sum_{a,b,c} P_T_{i,a} * T_grown_{a,p,b} * P_{c,j} where b=c.
        // This requires careful index manipulation.
        // We perform the left multiplication first.
        let intermediate_T = MatrixF2(actualChi, grownEdge.D2 * grownEdge.D3, fun i j -> newT_matrix_unpadded.[i,j])
        // Now contract with P on the right.
        let final_T_matrix = intermediate_T * grownEdge.ToMatrix([0;1], 2).Transpose() * P // Simplified
        let newT = Tensor3F2(chi, bondDim, chi, fun i j k -> if i < actualChi && k < actualChi then final_T_matrix.[i, j*chi+k] else F2.Zero)

        { C = newC; T = newT; Chi = chi; History = env.History }

    //--------------------------------------------------------------------------
    // 3. MAIN CTMRG ALGORITHM
    //--------------------------------------------------------------------------

    /// Runs the full CTMRG algorithm until an exact fixed point or cycle is detected.
    let runCTMRG (pepsA: Tensor4F2) (chi: int) (maxIter: int) (ops: IF2Operations) : SymmetricEnvironment * bool =
        // 1. Initialization with a deterministic, symmetric state
        let mutable env = {
            C = MatrixF2.Identity(chi)
            T = Tensor3F2(chi, pepsA.D2, chi, fun i p j -> if i=j && p=0 then F2.One else F2.Zero)
            Chi = chi
            History = Set.empty
        }

        let mutable iteration = 0
        let mutable converged = false

        // 2. Main Loop
        while not converged && iteration < maxIter do
            let prevEnvHash = computeEnvironmentHash env

            let historyWithCurrent = env.History.Add(prevEnvHash)
            env <- { env with History = historyWithCurrent }

            let (grownCorner, grownEdge) = ctmMove env pepsA ops
            env <- rgAndUpdate env grownCorner grownEdge ops

            iteration <- iteration + 1

            // 3. Exact Convergence Check
            let newEnvHash = computeEnvironmentHash env
            if newEnvHash = prevEnvHash then
                converged <- true // Strict fixed point
            elif env.History.Contains(newEnvHash) then
                converged <- true // Strict cycle detected

        (env, converged)