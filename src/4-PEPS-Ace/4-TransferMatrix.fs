// src/4-PEPS-Ace/4-TransferMatrix.fs
// Advanced analysis and operations for Transfer Matrices derived from hexagonal lattice environments.
namespace E8.Ace

open System
open System.Collections.Generic
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.Ace.ACE_CTMRG // Importing the SymmetricEnvironment definition

/// Module for advanced analysis and manipulation of Transfer Matrices.
module TransferMatrix =

    /// Represents the detailed algebraic and combinatorial structure of a transfer matrix.
    type TransferMatrixStructure = {
        /// The raw matrix data as a bit-packed array.
        Data: BitBlock64[]
        /// The size of the matrix (assumed to be square).
        Size: int
        /// Analysis of the block structure, if any.
        BlockStructure: BlockDecomposition option
        /// Detailed information about the matrix's sparsity pattern.
        SparsityPattern: SparsityInfo
        /// Information about the symmetries the matrix possesses.
        Symmetries: SymmetryInfo
    }

    and BlockDecomposition = {
        /// The number of identified blocks.
        NumBlocks: int
        /// An array containing the size of each block.
        BlockSizes: int[]
        /// Couplings between different blocks.
        Couplings: (int * int)[]
    }

    and SparsityInfo = {
        /// The total count of non-zero elements.
        NonZeroCount: int
        /// The ratio of zero elements to total elements (0.0 to 1.0).
        SparsityRatio: float
        /// An array holding the non-zero count for each row.
        RowNonZeros: int[]
        /// An array holding the non-zero count for each column.
        ColNonZeros: int[]
    }

    and SymmetryInfo = {
        /// True if the matrix is symmetric (T = T^T).
        IsSymmetric: bool
        /// True if the matrix is anti-symmetric (T = -T).
        IsAntiSymmetric: bool
        /// True if the matrix has a block-diagonal structure.
        IsBlockDiagonal: bool
        /// True if the matrix is circulant.
        IsCirculant: bool
    }

    /// Constructs the "grown" corner transfer matrix from a C3-symmetric hexagonal environment.
    /// This function implements the core CTM (growth) step contraction logic.
    let constructTransferMatrix (env: SymmetricEnvironment) (pepsA: Tensor4F2) (ops: IF2Operations) : BitBlock64[] * int =
        let C, T = env.C, env.T
        let chi = env.Chi
        let bondDim = pepsA.D2 // Virtual dimension D
        let physDim = T.D2     // Physical dimension of the edge tensor
        let chiD = chi * bondDim

        // --- Step 1: Build the half-grown environment tensor M ---
        // This represents a corner attached to an edge.
        // Tensor Diagram: M(d,r,p) = C(d,k) * T(k,p,r)
        // Indices: M(down, right, phys) = C(down_C, internal_k) * T(internal_k, phys_p, right_T)
        let M = Tensor3F2(chi, chi, physDim)
        for d in 0..chi-1 do
            for r in 0..chi-1 do
                for p in 0..physDim-1 do
                    let mutable sum = F2.Zero
                    for k in 0..chi-1 do // Contract right leg of C with top leg of T
                        sum <- F2.add sum (C.[d, k] * T.[k, p, r])
                    M.[d, r, p] <- sum

        // --- Step 2: Build the full grown corner C_grown, which is our transfer matrix ---
        // This contracts a 2x1 rectangular block: two M tensors and one PEPS tensor A.
        // Tensor Diagram: C_grown([u_M,u_A],[r_M,r_A]) = sum_{...} M(...) * M(...) * A(...)
        // Index Mapping:
        // C_grown_row = u_new = u_M * D + u_A
        // C_grown_col = r_new = r_M * D + r_A
        // Internal indices to be summed over: p_left, p_bottom (from M's phys legs), d_internal (between M's)
        let C_grown_matrix = MatrixF2(chiD, chiD)
        for u_new in 0..chiD-1 do
            for r_new in 0..chiD-1 do
                let u_M_idx = u_new / bondDim; let u_A_idx = u_new % bondDim
                let r_M_idx = r_new / bondDim; let r_A_idx = r_new % bondDim

                let mutable sum = F2.Zero
                for p_left in 0..bondDim-1 do     // PEPS left virtual leg
                    for p_bottom in 0..bondDim-1 do // PEPS bottom virtual leg
                        for d_internal in 0..chi-1 do // Internal leg between the two M tensors
                            // The physical leg of the PEPS tensor is assumed to be open (not traced over).
                            // We choose a specific physical state (e.g., 0) for the PEPS tensor.
                            let phys_A = 0

                            let m_top_left = M.[u_M_idx, d_internal, p_left]
                            let m_top_right = M.[u_M_idx, r_M_idx, p_bottom]
                            // PEPS tensor indices: (phys, v1, v2, v3) -> (phys_A, left, bottom, top)
                            let a_val = pepsA.[u_A_idx, p_left, p_bottom, r_A_idx]

                            sum <- F2.add sum (m_top_left * m_top_right * a_val)
                C_grown_matrix.[u_new, r_new] <- sum

        (C_grown_matrix.Data, chiD)

    /// Analyzes the structure of a given transfer matrix.
    let analyzeStructure (matrix: BitBlock64[]) (size: int) (ops: IF2Operations) =
        let wordsPerRow = (size + 63) / 64
        let mutable nonZeroCount = 0
        let rowNonZeros = Array.zeroCreate size
        let colNonZeros = Array.zeroCreate size

        for i in 0 .. size - 1 do
            for j in 0 .. size - 1 do
                let wordIdx = i * wordsPerRow + j / 64
                let bitIdx = j % 64
                if wordIdx < matrix.Length then
                    if (matrix.[wordIdx].Bits >>> bitIdx) &&& 1UL = 1UL then
                        nonZeroCount <- nonZeroCount + 1
                        rowNonZeros.[i] <- rowNonZeros.[i] + 1
                        colNonZeros.[j] <- colNonZeros.[j] + 1

        let sparsity = {
            NonZeroCount = nonZeroCount
            SparsityRatio = 1.0 - (float nonZeroCount / float (size * size))
            RowNonZeros = rowNonZeros
            ColNonZeros = colNonZeros
        }

        let checkSymmetric() =
            let mutable isSymmetric = true
            for i in 0 .. size - 1 do
                for j in i + 1 .. size - 1 do
                    let ij_wordIdx = i * wordsPerRow + j / 64
                    let ij_bitIdx = j % 64
                    let ji_wordIdx = j * wordsPerRow + i / 64
                    let ji_bitIdx = i % 64

                    if ij_wordIdx < matrix.Length && ji_wordIdx < matrix.Length then
                        let ij_bit = (matrix.[ij_wordIdx].Bits >>> ij_bitIdx) &&& 1UL
                        let ji_bit = (matrix.[ji_wordIdx].Bits >>> ji_bitIdx) &&& 1UL
                        if ij_bit <> ji_bit then
                            isSymmetric <- false
            isSymmetric

        let symmetry = {
            IsSymmetric = checkSymmetric()
            IsAntiSymmetric = false
            IsBlockDiagonal = false
            IsCirculant = false
        }

        {
            Data = matrix
            Size = size
            BlockStructure = None
            SparsityPattern = sparsity
            Symmetries = symmetry
        }

    /// Computes the power of a transfer matrix using exponentiation by squaring.
    let matrixPower (matrix: BitBlock64[]) (size: int) (power: int) (ops: IF2Operations) =
        if power < 0 then
            raise (System.ArgumentException("Power must be a non-negative integer."))
        elif power = 0 then
            let wordsPerRow = (size + 63) / 64
            let identity = Array.zeroCreate (size * wordsPerRow)
            for i in 0 .. size - 1 do
                let wordIdx = i * wordsPerRow + i / 64
                let bitIdx = i % 64
                identity.[wordIdx] <- { Bits = identity.[wordIdx].Bits ||| (1UL <<< bitIdx) }
            identity
        elif power = 1 then
            Array.copy matrix
        else
            let rec powerRec (m: BitBlock64[]) (p: int) =
                if p = 1 then m
                elif p % 2 = 0 then
                    let half = powerRec m (p / 2)
                    ops.MatrixMultiply half half size size size
                else
                    let m' = powerRec m (p - 1)
                    ops.MatrixMultiply m m' size size size
            powerRec matrix power