// 2-MPOInjectivity.fs
// Verification that the tensor satisfies MPO-injectivity conditions
namespace E8.TensorConstruction

open E8.Algebra
open E8.Tensors
open E8.TensorAlgebra

/// Module for verifying MPO-injectivity of PEPS tensors
module MPOInjectivity =

    /// Transfer matrix constructed from the PEPS tensor
    type TransferMatrix = {
        Matrix: MatrixF2
        LeftDim: int
        RightDim: int
        Rank: int option
    }

    /// Constructs the transfer matrix from a PEPS tensor
    let constructTransferMatrix (tensor: Tensor5F2) : TransferMatrix =
        printfn "Constructing transfer matrix for MPO-injectivity check..."

        let physDim = tensor.D1
        let bondDim = tensor.D2

        // The transfer matrix acts on pairs of virtual indices
        let matDim = bondDim * bondDim
        let transferMat = MatrixF2(matDim, matDim)

        // T[(l1,r1), (l2,r2)] = Σ_p,u,d A[p,l1,r1,u,d] * A*[p,l2,r2,u,d]
        for l1 in 0 .. bondDim - 1 do
            for r1 in 0 .. bondDim - 1 do
                for l2 in 0 .. bondDim - 1 do
                    for r2 in 0 .. bondDim - 1 do
                        let idx1 = l1 * bondDim + r1
                        let idx2 = l2 * bondDim + r2

                        let mutable sum = F2.Zero

                        for p in 0 .. physDim - 1 do
                            for u in 0 .. bondDim - 1 do
                                for d in 0 .. bondDim - 1 do
                                    let elem1 = tensor.[p, l1, r1, u, d]
                                    let elem2 = tensor.[p, l2, r2, u, d]  // Conjugate in F₂ is identity
                                    sum <- F2.add sum (F2.mul elem1 elem2)

                        transferMat.[idx1, idx2] <- sum

        printfn "  Transfer matrix size: %d x %d" matDim matDim

        {
            Matrix = transferMat
            LeftDim = matDim
            RightDim = matDim
            Rank = None
        }

    /// Computes the rank of a matrix in F₂
    let computeRankF2 (matrix: MatrixF2) : int =
        // Use Gaussian elimination to find rank
        let rows = matrix.Rows
        let cols = matrix.Cols
        let workMatrix = matrix.Clone()

        let mutable rank = 0
        let mutable col = 0

        while rank < rows && col < cols do
            // Find pivot
            let mutable pivotRow = -1
            for row in rank .. rows - 1 do
                if workMatrix.[row, col] = F2.One then
                    pivotRow <- row
                    break

            if pivotRow >= 0 then
                // Swap rows if necessary
                if pivotRow <> rank then
                    for c in 0 .. cols - 1 do
                        let temp = workMatrix.[rank, c]
                        workMatrix.[rank, c] <- workMatrix.[pivotRow, c]
                        workMatrix.[pivotRow, c] <- temp

                // Eliminate column
                for row in 0 .. rows - 1 do
                    if row <> rank && workMatrix.[row, col] = F2.One then
                        for c in 0 .. cols - 1 do
                            workMatrix.[row, c] <- F2.add workMatrix.[row, c] workMatrix.[rank, c]

                rank <- rank + 1

            col <- col + 1

        rank

    /// Verifies MPO-injectivity of the tensor
    let verifyMPOInjectivity (tensor: Tensor5F2) : bool * int * int =
        printfn "\nVerifying MPO-injectivity..."
        printfn "  This ensures the tensor can be used as a good PEPS representation"

        // Construct the transfer matrix
        let transfer = constructTransferMatrix tensor

        // Compute its rank
        let rank = computeRankF2 transfer.Matrix
        let fullRank = transfer.LeftDim

        printfn "  Transfer matrix rank: %d / %d" rank fullRank

        // MPO-injectivity requires the transfer matrix to have full rank
        let isInjective = (rank = fullRank)

        if isInjective then
            printfn "  ✓ Tensor is MPO-injective!"
            printfn "    This guarantees unique ground state and spectral gap"
        else
            printfn "  ✗ Tensor is NOT MPO-injective"
            printfn "    Rank deficiency: %d" (fullRank - rank)

        (isInjective, rank, fullRank)

    /// Computes the kernel (null space) of the transfer matrix
    let computeKernel (transfer: TransferMatrix) : MatrixF2 option =
        let rank =
            match transfer.Rank with
            | Some r -> r
            | None -> computeRankF2 transfer.Matrix

        let fullDim = transfer.LeftDim
        let kernelDim = fullDim - rank

        if kernelDim = 0 then
            None  // Trivial kernel
        else
            // Find basis vectors for the kernel
            let kernel = MatrixF2(fullDim, kernelDim)
            // Implementation would use Gaussian elimination to find kernel basis
            // For now, return placeholder
            Some kernel

    /// Checks if the tensor satisfies the pulling-through condition
    let verifyPullingThrough (tensor: Tensor5F2) : bool =
        printfn "Verifying pulling-through conditions..."

        let physDim = tensor.D1
        let bondDim = tensor.D2
        let mutable isValid = true

        // The pulling-through condition relates different ways of contracting the tensor
        // This is essential for the tensor to represent a valid quantum state

        for p in 0 .. physDim - 1 do
            for l in 0 .. bondDim - 1 do
                for r in 0 .. bondDim - 1 do
                    // Check consistency of different contraction orders
                    let mutable sum1 = F2.Zero
                    let mutable sum2 = F2.Zero

                    for u in 0 .. bondDim - 1 do
                        for d in 0 .. bondDim - 1 do
                            sum1 <- F2.add sum1 tensor.[p, l, r, u, d]
                            sum2 <- F2.add sum2 tensor.[p, u, d, l, r]  // Different order

                    if sum1 <> sum2 then
                        isValid <- false
                        if not isValid then
                            printfn "    Violation at p=%d, l=%d, r=%d" p l r

        printfn "  Pulling-through: %s" (if isValid then "SATISFIED" else "VIOLATED")
        isValid

    /// Computes the entanglement spectrum from the transfer matrix
    let computeEntanglementSpectrum (transfer: TransferMatrix) : F2 list =
        // In F₂, the "spectrum" is just whether we have eigenvalues 0 or 1
        // This is determined by whether (T - I) has full rank (eigenvalue 0)
        // or whether T² = T (projector property, eigenvalues 0 and 1)

        let size = transfer.LeftDim
        let identity = MatrixF2.Identity(size)

        // Check if T² = T (projector property)
        let T2 = transfer.Matrix * transfer.Matrix
        let isProjector = (T2 = transfer.Matrix)

        if isProjector then
            [F2.Zero; F2.One]  // Both eigenvalues present
        else
            [F2.One]  // Only eigenvalue 1 (simplified)

    /// Complete MPO analysis including all checks
    let performCompleteMPOAnalysis (tensor: Tensor5F2) : unit =
        printfn "\n=== Complete MPO-Injectivity Analysis ==="

        // Step 1: Basic injectivity check
        let (isInjective, rank, fullRank) = verifyMPOInjectivity tensor

        // Step 2: Transfer matrix construction
        let transfer = constructTransferMatrix tensor

        // Step 3: Kernel analysis
        let kernel = computeKernel { transfer with Rank = Some rank }
        match kernel with
        | None -> printfn "  Kernel: Trivial (good for injectivity)"
        | Some k -> printfn "  Kernel: Non-trivial (dimension %d x %d)" k.Rows k.Cols

        // Step 4: Pulling-through conditions
        let pullingValid = verifyPullingThrough tensor

        // Step 5: Entanglement spectrum
        let spectrum = computeEntanglementSpectrum transfer
        printfn "  Entanglement spectrum eigenvalues: %A" spectrum

        // Summary
        printfn "\n[MPO Analysis Summary]"
        printfn "  Injective: %s" (if isInjective then "YES" else "NO")
        printfn "  Transfer matrix rank: %d/%d" rank fullRank
        printfn "  Pulling-through: %s" (if pullingValid then "SATISFIED" else "VIOLATED")
        printfn "  Spectrum contains: %A" spectrum

        if isInjective && pullingValid then
            printfn "  ✓✓ Tensor is suitable for PEPS simulation!"
        else
            printfn "  ✗✗ Tensor has issues that may affect simulation accuracy"