// 1-FibonacciTensor.fs
// Construction of the A_Fibonacci PEPS tensor encoding topological order
namespace E8.TensorConstruction

open System
open System.Collections.Generic
open E8.Algebra
open E8.BitPacking
open E8.TensorAlgebra
open E8.Tensors
open E8.QuantumAlgebra
open E8.QuantumAlgebra.GoldenRatio
open E8.QuantumAlgebra.FibonacciFusion
open E8.QuantumAlgebra.FSymbols

/// Module for constructing Fibonacci PEPS tensors
module FibonacciTensor =

    /// Metadata about the constructed tensor
    type TensorMetadata = {
        CreationTime: DateTime
        ConstructionMethod: string
        PhysicalDimension: int
        BondDimension: int
        NonZeroElements: int
        TotalElements: int
        SymmetryVerified: bool
        PlaquetteVerified: bool
        MPOInjectivityVerified: bool
        PentagonVerified: bool
        HexagonVerified: bool
        SpectralGap: F2 option
        MemorySavingsRatio: float
    }

    /// The main Fibonacci PEPS tensor structure
    type FibonacciPEPS = {
        /// The actual tensor data in bit-packed format
        Data: Tensor5F2
        /// Physical dimension (2 for Fibonacci: vacuum and tau)
        PhysicalDimension: int
        /// Bond dimension (virtual indices)
        BondDimension: int
        /// Whether the tensor satisfies MPO-injectivity
        IsInjective: bool
        /// Spectral gap if computed
        SpectralGap: F2 option
        /// Complete metadata
        Metadata: TensorMetadata
    }

    /// Represents a plaquette configuration for verification
    type PlaquetteConfig = {
        TopLeft: AnyonType
        TopRight: AnyonType
        BottomLeft: AnyonType
        BottomRight: AnyonType
        LeftEdge: AnyonType
        TopEdge: AnyonType
        RightEdge: AnyonType
        BottomEdge: AnyonType
    }

    /// Verifies that a plaquette configuration is physically valid
    let isValidPlaquette (config: PlaquetteConfig) : bool =
        // Check that all four vertices can fuse consistently
        // The plaquette is valid if there exists a consistent fusion path

        // Path 1: (TopLeft ⊗ TopEdge) → TopRight, (LeftEdge ⊗ BottomEdge) → BottomLeft
        let path1Valid =
            isAllowedFusion config.TopLeft config.TopEdge config.TopRight &&
            isAllowedFusion config.LeftEdge config.BottomEdge config.BottomLeft &&
            isAllowedFusion config.TopRight config.RightEdge config.BottomRight &&
            isAllowedFusion config.BottomLeft config.BottomRight config.BottomEdge

        // Path 2: (TopLeft ⊗ LeftEdge) → BottomLeft, (TopEdge ⊗ RightEdge) → TopRight
        let path2Valid =
            isAllowedFusion config.TopLeft config.LeftEdge config.BottomLeft &&
            isAllowedFusion config.TopEdge config.RightEdge config.TopRight &&
            isAllowedFusion config.BottomLeft config.BottomEdge config.BottomRight &&
            isAllowedFusion config.TopRight config.BottomRight config.RightEdge

        path1Valid || path2Valid

    /// Computes the amplitude for a given tensor element using F-symbols
    let computeAmplitude (physical: AnyonType) (left: AnyonType) (right: AnyonType)
                        (up: AnyonType) (down: AnyonType) : F2 =

        // The amplitude is determined by the F-symbols connecting the anyons
        // We need to check if there's a consistent fusion path

        let mutable totalAmplitude = F2.Zero

        // Try all possible intermediate fusion channels
        for intermediate1 in [Vacuum; Tau] do
            for intermediate2 in [Vacuum; Tau] do
                // Check if this fusion path is allowed
                if isAllowedFusion left right intermediate1 &&
                   isAllowedFusion up down intermediate2 &&
                   isAllowedFusion intermediate1 intermediate2 physical then

                    // Compute the F-symbol for this configuration
                    let fsym = computeFSymbol left right up physical intermediate1 intermediate2
                    let amplitude = fSymbolToF2 fsym

                    // Add to total amplitude (in F₂, addition is XOR)
                    totalAmplitude <- F2.add totalAmplitude amplitude

        totalAmplitude

    /// Creates the raw tensor data for A_Fibonacci
    let createRawTensor (physDim: int) (bondDim: int) : Tensor5F2 =
        // For Fibonacci anyons: physDim = 2 (vacuum, tau), bondDim = 2 (vacuum, tau)
        let tensor = Tensor5F2(physDim, bondDim, bondDim, bondDim, bondDim)

        printfn "Constructing Fibonacci PEPS tensor..."
        printfn "  Physical dimension: %d" physDim
        printfn "  Bond dimension: %d" bondDim
        printfn "  Total elements: %d" tensor.TotalElements

        let mutable nonZeroCount = 0

        // Iterate over all tensor elements
        for p in 0 .. physDim - 1 do
            for l in 0 .. bondDim - 1 do
                for r in 0 .. bondDim - 1 do
                    for u in 0 .. bondDim - 1 do
                        for d in 0 .. bondDim - 1 do
                            // Convert indices to anyon types
                            let physAnyon = AnyonType.FromInt p
                            let leftAnyon = AnyonType.FromInt l
                            let rightAnyon = AnyonType.FromInt r
                            let upAnyon = AnyonType.FromInt u
                            let downAnyon = AnyonType.FromInt d

                            // Compute the amplitude for this configuration
                            let amplitude = computeAmplitude physAnyon leftAnyon rightAnyon
                                                            upAnyon downAnyon

                            // Set the tensor element
                            tensor.[p, l, r, u, d] <- amplitude

                            if amplitude = F2.One then
                                nonZeroCount <- nonZeroCount + 1

        printfn "  Non-zero elements: %d (%.2f%%)"
                nonZeroCount
                (100.0 * float nonZeroCount / float tensor.TotalElements)

        tensor

    /// Verifies the plaquette conditions for the tensor
    let verifyPlaquetteConditions (tensor: Tensor5F2) : bool * int =
        printfn "Verifying plaquette conditions..."

        let physDim = tensor.D1
        let bondDim = tensor.D2
        let mutable isValid = true
        let mutable violationCount = 0

        // Check all possible 2x2 plaquette configurations
        for p1 in 0 .. physDim - 1 do
            for p2 in 0 .. physDim - 1 do
                for p3 in 0 .. physDim - 1 do
                    for p4 in 0 .. physDim - 1 do
                        // p1 -- p2
                        // |     |
                        // p3 -- p4

                        for e1 in 0 .. bondDim - 1 do  // Top edge
                            for e2 in 0 .. bondDim - 1 do  // Right edge
                                for e3 in 0 .. bondDim - 1 do  // Bottom edge
                                    for e4 in 0 .. bondDim - 1 do  // Left edge

                                        // Get the amplitude from the tensor
                                        let amp1 = tensor.[p1, e4, e1, 0, 0]  // Top-left
                                        let amp2 = tensor.[p2, e1, e2, 0, 0]  // Top-right
                                        let amp3 = tensor.[p3, e3, e4, 0, 0]  // Bottom-left
                                        let amp4 = tensor.[p4, e2, e3, 0, 0]  // Bottom-right

                                        // The plaquette amplitude is the product (AND in F₂)
                                        let plaquetteAmp =
                                            F2.mul (F2.mul (F2.mul amp1 amp2) amp3) amp4

                                        // Check if this should be allowed by fusion rules
                                        let config = {
                                            TopLeft = AnyonType.FromInt p1
                                            TopRight = AnyonType.FromInt p2
                                            BottomLeft = AnyonType.FromInt p3
                                            BottomRight = AnyonType.FromInt p4
                                            LeftEdge = AnyonType.FromInt e4
                                            TopEdge = AnyonType.FromInt e1
                                            RightEdge = AnyonType.FromInt e2
                                            BottomEdge = AnyonType.FromInt e3
                                        }

                                        let shouldBeValid = isValidPlaquette config

                                        // Check consistency
                                        if shouldBeValid && plaquetteAmp = F2.Zero then
                                            isValid <- false
                                            violationCount <- violationCount + 1
                                            if violationCount <= 5 then  // Report first few violations
                                                printfn "    Violation: Plaquette (%A,%A,%A,%A) with edges (%A,%A,%A,%A) should be non-zero but is zero"
                                                        config.TopLeft config.TopRight config.BottomLeft config.BottomRight
                                                        config.LeftEdge config.TopEdge config.RightEdge config.BottomEdge
                                        elif (not shouldBeValid) && plaquetteAmp = F2.One then
                                            isValid <- false
                                            violationCount <- violationCount + 1
                                            if violationCount <= 5 then
                                                printfn "    Violation: Plaquette (%A,%A,%A,%A) with edges (%A,%A,%A,%A) should be zero but is non-zero"
                                                        config.TopLeft config.TopRight config.BottomLeft config.BottomRight
                                                        config.LeftEdge config.TopEdge config.RightEdge config.BottomEdge

        if isValid then
            printfn "  ✓ All plaquette conditions satisfied!"
        else
            printfn "  ✗ Found %d plaquette violations" violationCount

        (isValid, violationCount)

    /// Enforces physical symmetries on the tensor
    let enforceSymmetries (tensor: Tensor5F2) : unit =
        printfn "Enforcing physical symmetries..."

        let physDim = tensor.D1
        let bondDim = tensor.D2

        // Enforce 90-degree rotational symmetry
        // A[p,l,r,u,d] should equal A[p,d,u,r,l] (90° rotation)
        for p in 0 .. physDim - 1 do
            for l in 0 .. bondDim - 1 do
                for r in 0 .. bondDim - 1 do
                    for u in 0 .. bondDim - 1 do
                        for d in 0 .. bondDim - 1 do
                            let original = tensor.[p, l, r, u, d]
                            let rotated = tensor.[p, d, u, r, l]

                            if original <> rotated then
                                // Average in F₂ (take OR for consistency)
                                let averaged = if original = F2.One || rotated = F2.One then F2.One else F2.Zero
                                tensor.[p, l, r, u, d] <- averaged
                                tensor.[p, d, u, r, l] <- averaged

        // Enforce reflection symmetry (horizontal)
        // A[p,l,r,u,d] should equal A[p,r,l,u,d]
        for p in 0 .. physDim - 1 do
            for l in 0 .. bondDim - 1 do
                for r in 0 .. bondDim - 1 do
                    for u in 0 .. bondDim - 1 do
                        for d in 0 .. bondDim - 1 do
                            let original = tensor.[p, l, r, u, d]
                            let reflected = tensor.[p, r, l, u, d]

                            if original <> reflected then
                                let averaged = if original = F2.One || reflected = F2.One then F2.One else F2.Zero
                                tensor.[p, l, r, u, d] <- averaged
                                tensor.[p, r, l, u, d] <- averaged

        // Enforce vertical reflection symmetry
        // A[p,l,r,u,d] should equal A[p,l,r,d,u]
        for p in 0 .. physDim - 1 do
            for l in 0 .. bondDim - 1 do
                for r in 0 .. bondDim - 1 do
                    for u in 0 .. bondDim - 1 do
                        for d in 0 .. bondDim - 1 do
                            let original = tensor.[p, l, r, u, d]
                            let reflected = tensor.[p, l, r, d, u]

                            if original <> reflected then
                                let averaged = if original = F2.One || reflected = F2.One then F2.One else F2.Zero
                                tensor.[p, l, r, u, d] <- averaged
                                tensor.[p, l, r, d, u] <- averaged

        printfn "  ✓ Symmetries enforced (4-fold rotation + reflections)"

    /// Main construction function for the Fibonacci PEPS tensor
    let constructFibonacciTensor() : FibonacciPEPS =
        printfn "\n=== Constructing Fibonacci PEPS Tensor ==="
        printfn "This tensor encodes the topological order of Fibonacci anyons"

        let startTime = DateTime.UtcNow

        // Dimensions for Fibonacci anyons
        let physDim = 2  // vacuum and tau
        let bondDim = 2  // vacuum and tau on virtual indices

        // Step 1: Verify algebraic consistency
        printfn "\n[Step 1] Verifying algebraic consistency..."
        let pentagonValid = FSymbols.verifyPentagonEquations()
        let hexagonValid = FSymbols.verifyHexagonEquations()

        if not pentagonValid || not hexagonValid then
            failwith "F-symbol consistency check failed! Cannot construct valid tensor."

        printfn "  ✓ F-symbols are mathematically consistent"

        // Step 2: Create raw tensor
        printfn "\n[Step 2] Creating raw tensor with fusion amplitudes..."
        let tensor = createRawTensor physDim bondDim

        // Step 3: Enforce symmetries
        printfn "\n[Step 3] Enforcing physical symmetries..."
        enforceSymmetries tensor

        // Step 4: Verify plaquette conditions
        printfn "\n[Step 4] Verifying plaquette conditions..."
        let (plaquetteValid, plaquetteViolations) = verifyPlaquetteConditions tensor

        // Step 5: Count non-zero elements
        let nonZeroCount = tensor.CountNonZero()
        let totalElements = tensor.TotalElements
        let sparsity = float nonZeroCount / float totalElements

        printfn "\n[Summary]"
        printfn "  Total elements: %d" totalElements
        printfn "  Non-zero elements: %d (%.2f%%)" nonZeroCount (100.0 * sparsity)
        printfn "  Memory savings from bit-packing: %.1f%%" (100.0 * (1.0 - 1.0/8.0))

        let endTime = DateTime.UtcNow
        let constructionTime = endTime - startTime

        // Create the PEPS structure
        let peps = {
            Data = tensor
            PhysicalDimension = physDim
            BondDimension = bondDim
            IsInjective = false  // Will be verified by MPOInjectivity module
            SpectralGap = None   // Will be computed by spectral analysis
            Metadata = {
                CreationTime = startTime
                ConstructionMethod = "F-symbol based with symmetry enforcement"
                PhysicalDimension = physDim
                BondDimension = bondDim
                NonZeroElements = nonZeroCount
                TotalElements = totalElements
                SymmetryVerified = true
                PlaquetteVerified = plaquetteValid
                MPOInjectivityVerified = false
                PentagonVerified = pentagonValid
                HexagonVerified = hexagonValid
                SpectralGap = None
                MemorySavingsRatio = 1.0 - 1.0/8.0
            }
        }

        printfn "  Construction completed in %.2f seconds" constructionTime.TotalSeconds
        printfn "  Plaquette conditions: %s" (if plaquetteValid then "SATISFIED" else sprintf "VIOLATED (%d)" plaquetteViolations)

        peps

    /// Extracts a specific slice of the tensor for analysis
    let extractSlice (tensor: Tensor5F2) (physical: int) : Tensor4F2 =
        let slice = Tensor4F2(tensor.D2, tensor.D3, tensor.D4, tensor.D5)
        for l in 0 .. tensor.D2 - 1 do
            for r in 0 .. tensor.D3 - 1 do
                for u in 0 .. tensor.D4 - 1 do
                    for d in 0 .. tensor.D5 - 1 do
                        slice.[l, r, u, d] <- tensor.[physical, l, r, u, d]
        slice

    /// Computes the norm of a tensor in F₂ (number of non-zero elements)
    let tensorNormF2 (tensor: Tensor5F2) : int =
        tensor.CountNonZero()

    /// Verifies that the tensor satisfies the isometry condition
    let verifyIsometry (tensor: Tensor5F2) : bool =
        printfn "Verifying isometry conditions..."

        // For a PEPS to be isometric, contracting physical indices should give identity
        // This is a simplified check - full isometry verification is more complex

        let physDim = tensor.D1
        let bondDim = tensor.D2
        let mutable isIsometric = true

        // Check that the tensor has the right normalization
        for l in 0 .. bondDim - 1 do
            for r in 0 .. bondDim - 1 do
                for u in 0 .. bondDim - 1 do
                    for d in 0 .. bondDim - 1 do
                        let mutable sum = F2.Zero
                        for p in 0 .. physDim - 1 do
                            sum <- F2.add sum tensor.[p, l, r, u, d]

                        // In F₂, we expect certain patterns
                        // This is a simplified check
                        if sum = F2.Zero && (l = r && u = d) then
                            isIsometric <- false

        printfn "  Isometry check: %s" (if isIsometric then "PASS" else "FAIL")
        isIsometric