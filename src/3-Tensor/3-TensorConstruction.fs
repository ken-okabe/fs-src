// 3-TensorConstruction.fs
// Integration module that combines tensor construction components
namespace E8.TensorConstruction

open System
open E8.Algebra
open E8.QuantumAlgebra.FibonacciFusion
open E8.QuantumAlgebra.FSymbols
open E8.TensorConstruction.FibonacciTensor

/// High-level interface for tensor construction
module TensorConstructionIntegration =

    /// Complete tensor construction with all verifications belonging to the construction layer
    let constructAndVerifyFibonacciPEPS() : FibonacciPEPS =
        printfn "\n" + String.replicate 70 "="
        printfn "   LAYER 3: FIBONACCI PEPS TENSOR CONSTRUCTION AND VERIFICATION"
        printfn String.replicate 70 "="

        // Phase 1: Verify mathematical foundations from Layer 2
        printfn "\n[PHASE 1] Mathematical Foundation Verification"
        printfn String.replicate 50 "-"

        let pentagonValid = FSymbols.verifyPentagonEquations()
        let hexagonValid = FSymbols.verifyHexagonEquations()

        if not (pentagonValid && hexagonValid) then
            failwith "Mathematical foundation (F-symbol) verification failed!"

        printfn "✓ All mathematical foundations from Layer 2 are consistent."

        // Phase 2: Construct the tensor from algebraic rules
        printfn "\n[PHASE 2] Tensor Construction from Algebraic Rules"
        printfn String.replicate 50 "-"

        // This internally calls createRawTensor which is the core of this layer
        let peps = FibonacciTensor.constructFibonacciTensor()

        // Phase 3: Symmetry enforcement and local verification
        printfn "\n[PHASE 3] Symmetry Enforcement and Plaquette Verification"
        printfn String.replicate 50 "-"

        SymmetryEnforcement.enforceD4Symmetry peps.Data
        SymmetryEnforcement.verifyAllSymmetries peps.Data

        let (plaquetteValid, plaquetteViolations) = FibonacciTensor.verifyPlaquetteConditions peps.Data

        // Update Metadata with the results of the verifications performed in this layer
        let updatedPEPS = {
            peps with
                Metadata = {
                    peps.Metadata with
                        SymmetryVerified = true // This is enforced and verified within this layer
                        PlaquetteVerified = plaquetteValid
                }
        }

        // Phase 4: Final summary for Layer 3
        printfn "\n" + String.replicate 70 "="
        printfn "                    CONSTRUCTION COMPLETE"
        printfn String.replicate 70 "="

        printfn "\n[Final Tensor Properties (Layer 3)]"
        printfn "  Physical dimension: %d" updatedPEPS.PhysicalDimension
        printfn "  Bond dimension: %d" updatedPEPS.BondDimension
        printfn "  Non-zero elements: %d/%d (%.2f%%)"
                updatedPEPS.Metadata.NonZeroElements
                updatedPEPS.Metadata.TotalElements
                (100.0 * float updatedPEPS.Metadata.NonZeroElements / float updatedPEPS.Metadata.TotalElements)
        printfn "  Symmetry verified: %s" (if updatedPEPS.Metadata.SymmetryVerified then "YES ✓" else "NO ✗")
        printfn "  Plaquette conditions: %s" (if updatedPEPS.Metadata.PlaquetteVerified then "SATISFIED ✓" else sprintf "VIOLATED (%d)" plaquetteViolations)

        updatedPEPS

    /// Quick construction without full verification (for testing)
    let constructQuickTensor() : FibonacciPEPS =
        // This function is intended for rapid testing and might skip some verification steps
        // For this complete version, we ensure it still constructs the core tensor correctly
        let physDim = 2
        let bondDim = 2
        let tensor = FibonacciTensor.createRawTensor physDim bondDim
        let metadata = {
            CreationTime = DateTime.UtcNow
            ConstructionMethod = "Quick F-symbol based"
            PhysicalDimension = physDim
            BondDimension = bondDim
            NonZeroElements = tensor.CountNonZero()
            TotalElements = tensor.TotalElements
            SymmetryVerified = false
            PlaquetteVerified = false
            PentagonVerified = false
            HexagonVerified = false
            MemorySavingsRatio = 1.0 - 1.0/8.0
        }
        {
            Data = tensor
            PhysicalDimension = physDim
            BondDimension = bondDim
            Metadata = metadata
        }