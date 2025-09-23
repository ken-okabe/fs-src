// 4-TensorConstruction.fs
// Integration module that combines all tensor construction components
namespace E8.TensorConstruction

open System
open E8.Algebra
open E8.QuantumAlgebra
open E8.QuantumAlgebra.GoldenRatio
open E8.QuantumAlgebra.FibonacciFusion
open E8.QuantumAlgebra.FSymbols

/// High-level interface for tensor construction
module TensorConstructionIntegration =

    /// Complete tensor construction with all verifications
    let constructAndVerifyFibonacciPEPS() : FibonacciTensor.FibonacciPEPS =
        printfn "\n" + String.replicate 70 "="
        printfn "   COMPLETE FIBONACCI PEPS TENSOR CONSTRUCTION AND VERIFICATION"
        printfn String.replicate 70 "="

        // Phase 1: Verify mathematical foundations
        printfn "\n[PHASE 1] Mathematical Foundation Verification"
        printfn String.replicate 50 "-"

        let goldenRatioValid = GoldenRatio.runComprehensiveTests()
        let fusionRulesValid = FibonacciFusion.verifyFusionRules()
        let fSymbolsValid = FSymbols.verifyFullConsistency()

        if not (goldenRatioValid && fusionRulesValid && fSymbolsValid) then
            failwith "Mathematical foundation verification failed!"

        printfn "✓ All mathematical foundations verified"

        // Phase 2: Construct the tensor
        printfn "\n[PHASE 2] Tensor Construction"
        printfn String.replicate 50 "-"

        let peps = FibonacciTensor.constructFibonacciTensor()

        // Phase 3: Symmetry enforcement
        printfn "\n[PHASE 3] Symmetry Enforcement and Verification"
        printfn String.replicate 50 "-"

        SymmetryEnforcement.enforceD4Symmetry peps.Data
        SymmetryEnforcement.verifyAllSymmetries peps.Data

        let symmetryDefect = SymmetryEnforcement.computeSymmetryDefect peps.Data
        printfn "  Symmetry defect: %.6f" symmetryDefect

        // Phase 4: MPO-Injectivity verification
        printfn "\n[PHASE 4] MPO-Injectivity Analysis"
        printfn String.replicate 50 "-"

        MPOInjectivity.performCompleteMPOAnalysis peps.Data
        let (isInjective, rank, fullRank) = MPOInjectivity.verifyMPOInjectivity peps.Data

        // Update the PEPS with injectivity information
        let updatedPEPS = {
            peps with
                IsInjective = isInjective
                Metadata = {
                    peps.Metadata with
                        MPOInjectivityVerified = true
                }
        }

        // Phase 5: Final summary
        printfn "\n" + String.replicate 70 "="
        printfn "                    CONSTRUCTION COMPLETE"
        printfn String.replicate 70 "="

        printfn "\n[Final Tensor Properties]"
        printfn "  Physical dimension: %d" updatedPEPS.PhysicalDimension
        printfn "  Bond dimension: %d" updatedPEPS.BondDimension
        printfn "  Non-zero elements: %d/%d (%.2f%%)"
                updatedPEPS.Metadata.NonZeroElements
                updatedPEPS.Metadata.TotalElements
                (100.0 * float updatedPEPS.Metadata.NonZeroElements / float updatedPEPS.Metadata.TotalElements)
        printfn "  MPO-Injective: %s" (if updatedPEPS.IsInjective then "YES ✓" else "NO ✗")
        printfn "  Transfer matrix rank: %d/%d" rank fullRank
        printfn "  Symmetry verified: %s" (if updatedPEPS.Metadata.SymmetryVerified then "YES ✓" else "NO ✗")
        printfn "  Plaquette conditions: %s" (if updatedPEPS.Metadata.PlaquetteVerified then "SATISFIED ✓" else "VIOLATED ✗")
        printfn "  Pentagon equations: %s" (if updatedPEPS.Metadata.PentagonVerified then "SATISFIED ✓" else "VIOLATED ✗")
        printfn "  Hexagon equations: %s" (if updatedPEPS.Metadata.HexagonVerified then "SATISFIED ✓" else "VIOLATED ✗")

        updatedPEPS

    /// Quick construction without full verification (for testing)
    let constructQuickTensor() : FibonacciTensor.FibonacciPEPS =
        FibonacciTensor.constructFibonacciTensor()

    /// Validates an existing tensor
    let validateExistingTensor (peps: FibonacciTensor.FibonacciPEPS) : bool =
        printfn "Validating existing tensor..."

        let mutable isValid = true

        // Check plaquette conditions
        let (plaquetteValid, _) = FibonacciTensor.verifyPlaquetteConditions peps.Data
        if not plaquetteValid then
            printfn "  ✗ Plaquette conditions violated"
            isValid <- false

        // Check symmetries
        let symmetryDefect = SymmetryEnforcement.computeSymmetryDefect peps.Data
        if symmetryDefect > 0.01 then
            printfn "  ✗ Symmetry defect too large: %.4f" symmetryDefect
            isValid <- false

        // Check MPO-injectivity
        let (injective, _, _) = MPOInjectivity.verifyMPOInjectivity peps.Data
        if not injective then
            printfn "  ✗ Not MPO-injective"
            isValid <- false

        if isValid then
            printfn "  ✓ Tensor validation PASSED"
        else
            printfn "  ✗ Tensor validation FAILED"

        isValid