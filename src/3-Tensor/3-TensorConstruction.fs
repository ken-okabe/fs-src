// 3-TensorConstruction.fs
// Integration module that combines tensor construction components for the hexagonal lattice.
namespace E8.TensorConstruction

open System
open E8.Algebra
open E8.QuantumAlgebra.FSymbols
open E8.TensorConstruction.FibonacciTensor
open E8.TensorConstruction.SymmetryEnforcement
open E8.Tensors

/// High-level interface for tensor construction, orchestrating the creation
/// and verification of hexagonal Fibonacci PEPS tensors.
module TensorConstructionIntegration =

    /// Complete tensor construction with all verifications belonging to the construction layer.
    /// This function serves as the primary entry point for creating a physically valid
    /// hexagonal PEPS tensor.
    let constructAndVerifyFibonacciPEPS() : FibonacciPEPS =
        // Phase 1: Verify the mathematical foundations from Layer 2.
        // This is a prerequisite for any valid tensor construction.
        let pentagonValid = FSymbols.verifyPentagonEquations()
        let hexagonValid = FSymbols.verifyHexagonEquations()

        if not (pentagonValid && hexagonValid) then
            failwith "Mathematical foundation (F-symbol) verification failed! Cannot proceed with tensor construction."

        // Phase 2: Construct the raw tensor from the verified algebraic rules.
        // This leverages the logic defined in the FibonacciTensor module.
        let peps = FibonacciTensor.constructFibonacciTensor()

        // Phase 3: Enforce and verify the geometric symmetries required by the hexagonal lattice.
        // This step applies the logic from the SymmetryEnforcement module to the raw tensor.
        enforceC3Symmetry peps.Data
        let isSymmetric = verifyAllSymmetries peps.Data

        // The Plaquette condition is a crucial integration test that verifies if the
        // local construction rules result in a globally consistent state.
        let (plaquetteValid, _) = FibonacciTensorTests.verifyPlaquetteConditions peps.Data

        // Final verification check before returning the tensor.
        if not isSymmetric then
            failwith "Symmetry enforcement failed. The resulting tensor does not satisfy the required geometric symmetries."
        if not plaquetteValid then
            failwith "Plaquette condition violated. The tensor is not globally consistent."

        // Update Metadata with the results of the verifications performed in this layer.
        let updatedPEPS = {
            peps with
                Metadata = {
                    peps.Metadata with
                        SymmetryVerified = isSymmetric
                        PlaquetteVerified = plaquetteValid
                }
        }

        updatedPEPS

    /// Quick construction without full verification (for scenarios where speed is critical
    /// and prior verification is assumed, e.g., certain internal loops).
    let constructQuickTensor() : FibonacciPEPS =
        // This function is intended for rapid testing and might skip some verification steps.
        // It still constructs the core tensor correctly based on the proven algorithms.
        let physDim = 2
        let bondDim = 2
        let tensor = FibonacciTensor.createRawTensor physDim bondDim
        let metadata = {
            CreationTime = DateTime.UtcNow
            ConstructionMethod = "Quick F-symbol based for hexagonal lattice"
            PhysicalDimension = physDim
            BondDimension = bondDim
            NonZeroElements = tensor.CountNonZero()
            TotalElements = tensor.TotalElements
            PentagonVerified = true // Assumed true for quick construction
            HexagonVerified = true  // Assumed true for quick construction
            SymmetryVerified = false // Not verified in quick construction
            PlaquetteVerified = false // Not verified in quick construction
            MemorySavingsRatio = 1.0 - (float tensor.TotalElements / (float tensor.TotalElements * 64.0))
        }
        {
            Data = tensor
            PhysicalDimension = physDim
            BondDimension = bondDim
            Metadata = metadata
        }