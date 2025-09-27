// 1-FibonacciTensor.fs
// Construction of the A_Fibonacci PEPS tensor encoding topological order for a hexagonal lattice.
// This module contains only pure functions for constructing tensor data structures.
// All verification logic is delegated to the corresponding test file.
namespace E8.TensorConstruction

open System
open E8.Algebra
open E8.TensorAlgebra
open E8.Tensors
open E8.QuantumAlgebra
open E8.QuantumAlgebra.FibonacciFusion
open E8.QuantumAlgebra.FSymbols

/// Module for constructing Fibonacci PEPS tensors for hexagonal lattices.
module FibonacciTensor =

    /// Metadata about the constructed tensor.
    type TensorMetadata = {
        CreationTime: DateTime
        ConstructionMethod: string
        PhysicalDimension: int
        BondDimension: int
        NonZeroElements: int
        TotalElements: int
        PentagonVerified: bool
        HexagonVerified: bool
        MemorySavingsRatio: float
    }

    /// The main Fibonacci PEPS tensor structure for a hexagonal lattice.
    type FibonacciPEPS = {
        /// The actual tensor data, a 4th-rank tensor for 3 virtual bonds.
        Data: Tensor4F2
        /// Physical dimension (2 for Fibonacci: vacuum and tau).
        PhysicalDimension: int
        /// Bond dimension (virtual indices).
        BondDimension: int
        /// Complete metadata.
        Metadata: TensorMetadata
    }

    /// Computes the amplitude for a given tensor element using F-symbols.
    /// For a hexagonal tensor with 1 physical and 3 virtual bonds (v1, v2, v3).
    let computeAmplitude (physical: AnyonType) (v1: AnyonType) (v2: AnyonType) (v3: AnyonType) : F2 =
        // The amplitude is determined by the fusion rules of Fibonacci anyons.
        // A non-zero amplitude exists if there is a valid fusion path.
        // Fusion path for 3 virtual indices: (v1 ⊗ v2) ⊗ v3 -> physical
        let mutable totalAmplitude = F2.Zero

        // Sum over all possible intermediate fusion channels for the first two virtual bonds.
        for intermediate in [AnyonType.Vacuum; AnyonType.Tau] do
            if isAllowedFusion v1 v2 intermediate &&
               isAllowedFusion intermediate v3 physical then

                // The amplitude is determined by the fusion multiplicity of the final step.
                let amplitude = fusionMultiplicityF2 intermediate v3 physical

                // In F₂, addition is XOR. Summing over paths accumulates the parity.
                totalAmplitude <- F2.add totalAmplitude amplitude

        totalAmplitude

    /// Creates the raw tensor data for A_Fibonacci on a hexagonal lattice.
    let createRawTensor (physDim: int) (bondDim: int) : Tensor4F2 =
        // For Fibonacci anyons on a hexagonal lattice: physDim = 2, bondDim = 2.
        let tensor = Tensor4F2(physDim, bondDim, bondDim, bondDim)

        // Iterate over all tensor elements and set their values based on fusion rules.
        for p in 0 .. physDim - 1 do
            for v1 in 0 .. bondDim - 1 do
                for v2 in 0 .. bondDim - 1 do
                    for v3 in 0 .. bondDim - 1 do
                        let physAnyon = AnyonType.FromInt p
                        let v1Anyon = AnyonType.FromInt v1
                        let v2Anyon = AnyonType.FromInt v2
                        let v3Anyon = AnyonType.FromInt v3

                        let amplitude = computeAmplitude physAnyon v1Anyon v2Anyon v3Anyon
                        tensor.[p, v1, v2, v3] <- amplitude
        tensor

    /// Main construction function for the Fibonacci PEPS tensor for a hexagonal lattice.
    let constructFibonacciTensor() : FibonacciPEPS =
        let startTime = DateTime.UtcNow

        // Dimensions for Fibonacci anyons.
        let physDim = 2
        let bondDim = 2

        // Step 1: Verify algebraic consistency of the underlying F-symbols.
        // This is a prerequisite for constructing a valid tensor.
        let pentagonValid = FSymbols.verifyPentagonEquations()
        let hexagonValid = FSymbols.verifyHexagonEquations()

        if not pentagonValid || not hexagonValid then
            failwith "F-symbol consistency check failed! Cannot construct a valid tensor."

        // Step 2: Create raw tensor based on the verified algebraic rules.
        let tensor = createRawTensor physDim bondDim

        let nonZeroCount = tensor.CountNonZero()
        let totalElements = tensor.TotalElements

        // Create the final PEPS structure with its metadata.
        let peps = {
            Data = tensor
            PhysicalDimension = physDim
            BondDimension = bondDim
            Metadata = {
                CreationTime = startTime
                ConstructionMethod = "F-symbol based for hexagonal lattice"
                PhysicalDimension = physDim
                BondDimension = bondDim
                NonZeroElements = nonZeroCount
                TotalElements = totalElements
                PentagonVerified = pentagonValid
                HexagonVerified = hexagonValid
                MemorySavingsRatio = 1.0 - (float totalElements / (float totalElements * 64.0)) // Bit-packing savings
            }
        }
        peps