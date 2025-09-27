// 1-FibonacciTensor.Tests.fs
namespace E8.Tests.TensorConstruction

open Xunit
open E8.Algebra
open E8.Tensors
open E8.TensorConstruction.FibonacciTensor
open E8.QuantumAlgebra
open E8.QuantumAlgebra.FibonacciFusion
open E8.QuantumAlgebra.AnyonType

module FibonacciTensorTests =

    [<Fact>]
    let ``Tensor amplitudes are computed correctly for 3 virtual bonds`` () =
        // Test a trivial fusion path: (V ⊗ V) ⊗ V -> V
        // (V ⊗ V) -> V, then V ⊗ V -> V. This path is allowed and unique.
        let amp1 = computeAmplitude AnyonType.Vacuum AnyonType.Vacuum AnyonType.Vacuum AnyonType.Vacuum
        Assert.Equal(F2.One, amp1)

        // Test a non-trivial fusion path: (τ ⊗ τ) ⊗ τ -> τ
        // Path 1: (τ ⊗ τ) -> V, then V ⊗ τ -> τ. Allowed.
        // Path 2: (τ ⊗ τ) -> τ, then τ ⊗ τ -> 1 ⊕ τ. One path to τ is allowed.
        // Total amplitude in F2 should be 1 + 1 = 0.
        let amp2 = computeAmplitude AnyonType.Tau AnyonType.Tau AnyonType.Tau AnyonType.Tau
        Assert.Equal(F2.Zero, amp2)

        // Test another path: (τ ⊗ V) ⊗ τ -> V
        // (τ ⊗ V) -> τ, then τ ⊗ τ -> V. Allowed.
        let amp3 = computeAmplitude AnyonType.Vacuum AnyonType.Tau AnyonType.Vacuum AnyonType.Tau
        Assert.Equal(F2.One, amp3)

    [<Fact>]
    let ``Constructed raw tensor has correct hexagonal dimensions`` () =
        let tensor = createRawTensor 2 2
        Assert.IsType<Tensor4F2>(tensor)
        Assert.Equal(2, tensor.D1)  // Physical
        Assert.Equal(2, tensor.D2)  // Virtual 1
        Assert.Equal(2, tensor.D3)  // Virtual 2
        Assert.Equal(2, tensor.D4)  // Virtual 3

    [<Fact>]
    let ``Tensor construction is deterministic`` () =
        let tensor1 = createRawTensor 2 2
        let tensor2 = createRawTensor 2 2

        // Check that the same elements are produced for the entire tensor.
        for p in 0..1 do
            for v1 in 0..1 do
                for v2 in 0..1 do
                    for v3 in 0..1 do
                        Assert.Equal(tensor1.[p,v1,v2,v3], tensor2.[p,v1,v2,v3])

    /// Verifies the plaquette conditions for the tensor on a hexagonal lattice.
    /// This is a critical integration test for the createRawTensor algorithm, ensuring
    /// that the locally defined fusion rules lead to a globally consistent state.
    let verifyPlaquetteConditions (tensor: Tensor4F2) : bool * int =
        let mutable isValid = true
        let mutable violationCount = 0

        // A hexagonal plaquette involves 6 sites and 6 internal virtual bonds.
        // To avoid combinatorial explosion, we test a representative set of non-trivial configurations
        // that are known to probe the topological nature of the state.

        // Configuration 1: A loop of Tau anyons on the internal edges.
        let internalEdges = Array.create 6 AnyonType.Tau

        // Physical sites are all Vacuum. This should result in a non-zero amplitude
        // as the Tau loop can propagate freely.
        let physicalSites = Array.create 6 AnyonType.Vacuum

        // 1. Calculate the amplitude by contracting the six PEPS tensors for the plaquette.
        let mutable calculatedAmplitude = F2.One
        for i in 0..5 do
            let p = physicalSites.[i].ToInt()
            let v_in = internalEdges.[(i + 5) % 6].ToInt() // Bond from previous site
            let v_out = internalEdges.[i].ToInt()         // Bond to next site
            // The third virtual leg connects to a trivial boundary (assumed Vacuum).
            let v_boundary = AnyonType.Vacuum.ToInt()

            // The tensor indices are (phys, v1, v2, v3). We need a consistent mapping for the loop.
            // Let's map (p, v_in, v_out, v_boundary)
            let tensorElement = tensor.[p, v_in, v_out, v_boundary]
            calculatedAmplitude <- F2.mul calculatedAmplitude tensorElement

        // 2. Calculate the theoretically expected amplitude from fusion rules.
        // A closed loop of Tau anyons with Vacuum physical sites must have a non-zero amplitude.
        // This corresponds to a valid string-net configuration.
        let expectedAmplitude = F2.One

        // 3. Compare the results.
        if calculatedAmplitude <> expectedAmplitude then
            isValid <- false
            violationCount <- violationCount + 1
            Xunit.Assert.Fail(sprintf "VIOLATION: Plaquette (All V) mismatch. Calculated: %A, Expected: %A" calculatedAmplitude expectedAmplitude)

        (isValid, violationCount)

    [<Fact>]
    let ``Constructed tensor satisfies hexagonal plaquette conditions`` () =
        // This is the main integration test for this module.
        // It verifies that the createRawTensor algorithm produces a physically valid tensor
        // by checking its global properties on a closed loop (plaquette).
        let peps = constructFibonacciTensor()

        let (plaquetteValid, violations) = verifyPlaquetteConditions peps.Data
        Assert.True(plaquetteValid, sprintf "Found %d plaquette violations." violations)