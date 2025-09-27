// 2-SymmetryEnforcement.Tests.fs
// This file verifies the correctness of the symmetry operations defined for hexagonal lattice PEPS tensors.
namespace E8.Tests.TensorConstruction

open Xunit
open E8.Algebra
open E8.Tensors
open E8.TensorConstruction.SymmetryEnforcement
open E8.TensorConstruction.FibonacciTensor // To create a tensor for integration tests

module SymmetryEnforcementTests =

    [<Fact>]
    let ``Symmetry operations transform indices correctly for 3 bonds`` () =
        let indices = (0, 1, 2)  // v1, v2, v3

        // Test Identity
        let identity = applySymmetryToIndices Identity indices
        Assert.Equal(indices, identity)

        // Test C3 Rotations
        let rotated120 = applySymmetryToIndices Rotation120 indices
        Assert.Equal((1, 2, 0), rotated120)

        let rotated240 = applySymmetryToIndices Rotation240 indices
        Assert.Equal((2, 0, 1), rotated240)

        // Test Reflections
        let reflected1 = applySymmetryToIndices Reflection1 indices
        Assert.Equal((0, 2, 1), reflected1)

        let reflected2 = applySymmetryToIndices Reflection2 indices
        Assert.Equal((2, 1, 0), reflected2)

        let reflected3 = applySymmetryToIndices Reflection3 indices
        Assert.Equal((1, 0, 2), reflected3)

    [<Fact>]
    let ``Symmetry group operations compose correctly`` () =
        // Test that rot120 ∘ rot120 = rot240
        let indices = (0, 1, 2)
        let rot120_twice =
            indices
            |> applySymmetryToIndices Rotation120
            |> applySymmetryToIndices Rotation120
        let rot240_once = applySymmetryToIndices Rotation240 indices
        Assert.Equal(rot240_once, rot120_twice)

        // Test that rot120 ∘ rot240 = identity
        let rot_full_cycle =
            indices
            |> applySymmetryToIndices Rotation120
            |> applySymmetryToIndices Rotation240
        Assert.Equal(indices, rot_full_cycle)

        // Test that reflection ∘ reflection = identity
        let reflect_twice =
            indices
            |> applySymmetryToIndices Reflection1
            |> applySymmetryToIndices Reflection1
        Assert.Equal(indices, reflect_twice)

    [<Fact>]
    let ``Enforcement correctly makes a tensor symmetric`` () =
        // This is a crucial integration test. It verifies that the `enforce` function
        // correctly modifies a tensor to make it symmetric, as verified by the `check` function.

        // 1. Create a non-symmetric tensor
        let physDim = 2
        let bondDim = 2
        let tensor = Tensor4F2(physDim, bondDim, bondDim, bondDim)
        // Introduce a clear asymmetry:
        tensor.[0, 0, 1, 2] <- F2.One // Assuming bondDim >= 3 is not required for this logic
        tensor.[0, 1, 2, 0] <- F2.Zero
        tensor.[0, 2, 0, 1] <- F2.Zero

        // 2. Verify it is NOT C3 symmetric initially
        let (isSymmetricBefore, violationsBefore) = checkSymmetry tensor Rotation120
        Assert.False(isSymmetricBefore)
        Assert.True(violationsBefore > 0)

        // 3. Enforce C3 symmetry
        enforceC3Symmetry tensor

        // 4. Verify it IS C3 symmetric after enforcement
        let (isSymmetricAfter, violationsAfter) = checkSymmetry tensor Rotation120
        Assert.True(isSymmetricAfter, "The tensor should be C3 symmetric after enforcement.")
        Assert.Equal(0, violationsAfter)

        // 5. Check that the symmetrization worked as expected (logical OR)
        // The original non-zero element should have propagated to its orbit.
        Assert.Equal(F2.One, tensor.[0, 0, 1, 2])
        Assert.Equal(F2.One, tensor.[0, 1, 2, 0])
        Assert.Equal(F2.One, tensor.[0, 2, 0, 1])

    [<Fact>]
    let ``Symmetry defect is zero for a perfectly symmetric tensor`` () =
        // 1. Create a tensor that is symmetric by construction
        let physDim = 2
        let bondDim = 2
        let tensor = Tensor4F2(physDim, bondDim, bondDim, bondDim)

        // Manually set an orbit to be symmetric
        tensor.[1, 0, 1, 2] <- F2.One
        tensor.[1, 1, 2, 0] <- F2.One
        tensor.[1, 2, 0, 1] <- F2.One

        // 2. Check the symmetry defect
        let defect = computeSymmetryDefect tensor
        Assert.Equal(0.0, defect)

    [<Fact>]
    let ``Symmetry defect is non-zero for an asymmetric tensor`` () =
        // 1. Create an asymmetric tensor
        let physDim = 2
        let bondDim = 2
        let tensor = Tensor4F2(physDim, bondDim, bondDim, bondDim)
        tensor.[0, 0, 0, 1] <- F2.One

        // 2. Check the symmetry defect
        let defect = computeSymmetryDefect tensor
        Assert.True(defect > 0.0)