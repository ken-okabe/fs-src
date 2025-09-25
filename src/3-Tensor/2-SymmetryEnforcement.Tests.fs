// 2-SymmetryEnforcement.Tests.fs
namespace E8.Tests.TensorConstruction

open Xunit
open E8.Algebra
open E8.Tensors
open E8.TensorConstruction.SymmetryEnforcement

module SymmetryEnforcementTests =

    [<Fact>]
    let ``Symmetry operations transform indices correctly`` () =
        let indices = (0, 1, 2, 3)  // l, r, u, d

        let rotated90 = applySymmetryToIndices Rotation90 indices
        Assert.Equal((3, 2, 0, 1), rotated90)

        let rotated180 = applySymmetryToIndices Rotation180 indices
        Assert.Equal((1, 0, 3, 2), rotated180)

        let reflected_h = applySymmetryToIndices ReflectionH indices
        Assert.Equal((1, 0, 2, 3), reflected_h)

        let identity = applySymmetryToIndices Identity indices
        Assert.Equal(indices, identity)

    [<Fact>]
    let ``D4 symmetry group operations compose correctly`` () =
        // Test that rot90 ∘ rot90 = rot180
        let indices = (0, 1, 2, 3)
        let rot90_twice =
            indices
            |> applySymmetryToIndices Rotation90
            |> applySymmetryToIndices Rotation90
        let rot180_once = applySymmetryToIndices Rotation180 indices
        Assert.Equal(rot180_once, rot90_twice)

    [<Fact>]
    let ``Symmetry enforcement preserves tensor properties`` () =
        let tensor = FibonacciTensor.createRawTensor 2 2
        let nonZeroBefore = tensor.CountNonZero()

        enforceD4Symmetry tensor

        let nonZeroAfter = tensor.CountNonZero()
        // Enforcement should not drastically change sparsity
        Assert.True(abs(nonZeroAfter - nonZeroBefore) < tensor.TotalElements / 2)

    [<Fact>]
    let ``Random symmetric tensor is actually symmetric`` () =
        let tensor = generateRandomSymmetricTensor 2 2 42
        let (isSymmetric, violations) = checkSymmetry tensor Rotation90
        Assert.True(isSymmetric)
        Assert.Equal(0, violations)

    [<Fact>]
    let ``Symmetry defect is zero for symmetric tensor`` () =
        let tensor = generateRandomSymmetricTensor 2 2 123
        let defect = computeSymmetryDefect tensor
        Assert.True(defect < 0.001)  // Should be essentially zero