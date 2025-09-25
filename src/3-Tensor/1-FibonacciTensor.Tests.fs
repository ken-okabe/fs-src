// 1-FibonacciTensor.Tests.fs
namespace E8.Tests.TensorConstruction

open Xunit
open E8.Algebra
open E8.Tensors
open E8.TensorConstruction.FibonacciTensor
open E8.QuantumAlgebra.FibonacciFusion

module FibonacciTensorTests =

    [<Fact>]
    let ``Plaquette configurations respect fusion rules`` () =
        let config1 = {
            TopLeft = Vacuum
            TopRight = Tau
            BottomLeft = Tau
            BottomRight = Vacuum
            LeftEdge = Tau
            TopEdge = Tau
            RightEdge = Tau
            BottomEdge = Tau
        }
        Assert.True(isValidPlaquette config1)

        let config2 = {
            TopLeft = Vacuum
            TopRight = Vacuum
            BottomLeft = Vacuum
            BottomRight = Tau  // Inconsistent
            LeftEdge = Vacuum
            TopEdge = Vacuum
            RightEdge = Vacuum
            BottomEdge = Vacuum
        }
        Assert.False(isValidPlaquette config2)

    [<Fact>]
    let ``Tensor amplitudes are computed correctly`` () =
        // Test vacuum amplitude
        let amp1 = computeAmplitude Vacuum Vacuum Vacuum Vacuum Vacuum
        Assert.Equal(F2.One, amp1)  // Identity should have amplitude 1

        // Test non-trivial amplitude
        let amp2 = computeAmplitude Tau Tau Tau Vacuum Vacuum
        // This should be non-zero due to fusion rules
        Assert.NotEqual(F2.Zero, amp2)

    [<Fact>]
    let ``Constructed tensor has correct dimensions`` () =
        let tensor = createRawTensor 2 2
        Assert.Equal(2, tensor.D1)  // Physical
        Assert.Equal(2, tensor.D2)  // Left
        Assert.Equal(2, tensor.D3)  // Right
        Assert.Equal(2, tensor.D4)  // Up
        Assert.Equal(2, tensor.D5)  // Down

    [<Fact>]
    let ``Tensor construction is deterministic`` () =
        let tensor1 = createRawTensor 2 2
        let tensor2 = createRawTensor 2 2

        // Check that the same elements are produced
        for p in 0..1 do
            for l in 0..1 do
                for r in 0..1 do
                    for u in 0..1 do
                        for d in 0..1 do
                            Assert.Equal(tensor1.[p,l,r,u,d], tensor2.[p,l,r,u,d])

    [<Fact>]
    let ``Full construction completes without errors`` () =
        let peps = constructFibonacciTensor()
        Assert.NotNull(peps)
        Assert.NotNull(peps.Data)
        Assert.True(peps.Metadata.PentagonVerified)
        Assert.True(peps.Metadata.HexagonVerified)