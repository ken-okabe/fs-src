// 3-TensorConstruction.Tests.fs
// This file contains the final integration tests for Layer 3, ensuring that all
// constituent modules work together to produce a valid, verified PEPS tensor.
namespace E8.Tests.TensorConstruction

open Xunit
open E8.Algebra
open E8.Tensors
open E8.TensorConstruction

module IntegrationTests =

    [<Fact>]
    let ``Complete construction and verification succeeds for hexagonal PEPS`` () =
        // This is the primary end-to-end test for Layer 3.
        // It calls the main integration function and verifies the final output.
        let peps = TensorConstructionIntegration.constructAndVerifyFibonacciPEPS()

        // 1. Verify the object and its basic properties
        Assert.NotNull(peps)
        Assert.NotNull(peps.Data)
        Assert.IsType<Tensor4F2>(peps.Data)
        Assert.Equal(2, peps.PhysicalDimension)
        Assert.Equal(2, peps.BondDimension)

        // 2. Verify that the metadata correctly reflects the successful verification steps
        //    performed during the construction process.
        Assert.True(peps.Metadata.PentagonVerified)
        Assert.True(peps.Metadata.HexagonVerified)
        Assert.True(peps.Metadata.SymmetryVerified)
        Assert.True(peps.Metadata.PlaquetteVerified)

    [<Fact>]
    let ``Quick construction produces a valid tensor object without full verification`` () =
        // This test ensures the quick construction path runs without error and returns
        // a tensor of the correct shape, with metadata indicating non-verification.
        let peps = TensorConstructionIntegration.constructQuickTensor()

        // 1. Verify basic properties
        Assert.NotNull(peps)
        Assert.NotNull(peps.Data)
        Assert.Equal(2, peps.PhysicalDimension)
        Assert.Equal(2, peps.BondDimension)

        // 2. Verify that metadata correctly shows that verifications were skipped
        Assert.False(peps.Metadata.SymmetryVerified)
        Assert.False(peps.Metadata.PlaquetteVerified)