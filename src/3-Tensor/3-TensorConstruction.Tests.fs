// 3-TensorConstruction.Tests.fs
namespace E8.Tests.TensorConstruction

open Xunit
open E8.Algebra
open E8.TensorConstruction

module IntegrationTests =

    [<Fact>]
    let ``Complete construction and verification succeeds within Layer 3 responsibilities`` () =
        let peps = TensorConstructionIntegration.constructAndVerifyFibonacciPEPS()

        Assert.NotNull(peps)
        Assert.Equal(2, peps.PhysicalDimension)
        Assert.Equal(2, peps.BondDimension)
        Assert.True(peps.Metadata.PentagonVerified)
        Assert.True(peps.Metadata.HexagonVerified)
        Assert.True(peps.Metadata.SymmetryVerified)
        Assert.True(peps.Metadata.PlaquetteVerified)

    [<Fact>]
    let ``Quick construction produces a valid tensor object`` () =
        let peps = TensorConstructionIntegration.constructQuickTensor()
        Assert.NotNull(peps)
        Assert.NotNull(peps.Data)
        Assert.Equal(2, peps.PhysicalDimension)
        Assert.Equal(2, peps.BondDimension)

    [<Fact>]
    let ``F2 policy is maintained throughout the construction process`` () =
        // This test verifies that no non-F2 values are present in the final tensor
        let peps = TensorConstructionIntegration.constructAndVerifyFibonacciPEPS()

        // Check that all tensor elements are valid F2 values
        for p in 0 .. peps.PhysicalDimension - 1 do
            for l in 0 .. peps.BondDimension - 1 do
                for r in 0 .. peps.BondDimension - 1 do
                    for u in 0 .. peps.BondDimension - 1 do
                        for d in 0 .. peps.BondDimension - 1 do
                            let value = peps.Data.[p, l, r, u, d]
                            Assert.True(value = F2.Zero || value = F2.One)

    [<Fact>]
    let ``Metadata correctly reflects construction-layer verifications`` () =
        let peps = TensorConstructionIntegration.constructAndVerifyFibonacciPEPS()

        // These should be true because they are verified within this layer's responsibility
        Assert.True(peps.Metadata.SymmetryVerified)
        Assert.True(peps.Metadata.PlaquetteVerified)
        Assert.True(peps.Metadata.PentagonVerified)
        Assert.True(peps.Metadata.HexagonVerified)

        // Fields related to MPOInjectivity are no longer part of this layer's metadata,
        // so no test is needed here. This confirms their removal.