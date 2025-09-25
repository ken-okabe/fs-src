// 2-FibonacciFusion.Tests.fs
namespace E8.Tests.QuantumAlgebra

open Xunit
open E8.QuantumAlgebra.FibonacciFusion
open System.Numerics

module FibonacciFusionTests =

    [<Fact>]
    let ``Vacuum is the identity for fusion`` () =
        Assert.Equal([Vacuum], fusionRule Vacuum Vacuum)
        Assert.Equal([Tau], fusionRule Vacuum Tau)
        Assert.Equal([Tau], fusionRule Tau Vacuum)

    [<Fact>]
    let ``Tau fusion produces both outcomes`` () =
        let result = fusionRule Tau Tau
        Assert.Equal(2, result.Length)
        Assert.Contains(Vacuum, result)
        Assert.Contains(Tau, result)

    [<Fact>]
    let ``Fusion multiplicities are correct`` () =
        Assert.Equal(1, fusionMultiplicity Vacuum Vacuum Vacuum)
        Assert.Equal(0, fusionMultiplicity Vacuum Vacuum Tau)
        Assert.Equal(1, fusionMultiplicity Tau Tau Vacuum)
        Assert.Equal(1, fusionMultiplicity Tau Tau Tau)

    [<Fact>]
    let ``Fusion algebra is associative`` () =
        Assert.True(validateAssociativity())

    [<Fact>]
    let ``Quantum dimensions are correct`` () =
        let d_vacuum = quantumDimension Vacuum
        let d_tau = quantumDimension Tau

        // d_vacuum = 1 (represented as 0·φ + 1)
        Assert.Equal((BigInteger.Zero, BigInteger.One), d_vacuum)

        // d_tau = φ (represented as 1·φ + 0)
        Assert.Equal((BigInteger.One, BigInteger.Zero), d_tau)

    [<Fact>]
    let ``Fusion rules verification passes`` () =
        Assert.True(verifyFusionRules())