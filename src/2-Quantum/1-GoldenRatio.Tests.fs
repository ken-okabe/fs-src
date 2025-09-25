// 1-GoldenRatio.Tests.fs
namespace E8.Tests.QuantumAlgebra

open Xunit
open E8.QuantumAlgebra.GoldenRatio
open System.Numerics

module GoldenRatioTests =

    [<Fact>]
    let ``Fibonacci computation is correct`` () =
        Assert.Equal(BigInteger.Zero, computeFibonacci 0)
        Assert.Equal(BigInteger.One, computeFibonacci 1)
        Assert.Equal(BigInteger.One, computeFibonacci 2)
        Assert.Equal(BigInteger 2, computeFibonacci 3)
        Assert.Equal(BigInteger 3, computeFibonacci 4)
        Assert.Equal(BigInteger 5, computeFibonacci 5)
        Assert.Equal(BigInteger 8, computeFibonacci 6)
        Assert.Equal(BigInteger 13, computeFibonacci 7)

    [<Fact>]
    let ``Golden ratio satisfies φ² = φ + 1`` () =
        let phi = phiPower 1
        let phi2 = phiPower 2
        // φ² should equal φ + 1, which is F₂φ + F₁ = 1·φ + 1
        Assert.Equal(BigInteger.One, phi2.FibN)
        Assert.Equal(BigInteger.One, phi2.FibNMinus1)

    [<Fact>]
    let ``Multiplication in Q(φ) is correct`` () =
        // (φ + 1) × (φ - 1) = φ² - 1 = (φ + 1) - 1 = φ
        let phi_plus_1 = (BigInteger.One, BigInteger.One)
        let phi_minus_1 = (BigInteger.One, BigInteger.MinusOne)
        let result = multiplyPhiExpressions phi_plus_1 phi_minus_1
        Assert.Equal((BigInteger.One, BigInteger.Zero), result)

    [<Fact>]
    let ``Cassini identity holds`` () =
        for n in 1..10 do
            Assert.True(verifyCassini n)

    [<Fact>]
    let ``Binet formula is verified`` () =
        for n in 0..20 do
            Assert.True(verifyBinet n)

    [<Fact>]
    let ``F₂ projection preserves field operations`` () =
        let expr1 = (BigInteger 3, BigInteger 5)  // 3φ + 5
        let expr2 = (BigInteger 7, BigInteger 2)  // 7φ + 2

        // Project individually
        let (p1_phi, p1_const) = projectToF2 expr1
        let (p2_phi, p2_const) = projectToF2 expr2

        // Add in Q(φ) then project
        let sum = addPhiExpressions expr1 expr2
        let (sum_phi, sum_const) = projectToF2 sum

        // Should satisfy: π(a + b) = π(a) + π(b) in F₂
        Assert.Equal(E8.Algebra.F2.add p1_phi p2_phi, sum_phi)
        Assert.Equal(E8.Algebra.F2.add p1_const p2_const, sum_const)

    [<Fact>]
    let ``Comprehensive golden ratio tests pass`` () =
        // Note: This test will print to console, which is fine for verification scripts
        // In a real test suite, one might capture output or just assert the bool result
        Assert.True(runComprehensiveTests())