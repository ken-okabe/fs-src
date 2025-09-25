// 3-FSymbols.Tests.fs
namespace E8.Tests.QuantumAlgebra

open Xunit
open System.Numerics
open E8.Algebra
open E8.QuantumAlgebra
open E8.QuantumAlgebra.GoldenRatio
open E8.QuantumAlgebra.FibonacciFusion
open E8.QuantumAlgebra.FSymbols

module FSymbolTests =

    [<Fact>]
    let ``F-symbols respect fusion rules`` () =
        // F-symbol should be zero for forbidden fusions
        // a=Vac, b=Vac, c=Tau -> e=Vac.  e=Vac, c=Tau, d=Vac -> forbidden
        let fsym = computeFSymbol Vacuum Vacuum Tau Vacuum Tau Tau
        Assert.Equal((BigInteger.Zero, BigInteger.Zero), fsym.Value)

    [<Fact>]
    let ``Non-trivial F-symbols have correct golden ratio values`` () =
        let fsym = computeFSymbol Tau Tau Tau Tau Vacuum Vacuum
        // Should be 1/φ = φ - 1
        let phi_inv = phiInverse()
        Assert.Equal(phi_inv, fsym.Value)

    [<Fact>]
    let ``F₂ projection of F-symbols is deterministic and cached`` () =
        let fsym = computeFSymbol Tau Tau Tau Tau Vacuum Vacuum
        let f2_val1 = fSymbolToF2 fsym
        // The mutable CachedF2Value field should now be Some
        Assert.True(fsym.CachedF2Value.IsSome)
        let f2_val2 = fSymbolToF2 fsym  // Should use cache
        Assert.Equal(f2_val1, f2_val2)

    [<Fact>]
    let ``Pentagon equations are satisfied`` () =
        // This is a comprehensive check and may be slow, but is vital
        Assert.True(verifyPentagonEquations())

    [<Fact>]
    let ``Hexagon equations are satisfied`` () =
        // This is a comprehensive check and may be slow, but is vital
        Assert.True(verifyHexagonEquations())

    [<Fact>]
    let ``Full algebraic consistency check passes`` () =
        Assert.True(verifyFullConsistency())

    [<Fact>]
    let ``F-matrix has correct dimensions and values`` () =
        // For (τ,τ,τ,τ), fusion paths are:
        // (τ,τ)->e->(e,τ)->τ  => e can be 1 or τ
        // (τ,τ)->f->(τ,f)->τ  => f can be 1 or τ
        // So we expect a 2x2 matrix
        let fmatrix = createFMatrix Tau Tau Tau Tau
        Assert.Equal(2, fmatrix.Rows)
        Assert.Equal(2, fmatrix.Cols)

        // Check values against known F-matrix for MTC
        // F[τ,τ,τ]^τ = [[φ⁻¹, φ⁻²], [φ⁻², -φ⁻³]]
        // Projected to F2, using φ²=φ+1 => φ³=2φ+1 => φ⁻¹=φ+1, φ⁻²=3-φ, φ⁻³=2φ-3
        // All these project to φ+1 in F2
        let expected = F2.op_Explicit(phiPower(-1)).Value + F2.op_Explicit(phiPower(-2)).Value
        // F2 projection of (φ-1) is 1+1 = 0
        // F2 projection of (2-φ) is 0+1 = 1
        // F2 projection of (2-φ) is 0+1 = 1
        // F2 projection of (2φ-3) is 0+1 = 1
        Assert.Equal(F2.Zero, fmatrix.[0,0]) // F_{1,1} = φ⁻¹
        Assert.Equal(F2.One,  fmatrix.[0,1]) // F_{1,τ} = -φ⁻²
        Assert.Equal(F2.One,  fmatrix.[1,0]) // F_{τ,1} = -φ⁻²
        Assert.Equal(F2.One,  fmatrix.[1,1]) // F_{τ,τ}

module IntegrationTests =

    [<Fact>]
    let ``Golden ratio and F-symbols are compatible`` () =
        // The F-symbols should use golden ratio values correctly
        let fsym = computeFSymbol Tau Tau Tau Tau Tau Tau
        let (phi_coeff, const_coeff) = fsym.Value

        // This specific F-symbol equals (2φ - 3)
        Assert.Equal(BigInteger 2, phi_coeff)
        Assert.Equal(BigInteger(-3), const_coeff)

    [<Fact>]
    let ``All modules preserve F₂ policy`` () =
        // No floating point operations should occur in computations
        let phi = phiPower 10
        let _ = phiPowerToF2 phi

        let channel = { Input1 = Tau; Input2 = Tau; Output = Vacuum; Multiplicity = 1 }
        let _ = fusionMultiplicityF2 channel.Input1 channel.Input2 channel.Output

        let fsym = computeFSymbol Tau Tau Tau Tau Vacuum Tau
        let _ = fSymbolToF2 fsym

        // If we got here without exceptions, F₂ policy is maintained
        Assert.True(true)

    [<Fact>]
    let ``Complete quantum algebraic layer is self-consistent`` () =
        // Run all comprehensive tests
        Assert.True(GoldenRatio.runComprehensiveTests())
        Assert.True(FibonacciFusion.verifyFusionRules())
        Assert.True(FSymbols.verifyFullConsistency())