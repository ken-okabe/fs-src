// 5-SpectralAnalysis.Tests.fs
// Tests for observables derived from spectral properties.
namespace E8.Tests.Observable

open Xunit
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.Hardware.CPU
open E8.Ace
open E8.Ace.ACE_CTMRG
open E8.Observable

module SpectralAnalysisTests =

    [<Fact>]
    let ``SpectralObservable correctly deduces AlgebraicConfidence`` () =
        // This test verifies that the confidence metric is correctly extracted
        // as the minimal polynomial degree from Layer 4.
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let chi = 2
        let exactC = MatrixF2.Identity(chi) // Leads to a projector T
        let exactT = Tensor3F2(chi, 2, chi, fun i p j -> if i=j && p=0 then F2.One else F2.Zero)
        let exactEnv = { C = exactC; T = exactT; Chi = chi; History = Set.empty }
        let peps = Tensor4F2(2, chi, chi, chi) // Dummy peps

        let location = { PrimaryPosition = (0,0); SecondaryPositions = []; Radius = 0 }

        let confidenceObservable = SpectralObservable("confidence")
        let result = confidenceObservable.Compute exactEnv peps location ops

        // For a perfect projector, the minimal polynomial is x^2+x, so the degree is 2.
        let expectedConfidence = 2

        Assert.Equal(AlgebraicConfidenceValue, result.Kind)
        Assert.Equal(expectedConfidence, result.AlgebraicConfidence)
        match result.Value with
        | GroundStateDegeneracy deg -> Assert.Equal(expectedConfidence, deg)
        | _ -> Assert.Fail("Confidence should be returned as an integer value.")

    [<Fact>]
    let ``SpectralObservable correctly deduces SpectralGap`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let chi = 2
        let exactC = MatrixF2.Identity(chi)
        let exactT = Tensor3F2(chi, 2, chi, fun i p j -> if i=j && p=0 then F2.One else F2.Zero)
        let exactEnv = { C = exactC; T = exactT; Chi = chi; History = Set.empty }
        let peps = Tensor4F2(2, chi, chi, chi)
        let location = { PrimaryPosition = (0,0); SecondaryPositions = []; Radius = 0 }

        let gapObservable = SpectralObservable("spectralgap")
        let result = gapObservable.Compute exactEnv peps location ops

        // For a non-trivial projector, the gap between eigenvalues {0, 1} is 1.
        let expectedGap = F2.One

        Assert.Equal(SpectralGap, result.Kind)
        match result.Value with
        | Parity p -> Assert.Equal(expectedGap, p)
        | _ -> Assert.Fail("SpectralGap should be returned as a Parity value.")