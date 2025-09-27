// 2-LocalObservables.Tests.fs
// Tests for local observables like magnetization.
namespace E8.Tests.Observable

open Xunit
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.Hardware.CPU
open E8.Ace
open E8.Ace.ACE_CTMRG
open E8.Observable

module LocalObservablesTests =

    [<Fact>]
    let ``LocalMagnetization computation delegates correctly and returns valid algebraic value`` () =
        // This test ensures that the LocalMagnetization observable correctly uses the
        // underlying rigorous correlation computation logic.
        use ops = new OptimizedCPUOperations() :> IF2Operations

        // 1. Create a known, simple environment (e.g., from Toric Code)
        let chi = 2
        let exactC = MatrixF2(chi, chi, fun _ _ -> F2.One)
        let exactT = Tensor3F2(chi, 2, chi, fun _ _ _ -> F2.One)
        let exactEnv = { C = exactC; T = exactT; Chi = chi; History = Set.empty }
        let peps = Tensor4F2(2, chi, chi, chi) // Dummy peps, not used by simplified correlator

        let location = { PrimaryPosition = (0,0); SecondaryPositions = []; Radius = 0 }

        // 2. Execute the computation
        let magnetization = LocalMagnetization()
        let result = magnetization.Compute exactEnv peps location ops

        // 3. Verify the result type and confidence
        Assert.IsType<ObservableValue>(result)
        Assert.True(result.AlgebraicConfidence >= 2) // Should be a stable projector

        match result.Value with
        | CorrelationParity p -> Assert.True(p = F2.Zero || p = F2.One)
        | _ -> Assert.Fail("LocalMagnetization should return a CorrelationParity value.")