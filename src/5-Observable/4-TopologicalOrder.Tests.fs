// 4-TopologicalOrder.Tests.fs
// Tests for topological observables like TEE and Wilson Loops.
namespace E8.Tests.Observable

open Xunit
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.Hardware.CPU
open E8.Ace
open E8.Ace.ACE_CTMRG
open E8.Observable

module TopologicalOrderTests =

    // Helper function to create the exact PEPS and environment for the Toric Code model.
    let private createToricCodeFixtures () =
        let physDim, bondDim = 2, 2
        let chi = bondDim
        let toricPEPS = Tensor4F2(physDim, bondDim, bondDim, bondDim)
        for p in 0..1 do
            for v1 in 0..1 do
                for v2 in 0..1 do
                    for v3 in 0..1 do
                        if (v1 + v2 + v3) % 2 = p then
                            toricPEPS.[p, v1, v2, v3] <- F2.One

        let exactC = MatrixF2(chi, chi, fun _ _ -> F2.One)
        let exactT = Tensor3F2(chi, physDim, chi, fun _ _ _ -> F2.One)
        let exactEnv = { C = exactC; T = exactT; Chi = chi; History = Set.empty }
        (toricPEPS, exactEnv)

    [<Fact>]
    let ``TopologicalEntropyMeasurement computes correct GSD for Toric Code`` () =
        // This test verifies that the TEE observable correctly computes the GSD
        // by analyzing the algebraic structure of a known environment, avoiding placeholders.
        use ops = new OptimizedCPUOperations() :> IF2Operations
        let (peps, env) = createToricCodeFixtures()
        let location = { PrimaryPosition = (0,0); SecondaryPositions = []; Radius = 1 }

        let tee = TopologicalEntropyMeasurement()
        let result = tee.Compute env peps location ops

        // For the Toric Code on a torus, the Ground State Degeneracy is exactly 4.
        // Our algorithm should deduce this from the rank of the transfer matrix.
        let expectedGSD = 4

        match result.Value with
        | GroundStateDegeneracy gsd -> Assert.Equal(expectedGSD, gsd)
        | _ -> Assert.Fail("TEE should be represented as a GroundStateDegeneracy.")

        Assert.True(result.AlgebraicConfidence = 2)

    [<Fact>]
    let ``WilsonLoop correctly distinguishes trivial and non-trivial loops for Toric Code`` () =
        // This test verifies that the WilsonLoop implementation correctly computes the parity
        // for physically distinct situations in a model with known exact solutions.
        use ops = new OptimizedCPUOperations() :> IF2Operations
        let (peps, env) = createToricCodeFixtures()

        // --- Test Case 1: A non-trivial loop ---
        // In the Toric Code, a Wilson loop around a non-trivial cycle of the torus
        // should yield a non-trivial phase (parity 1).
        let nonTrivialLoop = { PrimaryPosition = (0,0); SecondaryPositions = [(1,0); (1,1); (0,1)]; Radius = 0 }
        let wilsonLoop = WilsonLoop()
        let resultNonTrivial = wilsonLoop.Compute env peps nonTrivialLoop ops

        match resultNonTrivial.Value with
        | WilsonLoopPhase phase -> Assert.Equal(F2.One, phase)
        | _ -> Assert.Fail("WilsonLoop should return a WilsonLoopPhase.")
        Assert.True(resultNonTrivial.AlgebraicConfidence = 2)

        // --- Test Case 2: A trivial (contractible) loop ---
        // A small loop that goes back on itself should be trivial (parity 0).
        let trivialLoop = { PrimaryPosition = (0,0); SecondaryPositions = [(1,0); (0,0)]; Radius = 0 }
        let resultTrivial = wilsonLoop.Compute env peps trivialLoop ops

        match resultTrivial.Value with
        | WilsonLoopPhase phase -> Assert.Equal(F2.Zero, phase)
        | _ -> Assert.Fail("WilsonLoop should return a WilsonLoopPhase.")
        Assert.True(resultTrivial.AlgebraicConfidence = 2)