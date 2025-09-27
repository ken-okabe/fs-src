// src/4-PEPS-Ace/2-ACE_CTMRG.Tests.fs
// This file contains the crucial integration tests for the new C3-symmetric CTMRG engine.
namespace E8.Tests.Ace

open System
open Xunit
open FsUnit.Xunit
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.Hardware.CPU
open E8.Ace
open E8.Ace.ACE_CTMRG
open E8.TensorConstruction.FibonacciTensor

module ACE_CTMRGTests =

    // Helper to check if two environments are identical bit-by-bit.
    let areEnvironmentsEqual (env1: SymmetricEnvironment) (env2: SymmetricEnvironment) =
        let cEqual = Array.forall2 (=) env1.C.Data env2.C.Data
        let tEqual = Array.forall2 (=) env1.T.Data.Data env2.T.Data.Data
        cEqual && tEqual

    [<Fact>]
    let ``Full CTMRG converges to a fixed point for a Fibonacci PEPS`` () =
        // This test verifies that the iterative process correctly reaches a stable fixed point.
        use ops = new OptimizedCPUOperations() :> IF2Operations
        let peps = constructFibonacciTensor()
        let chi = 4
        let maxIter = 50

        let (finalEnv, converged) = runCTMRG peps.Data chi maxIter ops

        Assert.True(converged, sprintf "CTMRG failed to converge within %d iterations." maxIter)

        let (grownCorner, grownEdge) = ctmMove finalEnv peps.Data ops
        let envAfterOneMoreStep = rgAndUpdate finalEnv grownCorner grownEdge ops

        let isFixedPoint = areEnvironmentsEqual finalEnv envAfterOneMoreStep
        Assert.True(isFixedPoint, "The final environment is not a true fixed point.")

        Assert.False(finalEnv.C.IsZero(), "The final corner matrix should not be zero.")

    [<Fact>]
    let ``CTMRG produces exact environment for the Toric Code model`` () =
        // This is the most rigorous test. It checks if the engine reproduces the
        // analytically known exact environment for the Toric Code model, proving its
        // physical correctness.
        use ops = new OptimizedCPUOperations() :> IF2Operations

        // 1. Construct the PEPS tensor for the Toric Code ground state on a hexagonal lattice.
        // It's a rank-4 tensor (phys, v1, v2, v3). Physical dim is 2 (qubit on edges).
        // Virtual dim is 2 (Z2 charge/flux).
        let physDim = 2
        let bondDim = 2
        let toricPEPS = Tensor4F2(physDim, bondDim, bondDim, bondDim)
        // The tensor is non-zero only if the sum (XOR) of virtual legs equals the physical leg.
        for p in 0..1 do
            for v1 in 0..1 do
                for v2 in 0..1 do
                    for v3 in 0..1 do
                        if (v1 + v2 + v3) % 2 = p then
                            toricPEPS.[p, v1, v2, v3] <- F2.One

        // 2. Define the theoretical exact environment for the Toric Code.
        // It is a maximally mixed state, represented by an equal superposition.
        // For C, it's a matrix with all elements equal to 1.
        // For T, it's a tensor with all elements equal to 1.
        let chi = 2
        let exactC = MatrixF2(chi, chi, fun _ _ -> F2.One)
        let exactT = Tensor3F2(chi, physDim, chi, fun _ _ _ -> F2.One)
        let exactEnv = { C = exactC; T = exactT; Chi = chi; History = Set.empty }

        // 3. Run the CTMRG algorithm. It should converge in 1 step.
        let maxIter = 5
        let (finalEnv, converged) = runCTMRG toricPEPS chi maxIter ops

        // 4. Assert that the simulation converged and the result is bit-perfect equal to the exact solution.
        Assert.True(converged, "CTMRG for the Toric Code must converge.")
        Assert.True(areEnvironmentsEqual exactEnv finalEnv, "The final environment does not match the exact theoretical solution for the Toric Code.")