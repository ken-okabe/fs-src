// src/4-PEPS-Ace/4-TransferMatrix.Tests.fs
namespace E8.Tests.Ace

open System
open System.Diagnostics
open Xunit
open FsUnit.Xunit
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.Hardware.CPU
open E8.Ace
open E8.Ace.ACE_CTMRG // For SymmetricEnvironment
open E8.Ace.TransferMatrix

module TransferMatrixTests =

    // Helper function to create a test matrix with a specific pattern.
    let createTestMatrix (size: int) (pattern: string) =
        let wordsPerRow = (size + 63) / 64
        let matrix = Array.zeroCreate (size * wordsPerRow)

        match pattern with
        | "identity" ->
            for i in 0 .. size - 1 do
                let wordIdx = i * wordsPerRow + i / 64
                let bitIdx = i % 64
                matrix.[wordIdx] <- { Bits = matrix.[wordIdx].Bits ||| (1UL <<< bitIdx) }
        | _ -> ()
        matrix

    [<Fact>]
    let ``constructTransferMatrix produces the exact Toric Code transfer matrix`` () =
        // This is the most rigorous test for this module. It verifies that the implemented
        // tensor contraction logic correctly reproduces the analytically known transfer matrix
        // for the Toric Code model, proving its physical correctness.
        use ops = new OptimizedCPUOperations() :> IF2Operations

        // 1. Define the Toric Code PEPS and its exact environment.
        let physDim = 2
        let bondDim = 2 // chi
        let toricPEPS = Tensor4F2(physDim, bondDim, bondDim, bondDim)
        // Tensor is 1 if sum of virtual legs (v1+v2+v3) equals physical leg (p).
        for p in 0..1 do
            for v1 in 0..1 do
                for v2 in 0..1 do
                    for v3 in 0..1 do
                        if (v1 + v2 + v3) % 2 = p then
                            toricPEPS.[p, v1, v2, v3] <- F2.One

        let exactC = MatrixF2(bondDim, bondDim, fun _ _ -> F2.One) // All 1s
        let exactT = Tensor3F2(bondDim, physDim, bondDim, fun _ _ _ -> F2.One) // All 1s
        let exactEnv = { C = exactC; T = exactT; Chi = bondDim; History = Set.empty }

        // 2. Define the THEORETICAL exact transfer matrix for this environment.
        // For the Toric Code, the fixed point transfer matrix is a projector onto the equal
        // superposition state. In F2, this means it's a matrix of all 1s.
        let chiD = bondDim * bondDim
        let expectedTransferMatrix = MatrixF2(chiD, chiD, fun _ _ -> F2.One)

        // 3. Execute the function to be tested.
        let (constructedMatrixData, size) = constructTransferMatrix exactEnv toricPEPS ops
        let constructedMatrix = MatrixF2(size, size, fun i j -> { Bits = constructedMatrixData.[i * ((size+63)/64) + j/64].Bits >>> (j%64) &&& 1UL } |> F2.op_Explicit)

        // 4. Assert that the constructed matrix is bit-perfect identical to the theoretical solution.
        Assert.Equal(chiD, size)
        for i in 0..size-1 do
            for j in 0..size-1 do
                Assert.True(expectedTransferMatrix.[i, j] = constructedMatrix.[i, j],
                    sprintf "Mismatch at (%d, %d). Expected %A, Got %A" i j expectedTransferMatrix.[i, j] constructedMatrix.[i, j])

    [<Fact>]
    let ``Matrix power computation is correct`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations
        let size = 4
        let identity = createTestMatrix size "identity"

        // I^n = I
        let i2 = matrixPower identity size 2 ops
        let (isEqual, _) = ops.MatrixEquals identity i2 size
        Assert.True(isEqual)

        let i10 = matrixPower identity size 10 ops
        let (isEqual10, _) = ops.MatrixEquals identity i10 size
        Assert.True(isEqual10)