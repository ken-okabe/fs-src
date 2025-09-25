// src/4-PEPS-Ace/2-ACE_CTMRG.Tests.fs
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
open E8.Ace.ACE_CTMRG

module ACE_CTMRGTests =

    let createTestPEPS (physDim: int) (bondDim: int) =
        let peps = Tensor5F2(physDim, bondDim, bondDim, bondDim, bondDim)
        // 単純なパターンで初期化
        for p in 0 .. physDim - 1 do
            for i in 0 .. bondDim - 1 do
                peps.[p, i, i, i, i] <- F2.One
        peps

    [<Fact>]
    let ``Initial environment is created correctly`` () =
        let env = {
            UniqueCorner = Array.init 4 (fun i -> { Bits = uint64 i })
            UniqueEdge = Array.init 8 (fun i -> { Bits = uint64 (i * 2) })
            Chi = 4
            History = {
                StateHashes = [||]
                CompressionRatios = [||]
                EffectiveRanks = [||]
                SpectralGaps = [||]
                Norms = [||]
            }
        }

        env.Chi |> should equal 4
        env.UniqueCorner.Length |> should equal 4
        env.UniqueEdge.Length |> should equal 8

    [<Fact>]
    let ``CTM move grows environment correctly`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let env = {
            UniqueCorner = Array.init 4 (fun i -> { Bits = uint64 i })
            UniqueEdge = Array.init 8 (fun i -> { Bits = uint64 i })
            Chi = 4
            History = {
                StateHashes = [||]
                CompressionRatios = [||]
                EffectiveRanks = [||]
                SpectralGaps = [||]
                Norms = [||]
            }
        }

        let peps = createTestPEPS 2 2

        let (grownCorner, grownEdge, grownChi) = ctmMove env peps ops

        grownChi |> should equal 8  // 4 * 2
        grownCorner |> should not' (be null)
        grownEdge |> should not' (be null)

    [<Fact>]
    let ``RG move compresses environment correctly`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let chi = 8
        let targetChi = 4

        let corner = Array.init (chi * chi / 64 + 1) (fun i -> { Bits = uint64 (i + 1) })
        let edge = Array.init (chi * chi * 2 / 64 + 1) (fun i -> { Bits = uint64 (i * 2) })

        let (newCorner, newEdge, rank, ratio) = rgMove corner edge chi targetChi ops

        rank |> should be (lessThanOrEqualTo targetChi)
        ratio |> should be (greaterThanOrEqualTo 0.0)
        ratio |> should be (lessThanOrEqualTo 1.0)

    [<Fact>]
    let ``State hash is deterministic`` () =
        let corner = [| { Bits = 42UL }; { Bits = 137UL } |]
        let edge = [| { Bits = 99UL }; { Bits = 256UL } |]

        let hash1 = computeStateHash corner edge
        let hash2 = computeStateHash corner edge

        hash1 |> should equal hash2

    [<Fact>]
    let ``State hash changes with different inputs`` () =
        let corner1 = [| { Bits = 42UL } |]
        let corner2 = [| { Bits = 43UL } |]
        let edge = [| { Bits = 99UL } |]

        let hash1 = computeStateHash corner1 edge
        let hash2 = computeStateHash corner2 edge

        hash1 |> should not' (equal hash2)

    [<Fact>]
    let ``CTMRG step executes without errors`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let env = {
            UniqueCorner = Array.init 4 (fun i -> { Bits = uint64 i })
            UniqueEdge = Array.init 8 (fun i -> { Bits = uint64 i })
            Chi = 4
            History = {
                StateHashes = [||]
                CompressionRatios = [||]
                EffectiveRanks = [||]
                SpectralGaps = [||]
                Norms = [||]
            }
        }

        let peps = createTestPEPS 2 2

        let newEnv = ctmrgStep env peps 4 ops

        newEnv.Chi |> should equal 4
        newEnv.History.StateHashes.Length |> should equal 1

    [<Fact>]
    let ``Convergence detection works for fixed point`` () =
        let history = {
            StateHashes = [| 100; 200; 300; 300; 300 |]
            CompressionRatios = [||]
            EffectiveRanks = [||]
            SpectralGaps = [||]
            Norms = [||]
        }

        hasConverged history 3 |> should be True

    [<Fact>]
    let ``Convergence detection works for cycle`` () =
        let history = {
            StateHashes = [| 100; 200; 300; 400; 300; 400 |]
            CompressionRatios = [||]
            EffectiveRanks = [||]
            SpectralGaps = [||]
            Norms = [||]
        }

        hasConverged history 4 |> should be True

    [<Fact>]
    let ``Convergence detection returns false for non-convergent`` () =
        let history = {
            StateHashes = [| 100; 200; 300; 400; 500 |]
            CompressionRatios = [||]
            EffectiveRanks = [||]
            SpectralGaps = [||]
            Norms = [||]
        }

        hasConverged history 3 |> should be False

    [<Fact>]
    let ``Full CTMRG converges or reaches max iterations`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let peps = createTestPEPS 2 2
        let config = {
            InitialChi = 2
            MaxChi = 4
            ConvergenceThreshold = 3
            MaxIterations = 10
        }

        let (finalEnv, converged) = runCTMRG peps config ops

        // 収束したか、最大反復回数に到達
        if converged then
            hasConverged finalEnv.History config.ConvergenceThreshold |> should be True
        else
            finalEnv.History.StateHashes.Length |> should equal config.MaxIterations

    [<Fact>]
    let ``Expectation value computation maintains F2`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let env = {
            UniqueCorner = [| { Bits = 0b1010UL }; { Bits = 0b0101UL } |]
            UniqueEdge = [||]
            Chi = 2
            History = {
                StateHashes = [||]
                CompressionRatios = [||]
                EffectiveRanks = [||]
                SpectralGaps = [||]
                Norms = [||]
            }
        }

        let observable = [| { Bits = 0b1100UL }; { Bits = 0b0011UL } |]

        let result = computeExpectation env observable ops

        // 結果はF2値
        result |> should satisfy (fun r -> r = F2.Zero || r = F2.One)

    [<Fact>]
    let ``History tracking works correctly`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let env = {
            UniqueCorner = Array.init 2 (fun i -> { Bits = uint64 i })
            UniqueEdge = Array.init 4 (fun i -> { Bits = uint64 i })
            Chi = 2
            History = {
                StateHashes = [||]
                CompressionRatios = [||]
                EffectiveRanks = [||]
                SpectralGaps = [||]
                Norms = [||]
            }
        }

        let peps = createTestPEPS 2 2

        // 3ステップ実行
        let env1 = ctmrgStep env peps 2 ops
        let env2 = ctmrgStep env1 peps 2 ops
        let env3 = ctmrgStep env2 peps 2 ops

        env3.History.StateHashes.Length |> should equal 3
        env3.History.CompressionRatios.Length |> should equal 3
        env3.History.EffectiveRanks.Length |> should equal 3
        env3.History.Norms.Length |> should equal 3

    // パフォーマンステスト
    [<Fact>]
    let ``CTMRG performance benchmark`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let sizes = [| (2, 2, 4); (2, 4, 8); (2, 8, 16) |]

        for (physDim, bondDim, maxChi) in sizes do
            let peps = createTestPEPS physDim bondDim
            let config = {
                InitialChi = bondDim
                MaxChi = maxChi
                ConvergenceThreshold = 3
                MaxIterations = 5
            }

            let sw = Stopwatch.StartNew()
            let _ = runCTMRG peps config ops
            sw.Stop()

            printfn "CTMRG D=%d chi=%d: %d ms" bondDim maxChi sw.ElapsedMilliseconds
            sw.ElapsedMilliseconds |> should be (lessThan 5000L)

    [<Fact>]
    let ``CTMRG maintains numerical stability in F2`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let peps = createTestPEPS 2 4
        let config = {
            InitialChi = 4
            MaxChi = 8
            ConvergenceThreshold = 3
            MaxIterations = 20
        }

        let (finalEnv, _) = runCTMRG peps config ops

        // ノルムが爆発していないことを確認
        finalEnv.History.Norms
        |> Array.forall (fun n -> n < System.UInt64.MaxValue / 2UL)
        |> should be True