// src/4-PEPS-Ace/3-ProjectorSpectral.Tests.fs
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
open E8.Ace.ProjectorSpectral

module ProjectorSpectralTests =

    let createProjectorMatrix (size: int) =
        // 射影行列を作成：P² = P
        let wordsPerRow = (size + 63) / 64
        let matrix = Array.zeroCreate (size * wordsPerRow)

        // 簡単な射影行列：最初の半分の対角要素を1に
        for i in 0 .. size / 2 - 1 do
            let wordIdx = i * wordsPerRow + i / 64
            let bitIdx = i % 64
            matrix.[wordIdx] <- { Bits = matrix.[wordIdx].Bits ||| (1UL <<< bitIdx) }

        matrix

    let createNonProjectorMatrix (size: int) =
        // 非射影行列を作成
        let wordsPerRow = (size + 63) / 64
        let matrix = Array.zeroCreate (size * wordsPerRow)

        // 巡回シフト行列
        for i in 0 .. size - 1 do
            let j = (i + 1) % size
            let wordIdx = i * wordsPerRow + j / 64
            let bitIdx = j % 64
            matrix.[wordIdx] <- { Bits = matrix.[wordIdx].Bits ||| (1UL <<< bitIdx) }

        matrix

    let createTestEnvironment (chi: int) =
        {
            ACE_CTMRG.UniqueCorner = Array.init chi (fun i -> { Bits = uint64 (i + 1) })
            ACE_CTMRG.UniqueEdge = Array.init (chi * 2) (fun i -> { Bits = uint64 (i % 3) })
            ACE_CTMRG.Chi = chi
            ACE_CTMRG.History = {
                StateHashes = [||]
                CompressionRatios = [||]
                EffectiveRanks = [||]
                SpectralGaps = [||]
                Norms = [||]
            }
        }

    [<Fact>]
    let ``Transfer matrix construction works correctly`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let env = createTestEnvironment 4
        let peps = Tensor5F2(2, 2, 2, 2, 2)

        let hTransfer = constructTransferMatrix env peps (Horizontal 2) ops
        let vTransfer = constructTransferMatrix env peps (Vertical 2) ops
        let cTransfer = constructTransferMatrix env peps (Corner 2) ops

        hTransfer |> should not' (be null)
        vTransfer |> should not' (be null)
        cTransfer |> should not' (be null)

    [<Fact>]
    let ``Projector hypothesis correctly identifies projectors`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 8
        let projector = createProjectorMatrix size

        let result = verifyProjectorHypothesis projector size ops

        result.IsProjector |> should be True
        result.DifferenceCount |> should equal 0
        result.Rank.IsSome |> should be True
        result.Rank.Value |> should equal (size / 2)

    [<Fact>]
    let ``Projector hypothesis correctly identifies non-projectors`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 8
        let nonProjector = createNonProjectorMatrix size

        let result = verifyProjectorHypothesis nonProjector size ops

        result.IsProjector |> should be False
        result.DifferenceCount |> should be (greaterThan 0)

    [<Fact>]
    let ``Trace calculation is correct`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 4
        let wordsPerRow = (size + 63) / 64
        let identity = Array.zeroCreate (size * wordsPerRow)

        // 単位行列
        for i in 0 .. size - 1 do
            let wordIdx = i * wordsPerRow + i / 64
            let bitIdx = i % 64
            identity.[wordIdx] <- { Bits = identity.[wordIdx].Bits ||| (1UL <<< bitIdx) }

        let result = verifyProjectorHypothesis identity size ops

        result.Trace |> should equal size
        result.IsProjector |> should be True  // I² = I

    [<Fact>]
    let ``Spectral info for projector is correct`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 8
        let projector = createProjectorMatrix size

        let info = computeSpectralInfo projector size true ops

        info.LeadingEigenvalue |> should equal F2.One
        info.SpectralGap |> should equal F2.One
        info.MinimalPolynomialDegree |> should equal 2

    [<Fact>]
    let ``Spectral info for non-projector is computed`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 8
        let matrix = createNonProjectorMatrix size

        let info = computeSpectralInfo matrix size false ops

        info.LeadingEigenvalue |> should satisfy (fun v -> v = F2.Zero || v = F2.One)
        info.MinimalPolynomialDegree |> should be (greaterThan 0)

    [<Fact>]
    let ``Minimal polynomial degree is bounded correctly`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 16
        let matrix = createNonProjectorMatrix size

        let degree = computeMinimalPolynomial matrix size ops

        degree |> should be (greaterThan 0)
        degree |> should be (lessThanOrEqualTo size)

    [<Fact>]
    let ``Full spectral analysis executes correctly`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let env = createTestEnvironment 4
        let peps = Tensor5F2(2, 2, 2, 2, 2)

        let result = analyzeEnvironmentSpectrum env peps ops

        result.ProjectorStatus.IsSome |> should be True
        result.SpectralInfo.IsSome |> should be True
        result.MinimalPolynomialDegree |> should be (greaterThan 0)

    [<Fact>]
    let ``Topological order detection works`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let env = createTestEnvironment 4
        let peps = Tensor5F2(2, 2, 2, 2, 2)

        // 環境を射影行列になるように調整
        let size = env.Chi * peps.D2
        env.UniqueEdge <- createProjectorMatrix size

        let result = analyzeEnvironmentSpectrum env peps ops

        // トポロジカル秩序の条件をチェック
        if result.IsTopologicallyOrdered then
            result.ProjectorStatus.Value.IsProjector |> should be True
            result.SpectralInfo.Value.SpectralGap |> should equal F2.One

    [<Fact>]
    let ``Zero matrix is handled correctly`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 8
        let zeroMatrix = Array.create (size * ((size + 63) / 64)) { Bits = 0UL }

        let result = verifyProjectorHypothesis zeroMatrix size ops

        result.IsProjector |> should be True  // 0² = 0
        result.Rank.Value |> should equal 0
        result.Trace |> should equal 0

    [<Fact>]
    let ``Identity matrix is a projector`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 8
        let wordsPerRow = (size + 63) / 64
        let identity = Array.zeroCreate (size * wordsPerRow)

        for i in 0 .. size - 1 do
            let wordIdx = i * wordsPerRow + i / 64
            let bitIdx = i % 64
            identity.[wordIdx] <- { Bits = identity.[wordIdx].Bits ||| (1UL <<< bitIdx) }

        let result = verifyProjectorHypothesis identity size ops

        result.IsProjector |> should be True
        result.Rank.Value |> should equal size
        result.Trace |> should equal size

    // パフォーマンステスト
    [<Fact>]
    let ``Spectral analysis performance benchmark`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let sizes = [| 4; 8; 16; 32 |]

        for chi in sizes do
            let env = createTestEnvironment chi
            let peps = Tensor5F2(2, 2, 2, 2, 2)

            let sw = Stopwatch.StartNew()
            let _ = analyzeEnvironmentSpectrum env peps ops
            sw.Stop()

            printfn "Spectral analysis chi=%d: %d ms" chi sw.ElapsedMilliseconds
            sw.ElapsedMilliseconds |> should be (lessThan 2000L)

    [<Fact>]
    let ``Projector verification is deterministic`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 16
        let matrix = createProjectorMatrix size

        // 複数回実行
        let results =
            [| for _ in 1..10 -> verifyProjectorHypothesis matrix size ops |]

        // すべて同じ結果
        results |> Array.forall (fun r -> r.IsProjector = results.[0].IsProjector) |> should be True
        results |> Array.forall (fun r -> r.Rank = results.[0].Rank) |> should be True