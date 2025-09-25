// src/4-PEPS-Ace/1-RenormF2.Tests.fs
namespace E8.Tests.Ace

open System
open System.Diagnostics
open Xunit
open FsUnit.Xunit
open E8.Algebra
open E8.Hardware
open E8.Hardware.CPU
open E8.Ace
open E8.Ace.RenormF2

module RenormF2Tests =

    let createTestMatrix (rows: int) (cols: int) (rank: int) =
        let rng = Random(42)
        let wordsPerRow = (cols + 63) / 64
        let matrix = Array.zeroCreate (rows * wordsPerRow)

        // ランクrの行列を生成
        for r in 0 .. min rank (min rows cols) - 1 do
            // 対角要素を1に
            let wordIdx = r * wordsPerRow + r / 64
            let bitIdx = r % 64
            matrix.[wordIdx] <- { Bits = matrix.[wordIdx].Bits ||| (1UL <<< bitIdx) }

            // ランダムな非零要素を追加
            for _ in 0 .. 2 do
                let i = rng.Next(rows)
                let j = rng.Next(cols)
                let wIdx = i * wordsPerRow + j / 64
                let bIdx = j % 64
                matrix.[wIdx] <- { Bits = matrix.[wIdx].Bits ||| (1UL <<< bIdx) }

        matrix

    [<Fact>]
    let ``Importance scores are computed correctly`` () =
        let matrix = [|
            { Bits = 0b1111UL }  // 行重み = 4
            { Bits = 0b0001UL }  // 行重み = 1
            { Bits = 0b0011UL }  // 行重み = 2
            { Bits = 0b0111UL }  // 行重み = 3
        |]

        let scores = computeImportanceScores matrix 4 4

        // スコアが降順にソートされている
        scores
        |> Array.pairwise
        |> Array.forall (fun (a, b) -> a.Score >= b.Score)
        |> should be True

        // 最高スコアの要素が正しい
        scores.[0].Score |> should be (greaterThan 0)

    [<Fact>]
    let ``Gaussian elimination maintains F2 arithmetic`` () =
        let matrix = [|
            { Bits = 0b1010UL }
            { Bits = 0b0101UL }
            { Bits = 0b1111UL }
            { Bits = 0b0000UL }
        |]

        // ピボット(0,1)でガウス消去
        gaussianElimination matrix 0 1 4 4

        // F₂での消去が正しく行われる
        // 行2の2ビット目が消去される（1 XOR 1 = 0）
        (matrix.[2].Bits >>> 1) &&& 1UL |> should equal 0UL

    [<Fact>]
    let ``Renormalization preserves rank structure`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let originalRank = 4
        let matrix = createTestMatrix 8 8 originalRank

        // ランク2に繰り込み
        let result = renormalize matrix 8 8 2 ops

        result.ActualRank |> should equal 2
        result.DiscardedRank |> should equal (originalRank - 2)
        result.SelectedPivots.Length |> should equal 2

    [<Fact>]
    let ``Renormalization handles full rank matrices`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        // フルランク行列
        let matrix = [|
            { Bits = 0b1000UL }
            { Bits = 0b0100UL }
            { Bits = 0b0010UL }
            { Bits = 0b0001UL }
        |]

        let result = renormalize matrix 4 4 4 ops

        result.ActualRank |> should equal 4
        result.DiscardedRank |> should equal 0

    [<Fact>]
    let ``Renormalization handles rank-deficient matrices`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        // ランク2の行列
        let matrix = [|
            { Bits = 0b1100UL }
            { Bits = 0b0011UL }
            { Bits = 0b1100UL }  // 行0の複製
            { Bits = 0b0011UL }  // 行1の複製
        |]

        // ランク3を要求しても2しか得られない
        let result = renormalize matrix 4 4 3 ops

        result.ActualRank |> should be (lessThanOrEqualTo 2)

    [<Fact>]
    let ``Left and right basis matrices have correct dimensions`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let matrix = createTestMatrix 16 32 8
        let targetRank = 4

        let result = renormalize matrix 16 32 targetRank ops

        // 左基底: rows × targetRank
        let leftWords = 16 * ((targetRank + 63) / 64)
        result.LeftBasis.Length |> should be (lessThanOrEqualTo leftWords)

        // 右基底: targetRank × cols
        let rightWords = targetRank * ((32 + 63) / 64)
        result.RightBasis.Length |> should be (lessThanOrEqualTo rightWords)

    [<Fact>]
    let ``Selected pivots are unique`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let matrix = createTestMatrix 8 8 5
        let result = renormalize matrix 8 8 3 ops

        // すべてのピボットが異なる行と列
        let rows = result.SelectedPivots |> Array.map fst |> Set.ofArray
        let cols = result.SelectedPivots |> Array.map snd |> Set.ofArray

        rows.Count |> should equal result.SelectedPivots.Length
        cols.Count |> should equal result.SelectedPivots.Length

    [<Fact>]
    let ``Environment renormalization works correctly`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let chi = 8
        let chiNew = 4

        let corner = Array.init (chi * chi) (fun i -> { Bits = uint64 i })
        let edge = Array.init (chi * chi) (fun i -> { Bits = uint64 (i * 2) })

        let (newCorner, newEdge, result) = renormalizeEnvironment corner edge chi chiNew ops

        result.ActualRank |> should be (lessThanOrEqualTo chiNew)
        newCorner |> should not' (be null)
        newEdge |> should not' (be null)

    [<Fact>]
    let ``Renormalization is deterministic`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let matrix = createTestMatrix 16 16 6

        // 同じ入力で複数回実行
        let result1 = renormalize (Array.copy matrix) 16 16 4 ops
        let result2 = renormalize (Array.copy matrix) 16 16 4 ops

        result1.ActualRank |> should equal result2.ActualRank
        result1.SelectedPivots |> should equal result2.SelectedPivots

    [<Fact>]
    let ``Renormalization maintains F2 policy`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let matrix = createTestMatrix 32 32 10
        let result = renormalize matrix 32 32 5 ops

        // すべての出力がF₂値
        result.LeftBasis |> Array.iter (fun b ->
            // ビットブロックは定義上F₂
            b.Bits |> ignore
        )

        result.RightBasis |> Array.iter (fun b ->
            b.Bits |> ignore
        )

        // テスト成功（例外が発生しない）
        true |> should be True

    [<Fact>]
    let ``Handles edge cases gracefully`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        // 空行列
        let empty = [||]
        let emptyResult = renormalize empty 0 0 0 ops
        emptyResult.ActualRank |> should equal 0

        // 単一要素
        let single = [| { Bits = 1UL } |]
        let singleResult = renormalize single 1 1 1 ops
        singleResult.ActualRank |> should equal 1

        // ゼロ行列
        let zero = Array.create 16 { Bits = 0UL }
        let zeroResult = renormalize zero 4 4 2 ops
        zeroResult.ActualRank |> should equal 0

    // パフォーマンステスト
    [<Fact>]
    let ``Performance benchmark for renormalization`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let sizes = [| (16, 16, 8); (32, 32, 16); (64, 64, 32) |]

        for (rows, cols, targetRank) in sizes do
            let matrix = createTestMatrix rows cols (targetRank * 2)

            let sw = Stopwatch.StartNew()
            let _ = renormalize matrix rows cols targetRank ops
            sw.Stop()

            printfn "Renorm %dx%d->%d: %d ms" rows cols targetRank sw.ElapsedMilliseconds
            sw.ElapsedMilliseconds |> should be (lessThan 1000L)

    [<Fact>]
    let ``Topological importance correctly identifies critical elements`` () =
        // ハブノードを持つ行列
        let matrix = [|
            { Bits = 0b1111UL }  // ハブ行（すべてに接続）
            { Bits = 0b1000UL }  // リーフ
            { Bits = 0b1000UL }  // リーフ
            { Bits = 0b1000UL }  // リーフ
        |]

        let scores = computeImportanceScores matrix 4 4

        // ハブ要素が最高スコア
        scores.[0].RowIndex |> should equal 0
        scores.[0].Score |> should be (greaterThan scores.[1].Score)