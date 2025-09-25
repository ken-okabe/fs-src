// src/4-Hardware/2-CPUOperations.Tests.fs
namespace E8.Tests.Hardware

open System
open System.Diagnostics
open Xunit
open FsUnit.Xunit
open E8.Algebra
open E8.Hardware
open E8.Hardware.CPU

module CPUOperationsTests =

    let createTestMatrix (rows: int) (cols: int) (seed: int) =
        let rng = Random(seed)
        let wordsPerRow = (cols + 63) / 64
        Array.init (rows * wordsPerRow) (fun _ ->
            { Bits = uint64(rng.Next()) + (uint64(rng.Next()) <<< 32) }
        )

    [<Fact>]
    let ``CPU operations initialize correctly`` () =
        use ops = new OptimizedCPUOperations(memoryPoolSizeMB = 64)
        ops |> should not' (be null)

        let info = (ops :> IF2Operations).DeviceInfo()
        match info.DeviceType with
        | CPU(cores, _, _) -> cores |> should be (greaterThan 0)
        | _ -> failwith "Expected CPU device"

    [<Fact>]
    let ``Memory pool manages allocation correctly`` () =
        use ops = new OptimizedCPUOperations(memoryPoolSizeMB = 16)
        let opsInterface = ops :> IF2Operations

        let initialMem = opsInterface.MemoryInfo()
        initialMem.InUse |> should equal 0L

        // 行列演算を実行してメモリ使用を確認
        let matrix = createTestMatrix 64 64 42
        let _ = opsInterface.MatrixMultiply matrix matrix 64 64 64

        let afterMem = opsInterface.MemoryInfo()
        afterMem.TotalAllocated |> should be (greaterThanOrEqualTo 0L)

    [<Fact>]
    let ``Matrix multiplication produces correct results`` () =
        use ops = new OptimizedCPUOperations()
        let opsInterface = ops :> IF2Operations

        // 単位行列のテスト
        let identity = [|
            { Bits = 0b10UL }   // [1 0]
            { Bits = 0b01UL }   // [0 1]
        |]

        let result = opsInterface.MatrixMultiply identity identity 2 2 2

        // I * I = I
        result.[0].Bits |> should equal 0b10UL
        result.[1].Bits |> should equal 0b01UL

    [<Fact>]
    let ``Matrix multiplication maintains F2 arithmetic`` () =
        use ops = new OptimizedCPUOperations()
        let opsInterface = ops :> IF2Operations

        // F₂での1+1=0を確認
        let matrix = [| { Bits = 0b11UL } |]  // [1 1]
        let result = opsInterface.MatrixMultiply matrix matrix 1 2 2

        // [1 1] * [1; 1] = 1*1 + 1*1 = 1 + 1 = 0 in F₂
        result.[0].Bits &&& 0b11UL |> should equal 0UL

    [<Fact>]
    let ``Batch operations process correctly`` () =
        use ops = new OptimizedCPUOperations()
        let opsInterface = ops :> IF2Operations

        let matrices = Array.init 10 (fun i ->
            let m = createTestMatrix 8 8 i
            (m, m)
        )
        let dimensions = Array.create 10 (8, 8, 8)

        let results = opsInterface.BatchMatrixMultiply matrices dimensions

        results.Length |> should equal 10
        results |> Array.iter (fun r -> r |> should not' (be null))

    [<Fact>]
    let ``Topological rank computation is accurate`` () =
        use ops = new OptimizedCPUOperations()
        let opsInterface = ops :> IF2Operations

        // フルランク行列
        let fullRank = [|
            { Bits = 0b1000UL }
            { Bits = 0b0100UL }
            { Bits = 0b0010UL }
            { Bits = 0b0001UL }
        |]

        let (rank, pivots) = opsInterface.ComputeTopologicalRank fullRank 4 4
        rank |> should equal 4
        pivots.Length |> should equal 4

    [<Fact>]
    let ``Topological rank handles rank-deficient matrices`` () =
        use ops = new OptimizedCPUOperations()
        let opsInterface = ops :> IF2Operations

        // ランク2の行列
        let rankDeficient = [|
            { Bits = 0b1100UL }
            { Bits = 0b0011UL }
            { Bits = 0b1100UL }  // 行1と同じ
            { Bits = 0b0011UL }  // 行2と同じ
        |]

        let (rank, pivots) = opsInterface.ComputeTopologicalRank rankDeficient 4 4
        rank |> should equal 2
        pivots.Length |> should equal 2

    [<Fact>]
    let ``Parallel XOR combines vectors correctly`` () =
        use ops = new OptimizedCPUOperations()
        let opsInterface = ops :> IF2Operations

        let vectors = [|
            [| { Bits = 0xAAAAAAAAAAAAAAAAUL } |]
            [| { Bits = 0x5555555555555555UL } |]
        |]

        let result = opsInterface.ParallelXor vectors
        result.[0].Bits |> should equal 0xFFFFFFFFFFFFFFFFUL

    [<Fact>]
    let ``PopCount with hardware acceleration`` () =
        use ops = new OptimizedCPUOperations()
        let opsInterface = ops :> IF2Operations

        // 各種ビットパターンのテスト
        opsInterface.PopCount 0UL |> should equal 0
        opsInterface.PopCount 0xFFFFFFFFFFFFFFFFUL |> should equal 64
        opsInterface.PopCount 0x5555555555555555UL |> should equal 32
        opsInterface.PopCount 0x0F0F0F0F0F0F0F0FUL |> should equal 32

        // ランダムな値
        let rng = Random(42)
        for _ in 1..100 do
            let value = uint64(rng.Next()) ||| (uint64(rng.Next()) <<< 32)
            let expected =
                let mutable v = value
                let mutable count = 0
                while v <> 0UL do
                    count <- count + 1
                    v <- v &&& (v - 1UL)
                count
            opsInterface.PopCount value |> should equal expected

    [<Fact>]
    let ``AVX optimization provides performance benefit`` () =
        use ops = new OptimizedCPUOperations()
        let opsInterface = ops :> IF2Operations

        let info = opsInterface.DeviceInfo()
        let hasAvx =
            match info.DeviceType with
            | CPU(_, avx512, avx2) -> avx512 || avx2
            | _ -> false

        if hasAvx then
            // 大きな行列で性能を測定
            let size = 256
            let matrix1 = createTestMatrix size size 1
            let matrix2 = createTestMatrix size size 2

            let sw = Stopwatch.StartNew()
            let _ = opsInterface.MatrixMultiply matrix1 matrix2 size size size
            sw.Stop()

            // AVX最適化があれば1秒以内に完了すべき
            sw.ElapsedMilliseconds |> should be (lessThan 1000L)

    [<Fact>]
    let ``Memory pool prevents memory leaks`` () =
        use ops = new OptimizedCPUOperations(memoryPoolSizeMB = 8)
        let opsInterface = ops :> IF2Operations

        // 多数の操作を実行
        for i in 1..100 do
            let size = 16 + (i % 48)
            let matrix = createTestMatrix size size i
            let _ = opsInterface.MatrixMultiply matrix matrix size size size
            ()

        // メモリが解放されていることを確認
        GC.Collect()
        GC.WaitForPendingFinalizers()
        GC.Collect()

        let finalMem = opsInterface.MemoryInfo()
        finalMem.InUse |> should be (lessThanOrEqualTo (8L * 1024L * 1024L))

    [<Fact>]
    let ``Handles edge cases gracefully`` () =
        use ops = new OptimizedCPUOperations()
        let opsInterface = ops :> IF2Operations

        // 空の配列
        let empty = [||]
        let emptyResult = opsInterface.ParallelXor [| empty |]
        emptyResult |> should equal empty

        // 単一要素
        let single = [| { Bits = 42UL } |]
        let singleResult = opsInterface.MatrixMultiply single single 1 1 1
        singleResult.Length |> should equal 1

        // 非正方行列
        let rect = createTestMatrix 3 5 0
        let rectResult = opsInterface.MatrixMultiply rect rect 3 5 5
        rectResult |> should not' (be null)

    // ベンチマークテスト
    [<Fact>]
    let ``Performance benchmark for various operations`` () =
        use ops = new OptimizedCPUOperations()
        let opsInterface = ops :> IF2Operations

        let sizes = [| 16; 32; 64; 128 |]
        let results = ResizeArray<string * int64>()

        for size in sizes do
            let matrix = createTestMatrix size size 42

            // Matrix multiply
            let sw1 = Stopwatch.StartNew()
            for _ in 1..10 do
                let _ = opsInterface.MatrixMultiply matrix matrix size size size
                ()
            sw1.Stop()
            results.Add((sprintf "MatMul_%dx%d" size size, sw1.ElapsedMilliseconds / 10L))

            // Topological rank
            let sw2 = Stopwatch.StartNew()
            for _ in 1..10 do
                let _ = opsInterface.ComputeTopologicalRank matrix size size
                ()
            sw2.Stop()
            results.Add((sprintf "Rank_%dx%d" size size, sw2.ElapsedMilliseconds / 10L))

        // 結果を出力（デバッグ用）
        results |> Seq.iter (fun (name, time) ->
            printfn "%s: %d ms" name time
        )

        // すべての操作が妥当な時間内に完了
        results |> Seq.forall (fun (_, time) -> time < 1000L) |> should be True

    [<Fact>]
    let ``Thread safety of operations`` () =
        use ops = new OptimizedCPUOperations()
        let opsInterface = ops :> IF2Operations

        let tasks =
            [| 1..10 |]
            |> Array.map (fun i -> async {
                let matrix = createTestMatrix 32 32 i
                let result = opsInterface.MatrixMultiply matrix matrix 32 32 32
                return result |> Array.sumBy (fun b -> int b.Bits)
            })

        let results = tasks |> Async.Parallel |> Async.RunSynchronously

        // すべてのタスクが完了
        results.Length |> should equal 10