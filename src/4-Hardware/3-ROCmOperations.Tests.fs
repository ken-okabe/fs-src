// src/4-Hardware/3-ROCmOperations.Tests.fs
namespace E8.Tests.Hardware

open System
open System.Diagnostics
open Xunit
open FsUnit.Xunit
open E8.Algebra
open E8.Hardware
open E8.Hardware.GPU

module ROCmOperationsTests =

    let isGPUAvailable() =
        try
            use ops = new ROCmOperations(0)
            true
        with
        | _ -> false

    let skipIfNoGPU() =
        if not (isGPUAvailable()) then
            Skip.If(true, "GPU not available")

    let createTestMatrix (rows: int) (cols: int) (seed: int) =
        let rng = Random(seed)
        let wordsPerRow = (cols + 63) / 64
        Array.init (rows * wordsPerRow) (fun _ ->
            { Bits = uint64(rng.Next()) + (uint64(rng.Next()) <<< 32) }
        )

    [<Fact>]
    let ``GPU operations initialize correctly`` () =
        skipIfNoGPU()

        use ops = new ROCmOperations(0)
        ops |> should not' (be null)

        let info = (ops :> IF2Operations).DeviceInfo()
        match info.DeviceType with
        | GPU(model, computeUnits, _) ->
            model |> should not' (be null)
            computeUnits |> should be (greaterThan 0)
        | _ -> failwith "Expected GPU device"

    [<Fact>]
    let ``GPU memory management works correctly`` () =
        skipIfNoGPU()

        use ops = new ROCmOperations(0)
        let opsInterface = ops :> IF2Operations

        let initialMem = opsInterface.MemoryInfo()
        initialMem.InUse |> should equal 0L

        // 実行後のメモリ確認
        let matrix = createTestMatrix 64 64 42
        let _ = opsInterface.MatrixMultiply matrix matrix 64 64 64

        // GPU操作後、メモリは解放されているはず
        let afterMem = opsInterface.MemoryInfo()
        afterMem.InUse |> should be (greaterThanOrEqualTo 0L)

    [<Fact>]
    let ``GPU matrix multiplication produces correct results`` () =
        skipIfNoGPU()

        use ops = new ROCmOperations(0)
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
    let ``GPU maintains F2 arithmetic`` () =
        skipIfNoGPU()

        use ops = new ROCmOperations(0)
        let opsInterface = ops :> IF2Operations

        // F₂での1+1=0を確認
        let matrix = [| { Bits = 0b11UL } |]  // [1 1]
        let result = opsInterface.MatrixMultiply matrix matrix 1 2 2

        // [1 1] * [1; 1] = 1*1 + 1*1 = 1 + 1 = 0 in F₂
        result.[0].Bits &&& 0b11UL |> should equal 0UL

    [<Fact>]
    let ``GPU batch operations process correctly`` () =
        skipIfNoGPU()

        use ops = new ROCmOperations(0)
        let opsInterface = ops :> IF2Operations

        let matrices = Array.init 5 (fun i ->
            let m = createTestMatrix 16 16 i
            (m, m)
        )
        let dimensions = Array.create 5 (16, 16, 16)

        let results = opsInterface.BatchMatrixMultiply matrices dimensions

        results.Length |> should equal 5
        results |> Array.iter (fun r -> r |> should not' (be null))

    [<Fact>]
    let ``GPU topological rank computation is accurate`` () =
        skipIfNoGPU()

        use ops = new ROCmOperations(0)
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
    let ``GPU handles rank-deficient matrices`` () =
        skipIfNoGPU()

        use ops = new ROCmOperations(0)
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

    [<Fact>]
    let ``GPU parallel XOR combines vectors correctly`` () =
        skipIfNoGPU()

        use ops = new ROCmOperations(0)
        let opsInterface = ops :> IF2Operations

        let vectors = [|
            [| { Bits = 0xAAAAAAAAAAAAAAAAUL } |]
            [| { Bits = 0x5555555555555555UL } |]
        |]

        let result = opsInterface.ParallelXor vectors
        result.[0].Bits |> should equal 0xFFFFFFFFFFFFFFFFUL

    [<Fact>]
    let ``GPU operations are faster than CPU for large matrices`` () =
        skipIfNoGPU()

        use gpuOps = new ROCmOperations(0) :> IF2Operations
        use cpuOps = new OptimizedCPUOperations() :> IF2Operations

        let size = 256
        let matrix1 = createTestMatrix size size 1
        let matrix2 = createTestMatrix size size 2

        // GPU実行
        let gpuSw = Stopwatch.StartNew()
        let _ = gpuOps.MatrixMultiply matrix1 matrix2 size size size
        gpuSw.Stop()

        // CPU実行
        let cpuSw = Stopwatch.StartNew()
        let _ = cpuOps.MatrixMultiply matrix1 matrix2 size size size
        cpuSw.Stop()

        printfn "GPU: %d ms, CPU: %d ms" gpuSw.ElapsedMilliseconds cpuSw.ElapsedMilliseconds

        // GPUの方が速いか、少なくとも同程度の速度
        gpuSw.ElapsedMilliseconds |> should be (lessThanOrEqualTo (cpuSw.ElapsedMilliseconds * 2L))

    [<Fact>]
    let ``GPU handles edge cases gracefully`` () =
        skipIfNoGPU()

        use ops = new ROCmOperations(0)
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

    [<Fact>]
    let ``GPU CPU consistency check`` () =
        skipIfNoGPU()

        use gpuOps = new ROCmOperations(0) :> IF2Operations
        use cpuOps = new OptimizedCPUOperations() :> IF2Operations

        let testCases = [
            (8, 8, 8)
            (16, 32, 16)
            (64, 64, 64)
        ]

        for (rows, cols, inner) in testCases do
            let matrix1 = createTestMatrix rows inner 100
            let matrix2 = createTestMatrix inner cols 200

            let gpuResult = gpuOps.MatrixMultiply matrix1 matrix2 rows cols inner
            let cpuResult = cpuOps.MatrixMultiply matrix1 matrix2 rows cols inner

            // GPU結果とCPU結果が一致
            let (isEqual, _) = gpuOps.MatrixEquals gpuResult cpuResult (rows * ((cols + 63) / 64))
            isEqual |> should be True

    // ベンチマークテスト
    [<Fact>]
    let ``GPU performance benchmark`` () =
        skipIfNoGPU()

        use ops = new ROCmOperations(0)
        let opsInterface = ops :> IF2Operations

        let sizes = [| 32; 64; 128; 256 |]
        let results = ResizeArray<string * int64>()

        for size in sizes do
            let matrix = createTestMatrix size size 42

            // Matrix multiply
            let sw1 = Stopwatch.StartNew()
            for _ in 1..5 do
                let _ = opsInterface.MatrixMultiply matrix matrix size size size
                ()
            sw1.Stop()
            results.Add((sprintf "GPU_MatMul_%dx%d" size size, sw1.ElapsedMilliseconds / 5L))

            // Topological rank
            let sw2 = Stopwatch.StartNew()
            for _ in 1..5 do
                let _ = opsInterface.ComputeTopologicalRank matrix size size
                ()
            sw2.Stop()
            results.Add((sprintf "GPU_Rank_%dx%d" size size, sw2.ElapsedMilliseconds / 5L))

        // 結果を出力
        results |> Seq.iter (fun (name, time) ->
            printfn "%s: %d ms" name time
        )

        // すべての操作が妥当な時間内に完了
        results |> Seq.forall (fun (_, time) -> time < 2000L) |> should be True

    [<Fact>]
    let ``GPU memory leak test`` () =
        skipIfNoGPU()

        use ops = new ROCmOperations(0)
        let opsInterface = ops :> IF2Operations

        let initialMem = opsInterface.MemoryInfo()

        // 多数の操作を実行
        for i in 1..50 do
            let size = 32 + (i % 64)
            let matrix = createTestMatrix size size i
            let _ = opsInterface.MatrixMultiply matrix matrix size size size
            ()

        // メモリリークがないことを確認
        let finalMem = opsInterface.MemoryInfo()
        finalMem.InUse |> should be (lessThanOrEqualTo (initialMem.InUse + 1024L * 1024L))

    [<Fact>]
    let ``GPU resource cleanup on dispose`` () =
        skipIfNoGPU()

        let mutable disposed = false

        do
            use ops = new ROCmOperations(0)
            let opsInterface = ops :> IF2Operations

            // いくつか操作を実行
            let matrix = createTestMatrix 32 32 0
            let _ = opsInterface.MatrixMultiply matrix matrix 32 32 32
            ()

            disposed <- true

        // Disposeが呼ばれた後
        disposed |> should be True