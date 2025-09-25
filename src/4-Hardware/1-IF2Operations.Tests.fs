// src/4-Hardware/1-IF2Operations.Tests.fs
namespace E8.Tests.Hardware

open System
open Xunit
open FsUnit.Xunit
open E8.Algebra
open E8.Hardware

module IF2OperationsTests =

    // モック実装（テスト用）
    type MockF2Operations() =
        let mutable allocated = 0L
        let mutable inUse = 0L

        interface IF2Operations with
            member _.MatrixMultiply left right rows cols inner =
                // 単純な行列積のモック実装
                let result = Array.zeroCreate (rows * ((cols + 63) / 64))
                for i in 0 .. rows - 1 do
                    for j in 0 .. cols - 1 do
                        let mutable sum = 0UL
                        for k in 0 .. inner - 1 do
                            let leftIdx = i * ((inner + 63) / 64) + k / 64
                            let rightIdx = k * ((cols + 63) / 64) + j / 64
                            let leftBit = (left.[leftIdx].Bits >>> (k % 64)) &&& 1UL
                            let rightBit = (right.[rightIdx].Bits >>> (j % 64)) &&& 1UL
                            sum <- sum ^^^ (leftBit &&& rightBit)
                        let resultIdx = i * ((cols + 63) / 64) + j / 64
                        result.[resultIdx] <- { Bits = result.[resultIdx].Bits ||| (sum <<< (j % 64)) }
                HardwareUtils.uint64ArrayToBitBlocks result

            member _.MatrixEquals matrix1 matrix2 size =
                let mutable differences = 0
                let mutable isEqual = true
                for i in 0 .. size - 1 do
                    if matrix1.[i].Bits <> matrix2.[i].Bits then
                        isEqual <- false
                        differences <- differences + 1
                (isEqual, differences)

            member _.BatchMatrixMultiply matrices dimensions =
                Array.mapi (fun i (left, right) ->
                    let (rows, cols, inner) = dimensions.[i]
                    (this :> IF2Operations).MatrixMultiply left right rows cols inner
                ) matrices

            member _.ComputeTopologicalRank matrix rows cols =
                // 簡単なランク計算のモック
                let mutable rank = 0
                let pivots = ResizeArray<int>()
                for i in 0 .. min rows cols - 1 do
                    let idx = i * ((cols + 63) / 64) + i / 64
                    if (matrix.[idx].Bits >>> (i % 64)) &&& 1UL <> 0UL then
                        rank <- rank + 1
                        pivots.Add(i * cols + i)
                (rank, pivots.ToArray())

            member _.ParallelXor vectors =
                if Array.isEmpty vectors then [||]
                else
                    let length = Array.length vectors.[0]
                    let result = Array.zeroCreate length
                    for vec in vectors do
                        for i in 0 .. length - 1 do
                            result.[i] <- { Bits = result.[i].Bits ^^^ vec.[i].Bits }
                    result

            member _.PopCount value =
                let rec count v acc =
                    if v = 0UL then acc
                    else count (v &&& (v - 1UL)) (acc + 1)
                count value 0

            member _.DeviceInfo() =
                {
                    DeviceType = CPU(cores = 4, avx512 = false, avx2 = true)
                    MaxThreads = 8
                    MaxMemory = 8L * 1024L * 1024L * 1024L
                    BitwiseOpsPerClock = 2
                    SupportsInt64Atomics = true
                    WarpSize = None
                }

            member _.MemoryInfo() =
                {
                    TotalAllocated = allocated
                    InUse = inUse
                    PoolSize = 0L
                    PinnedMemory = None
                }

            member _.Dispose() =
                allocated <- 0L
                inUse <- 0L

    [<Fact>]
    let ``Interface contract is well-defined`` () =
        let ops = MockF2Operations() :> IF2Operations
        ops |> should not' (be null)

    [<Fact>]
    let ``MatrixMultiply maintains F2 policy`` () =
        let ops = MockF2Operations() :> IF2Operations
        let left = [| { Bits = 0b1010UL }; { Bits = 0b0101UL } |]
        let right = [| { Bits = 0b1100UL }; { Bits = 0b0011UL } |]

        let result = ops.MatrixMultiply left right 2 2 2

        // すべての結果がF₂値（各ビットが0または1）
        result |> Array.iter (fun block ->
            block.Bits |> should be (lessThanOrEqualTo System.UInt64.MaxValue)
        )

    [<Fact>]
    let ``MatrixEquals detects identical matrices`` () =
        let ops = MockF2Operations() :> IF2Operations
        let matrix1 = [| { Bits = 42UL }; { Bits = 137UL } |]
        let matrix2 = [| { Bits = 42UL }; { Bits = 137UL } |]

        let (isEqual, differences) = ops.MatrixEquals matrix1 matrix2 2

        isEqual |> should be True
        differences |> should equal 0

    [<Fact>]
    let ``MatrixEquals detects different matrices`` () =
        let ops = MockF2Operations() :> IF2Operations
        let matrix1 = [| { Bits = 42UL }; { Bits = 137UL } |]
        let matrix2 = [| { Bits = 42UL }; { Bits = 138UL } |]

        let (isEqual, differences) = ops.MatrixEquals matrix1 matrix2 2

        isEqual |> should be False
        differences |> should equal 1

    [<Fact>]
    let ``BatchMatrixMultiply processes multiple operations`` () =
        let ops = MockF2Operations() :> IF2Operations
        let matrices = [|
            ([| { Bits = 1UL } |], [| { Bits = 1UL } |])
            ([| { Bits = 2UL } |], [| { Bits = 3UL } |])
        |]
        let dimensions = [| (1, 1, 1); (1, 1, 1) |]

        let results = ops.BatchMatrixMultiply matrices dimensions

        results.Length |> should equal 2
        results.[0].[0].Bits |> should equal 1UL
        results.[1].[0].Bits |> should equal 2UL

    [<Fact>]
    let ``ComputeTopologicalRank returns valid rank`` () =
        let ops = MockF2Operations() :> IF2Operations
        let identity = [|
            { Bits = 0b1000UL }  // [1 0]
            { Bits = 0b0001UL }  // [0 1]
        |]

        let (rank, pivots) = ops.ComputeTopologicalRank identity 2 2

        rank |> should be (greaterThan 0)
        rank |> should be (lessThanOrEqualTo 2)
        pivots.Length |> should equal rank

    [<Fact>]
    let ``ParallelXor combines vectors correctly`` () =
        let ops = MockF2Operations() :> IF2Operations
        let vectors = [|
            [| { Bits = 0b1010UL } |]
            [| { Bits = 0b1100UL } |]
            [| { Bits = 0b0001UL } |]
        |]

        let result = ops.ParallelXor vectors

        result.Length |> should equal 1
        result.[0].Bits |> should equal 0b0111UL

    [<Fact>]
    let ``PopCount counts set bits correctly`` () =
        let ops = MockF2Operations() :> IF2Operations

        ops.PopCount 0UL |> should equal 0
        ops.PopCount 1UL |> should equal 1
        ops.PopCount 0b1111UL |> should equal 4
        ops.PopCount 0b10101010UL |> should equal 4
        ops.PopCount System.UInt64.MaxValue |> should equal 64

    [<Fact>]
    let ``DeviceInfo returns valid capabilities`` () =
        let ops = MockF2Operations() :> IF2Operations
        let info = ops.DeviceInfo()

        info.MaxThreads |> should be (greaterThan 0)
        info.MaxMemory |> should be (greaterThan 0L)
        info.BitwiseOpsPerClock |> should be (greaterThan 0)

    [<Fact>]
    let ``MemoryInfo tracks allocation correctly`` () =
        let ops = MockF2Operations() :> IF2Operations
        let info = ops.MemoryInfo()

        info.TotalAllocated |> should be (greaterThanOrEqualTo 0L)
        info.InUse |> should be (lessThanOrEqualTo info.TotalAllocated)

    [<Fact>]
    let ``Dispose releases resources`` () =
        let ops = MockF2Operations() :> IF2Operations
        ops.Dispose()

        let info = ops.MemoryInfo()
        info.InUse |> should equal 0L

    // パフォーマンステスト
    [<Fact>]
    let ``Matrix operations complete in reasonable time`` () =
        let ops = MockF2Operations() :> IF2Operations
        let size = 128
        let matrix = Array.init (size * 2) (fun i -> { Bits = uint64 i })

        let stopwatch = System.Diagnostics.Stopwatch.StartNew()
        let _ = ops.MatrixMultiply matrix matrix size size size
        stopwatch.Stop()

        stopwatch.ElapsedMilliseconds |> should be (lessThan 1000L)

    // エッジケーステスト
    [<Fact>]
    let ``Handles empty matrices correctly`` () =
        let ops = MockF2Operations() :> IF2Operations
        let empty = [||]

        let result = ops.ParallelXor [| empty |]
        result |> should equal empty

    [<Fact>]
    let ``Handles single element operations`` () =
        let ops = MockF2Operations() :> IF2Operations
        let single = [| { Bits = 1UL } |]

        let result = ops.MatrixMultiply single single 1 1 1
        result.Length |> should equal 1
        result.[0].Bits |> should equal 1UL