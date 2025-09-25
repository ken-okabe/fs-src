// src/4-Hardware/2-CPUOperations.fs
namespace E8.Hardware.CPU

open System
open System.Runtime.Intrinsics
open System.Runtime.Intrinsics.X86
open System.Runtime.CompilerServices
open System.Threading.Tasks
open System.Collections.Concurrent
open E8.Algebra
open E8.Hardware

/// メモリプール管理
module MemoryPool =

    type PooledArray = {
        Data: uint64[]
        Size: int
        InUse: bool ref
    }

    type MemoryPool(maxSizeMB: int) =
        let maxBytes = int64 maxSizeMB * 1024L * 1024L
        let pools = ConcurrentDictionary<int, ConcurrentBag<PooledArray>>()
        let mutable totalAllocated = 0L
        let mutable currentInUse = 0L

        member _.Rent(size: int) : uint64[] =
            let sizeClass =
                if size <= 64 then 64
                elif size <= 256 then 256
                elif size <= 1024 then 1024
                elif size <= 4096 then 4096
                else size

            let bag = pools.GetOrAdd(sizeClass, fun _ -> ConcurrentBag<PooledArray>())

            let mutable found = None
            let mutable item = Unchecked.defaultof<PooledArray>

            while found.IsNone && bag.TryTake(&item) do
                if not !item.InUse then
                    item.InUse := true
                    found <- Some item

            match found with
            | Some pooled ->
                Interlocked.Add(&currentInUse, int64 pooled.Size) |> ignore
                pooled.Data
            | None ->
                let newArray = Array.zeroCreate sizeClass
                let pooled = { Data = newArray; Size = sizeClass; InUse = ref true }
                Interlocked.Add(&totalAllocated, int64 sizeClass * 8L) |> ignore
                Interlocked.Add(&currentInUse, int64 sizeClass) |> ignore
                newArray

        member _.Return(array: uint64[]) =
            let size = array.Length
            let sizeClass =
                if size <= 64 then 64
                elif size <= 256 then 256
                elif size <= 1024 then 1024
                elif size <= 4096 then 4096
                else size

            let bag = pools.GetOrAdd(sizeClass, fun _ -> ConcurrentBag<PooledArray>())

            Array.Clear(array, 0, array.Length)
            let pooled = { Data = array; Size = size; InUse = ref false }
            bag.Add(pooled)
            Interlocked.Add(&currentInUse, -int64 size) |> ignore

        member _.TotalAllocated = totalAllocated
        member _.CurrentInUse = currentInUse
        member _.Clear() =
            pools.Clear()
            totalAllocated <- 0L
            currentInUse <- 0L

/// AVX512/AVX2最適化CPU実装
type OptimizedCPUOperations(?memoryPoolSizeMB: int) =

    let poolSize = defaultArg memoryPoolSizeMB 256
    let memoryPool = MemoryPool(poolSize)

    // ハードウェア能力検出
    let hasAVX512 = Avx512F.IsSupported && Avx512BW.IsSupported
    let hasAVX2 = Avx2.IsSupported
    let hasPOPCNT = Popcnt.X64.IsSupported
    let parallelism = Environment.ProcessorCount

    let vectorSize =
        if hasAVX512 then 8  // 512ビット = 8 * uint64
        elif hasAVX2 then 4   // 256ビット = 4 * uint64
        else 1                // スカラー

    /// ポピュレーションカウント（1ビットの数を数える）
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline popcount (value: uint64) =
        if hasPOPCNT then
            int(Popcnt.X64.PopCount(value))
        else
            // ソフトウェアフォールバック
            let mutable v = value
            let mutable count = 0
            while v <> 0UL do
                count <- count + 1
                v <- v &&& (v - 1UL)
            count

    /// AVX512を使用したXOR演算
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline xorAVX512 (a: uint64[]) (b: uint64[]) (result: uint64[]) (length: int) =
        let mutable i = 0

        // ベクトル化された部分
        while i + 8 <= length do
            let va = Vector512.LoadUnsafe(&a.[i])
            let vb = Vector512.LoadUnsafe(&b.[i])
            let vr = Avx512F.Xor(va, vb)
            vr.StoreUnsafe(&result.[i])
            i <- i + 8

        // 残りの要素
        while i < length do
            result.[i] <- a.[i] ^^^ b.[i]
            i <- i + 1

    /// AVX2を使用したXOR演算
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline xorAVX2 (a: uint64[]) (b: uint64[]) (result: uint64[]) (length: int) =
        let mutable i = 0

        // ベクトル化された部分
        while i + 4 <= length do
            let va = Vector256.LoadUnsafe(&a.[i])
            let vb = Vector256.LoadUnsafe(&b.[i])
            let vr = Avx2.Xor(va, vb)
            vr.StoreUnsafe(&result.[i])
            i <- i + 4

        // 残りの要素
        while i < length do
            result.[i] <- a.[i] ^^^ b.[i]
            i <- i + 1

    /// スカラーXOR演算
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline xorScalar (a: uint64[]) (b: uint64[]) (result: uint64[]) (length: int) =
        for i in 0 .. length - 1 do
            result.[i] <- a.[i] ^^^ b.[i]

    /// 最適なXOR実装を選択
    let xorOptimal =
        if hasAVX512 then xorAVX512
        elif hasAVX2 then xorAVX2
        else xorScalar

    /// ブロック行列積（キャッシュ効率を考慮）
    let blockMatrixMultiply (left: uint64[]) (right: uint64[]) (rows: int) (cols: int) (inner: int) =
        let wordsPerRow = (cols + 63) / 64
        let result = memoryPool.Rent(rows * wordsPerRow)

        // ブロックサイズ（L1キャッシュに収まるように）
        let blockSize = 64

        // 並列処理
        Parallel.For(0, rows, fun i ->
            for jBlock in 0 .. blockSize .. cols - 1 do
                for kBlock in 0 .. blockSize .. inner - 1 do
                    for j in jBlock .. min (jBlock + blockSize - 1) (cols - 1) do
                        let mutable sum = 0UL
                        for k in kBlock .. min (kBlock + blockSize - 1) (inner - 1) do
                            let leftIdx = i * ((inner + 63) / 64) + k / 64
                            let rightIdx = k * wordsPerRow + j / 64
                            let leftBit = (left.[leftIdx] >>> (k % 64)) &&& 1UL
                            let rightBit = (right.[rightIdx] >>> (j % 64)) &&& 1UL
                            sum <- sum ^^^ (leftBit &&& rightBit)

                        let resultIdx = i * wordsPerRow + j / 64
                        let bitPos = j % 64
                        if sum = 1UL then
                            result.[resultIdx] <- result.[resultIdx] ||| (1UL <<< bitPos)
        )

        result

    interface IF2Operations with

        member _.MatrixMultiply left right rows cols inner =
            let leftArray = HardwareUtils.bitBlocksToUInt64Array left
            let rightArray = HardwareUtils.bitBlocksToUInt64Array right
            let resultArray = blockMatrixMultiply leftArray rightArray rows cols inner
            let result = HardwareUtils.uint64ArrayToBitBlocks resultArray
            memoryPool.Return(resultArray)
            result

        member _.MatrixEquals matrix1 matrix2 size =
            let arr1 = HardwareUtils.bitBlocksToUInt64Array matrix1
            let arr2 = HardwareUtils.bitBlocksToUInt64Array matrix2

            let mutable differences = 0
            let mutable isEqual = true

            // 並列比較
            let diffs = Array.zeroCreate parallelism
            Parallel.For(0, parallelism, fun thread ->
                let chunkSize = (size + parallelism - 1) / parallelism
                let start = thread * chunkSize
                let end' = min (start + chunkSize) size
                let mutable localDiffs = 0

                for i in start .. end' - 1 do
                    if arr1.[i] <> arr2.[i] then
                        localDiffs <- localDiffs + 1

                diffs.[thread] <- localDiffs
            )

            differences <- Array.sum diffs
            isEqual <- differences = 0
            (isEqual, differences)

        member this.BatchMatrixMultiply matrices dimensions =
            // 並列バッチ処理
            let results = Array.zeroCreate matrices.Length

            Parallel.For(0, matrices.Length, fun i ->
                let (left, right) = matrices.[i]
                let (rows, cols, inner) = dimensions.[i]
                results.[i] <- (this :> IF2Operations).MatrixMultiply left right rows cols inner
            )

            results

        member _.ComputeTopologicalRank matrix rows cols =
            let arr = HardwareUtils.bitBlocksToUInt64Array matrix
            let wordsPerRow = (cols + 63) / 64

            let mutable rank = 0
            let pivots = ResizeArray<int>()
            let used = Array.zeroCreate<bool> rows

            // ガウス消去法によるランク計算
            for col in 0 .. min rows cols - 1 do
                // ピボット探索
                let mutable pivotRow = -1
                for row in 0 .. rows - 1 do
                    if not used.[row] then
                        let wordIdx = row * wordsPerRow + col / 64
                        let bitPos = col % 64
                        if (arr.[wordIdx] >>> bitPos) &&& 1UL <> 0UL then
                            pivotRow <- row
                            break

                if pivotRow >= 0 then
                    used.[pivotRow] <- true
                    pivots.Add(pivotRow * cols + col)
                    rank <- rank + 1

                    // 消去ステップ
                    for row in 0 .. rows - 1 do
                        if row <> pivotRow && not used.[row] then
                            let wordIdx = row * wordsPerRow + col / 64
                            let bitPos = col % 64
                            if (arr.[wordIdx] >>> bitPos) &&& 1UL <> 0UL then
                                // XOR row with pivotRow
                                for w in 0 .. wordsPerRow - 1 do
                                    arr.[row * wordsPerRow + w] <-
                                        arr.[row * wordsPerRow + w] ^^^ arr.[pivotRow * wordsPerRow + w]

            (rank, pivots.ToArray())

        member _.ParallelXor vectors =
            if Array.isEmpty vectors || Array.isEmpty vectors.[0] then [||]
            else
                let length = vectors.[0].Length
                let arrVectors = vectors |> Array.map HardwareUtils.bitBlocksToUInt64Array
                let result = memoryPool.Rent(length)

                // 初期化
                Array.Clear(result, 0, length)

                // 並列XOR
                for vec in arrVectors do
                    xorOptimal vec result result length

                let blocks = HardwareUtils.uint64ArrayToBitBlocks result
                memoryPool.Return(result)
                blocks

        member _.PopCount value =
            popcount value

        member _.DeviceInfo() =
            {
                DeviceType = CPU(
                    cores = parallelism,
                    avx512 = hasAVX512,
                    avx2 = hasAVX2
                )
                MaxThreads = parallelism * 2
                MaxMemory = GC.GetTotalMemory(false)
                BitwiseOpsPerClock = if hasAVX512 then 8 elif hasAVX2 then 4 else 1
                SupportsInt64Atomics = true
                WarpSize = None
            }

        member _.MemoryInfo() =
            {
                TotalAllocated = memoryPool.TotalAllocated
                InUse = memoryPool.CurrentInUse
                PoolSize = int64 poolSize * 1024L * 1024L
                PinnedMemory = None
            }

        member _.Dispose() =
            memoryPool.Clear()

    interface IDisposable with
        member this.Dispose() =
            (this :> IF2Operations).Dispose()