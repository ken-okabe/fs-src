// 4-Hardware/ROCmOperations.fs
namespace E8.Hardware.GPU

open System
open System.Runtime.InteropServices
open System.Collections.Concurrent
open System.Threading
open E8.Algebra
open E8.Hardware

/// ROCm/HIP P/Invoke定義
module HIPNative =

    [<Literal>]
    let LibraryName = "f2_hip.so"

    // 基本的なHIP関数
    [<DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)>]
    extern int hipInit(int flags)

    [<DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)>]
    extern int hipGetDeviceCount(int& count)

    [<DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)>]
    extern int hipSetDevice(int device)

    [<DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)>]
    extern int hipGetDeviceProperties(nativeint props, int device)

    // メモリ管理
    [<DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)>]
    extern int hipMalloc(nativeint& ptr, uint64 size)

    [<DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)>]
    extern int hipHostMalloc(nativeint& ptr, uint64 size, int flags)

    [<DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)>]
    extern int hipMemcpy(nativeint dst, nativeint src, uint64 size, int kind)

    [<DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)>]
    extern int hipMemcpyAsync(nativeint dst, nativeint src, uint64 size, int kind, nativeint stream)

    [<DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)>]
    extern int hipMemset(nativeint ptr, int value, uint64 size)

    [<DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)>]
    extern int hipFree(nativeint ptr)

    [<DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)>]
    extern int hipHostFree(nativeint ptr)

    // ストリーム管理
    [<DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)>]
    extern int hipStreamCreate(nativeint& stream)

    [<DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)>]
    extern int hipStreamSynchronize(nativeint stream)

    [<DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)>]
    extern int hipStreamDestroy(nativeint stream)

    // F₂特化カーネル
    [<DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)>]
    extern int f2_matrix_multiply_gpu(
        nativeint d_left, nativeint d_right, nativeint d_result,
        int rows, int cols, int inner, nativeint stream)

    [<DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)>]
    extern int f2_matrix_equals_gpu(
        nativeint d_matrix1, nativeint d_matrix2,
        int size, nativeint d_differences, nativeint stream)

    [<DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)>]
    extern int f2_batch_matrix_multiply_gpu(
        nativeint d_matrices, nativeint d_dimensions, nativeint d_offsets,
        nativeint d_results, int batch_size, nativeint stream)

    [<DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)>]
    extern int f2_compute_topological_rank_gpu(
        nativeint d_matrix, int rows, int cols,
        nativeint d_rank, nativeint d_pivots, nativeint stream)

    [<DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)>]
    extern int f2_parallel_xor_gpu(
        nativeint d_vectors, int num_vectors, int vector_length,
        nativeint d_result, nativeint stream)

/// デバイスプロパティ構造体
[<Struct; StructLayout(LayoutKind.Sequential)>]
type HipDeviceProperties = {
    name: [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 256)>] byte[]
    totalGlobalMem: uint64
    sharedMemPerBlock: uint64
    regsPerBlock: int
    warpSize: int
    maxThreadsPerBlock: int
    maxThreadsDim: [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 3)>] int[]
    maxGridSize: [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 3)>] int[]
    clockRate: int
    multiProcessorCount: int
    computeMode: int
    major: int
    minor: int
}

/// メモリコピー方向
type HipMemcpyKind =
    | HostToHost = 0
    | HostToDevice = 1
    | DeviceToHost = 2
    | DeviceToDevice = 3

/// GPUメモリプール管理
type GpuMemoryPool(initialSizeMB: int) =
    let initialSize = int64(initialSizeMB * 1024 * 1024)
    let blocks = ConcurrentDictionary<nativeint, int64>()
    let freeList = ConcurrentBag<nativeint * int64>()
    let mutable totalAllocated = 0L
    let mutable currentlyUsed = 0L
    let allocLock = obj()

    // 初期ブロック確保
    do
        let mutable ptr = 0n
        if HIPNative.hipMalloc(&ptr, uint64 initialSize) = 0 then
            freeList.Add((ptr, initialSize))
            Interlocked.Add(&totalAllocated, initialSize) |> ignore

    member _.Allocate(size: int64) =
        lock allocLock (fun () ->
            // 既存の空きブロックを探す
            let mutable found = None
            let tempList = ResizeArray<nativeint * int64>()

            while found.IsNone && not freeList.IsEmpty do
                match freeList.TryTake() with
                | true, (ptr, blockSize) when blockSize >= size ->
                    found <- Some ptr
                    // 余った部分を戻す
                    if blockSize > size then
                        freeList.Add((ptr + nativeint size, blockSize - size))
                    blocks.[ptr] <- size
                    Interlocked.Add(&currentlyUsed, size) |> ignore
                | true, block ->
                    tempList.Add(block)
                | false, _ -> ()

            // 一時リストを戻す
            for block in tempList do
                freeList.Add(block)

            match found with
            | Some ptr -> ptr
            | None ->
                // 新規割り当て
                let allocSize = max size (int64(10 * 1024 * 1024))  // 最小10MB
                let mutable ptr = 0n
                if HIPNative.hipMalloc(&ptr, uint64 allocSize) = 0 then
                    blocks.[ptr] <- size
                    Interlocked.Add(&totalAllocated, allocSize) |> ignore
                    Interlocked.Add(&currentlyUsed, size) |> ignore
                    // 余りを空きリストに
                    if allocSize > size then
                        freeList.Add((ptr + nativeint size, allocSize - size))
                    ptr
                else
                    raise (OutOfMemoryException("GPU memory allocation failed"))
        )

    member _.Free(ptr: nativeint) =
        match blocks.TryRemove(ptr) with
        | true, size ->
            Interlocked.Add(&currentlyUsed, -size) |> ignore
            freeList.Add((ptr, size))
        | false, _ -> ()

    member _.Statistics =
        {|
            TotalAllocated = totalAllocated
            CurrentlyUsed = currentlyUsed
            FreeBlocks = freeList.Count
        |}

    interface IDisposable with
        member _.Dispose() =
            // すべての確保済みメモリを解放
            let allPtrs = HashSet<nativeint>()

            for KeyValue(ptr, _) in blocks do
                allPtrs.Add(ptr) |> ignore

            while not freeList.IsEmpty do
                match freeList.TryTake() with
                | true, (ptr, _) -> allPtrs.Add(ptr) |> ignore
                | false, _ -> ()

            for ptr in allPtrs do
                HIPNative.hipFree(ptr) |> ignore

/// ピン留めメモリ管理
type PinnedMemoryCache() =
    let cache = ConcurrentDictionary<int, nativeint>()
    let mutable totalPinned = 0L

    member _.GetBuffer(size: int) =
        cache.GetOrAdd(size, fun sz ->
            let mutable ptr = 0n
            if HIPNative.hipHostMalloc(&ptr, uint64 sz, 0) = 0 then
                Interlocked.Add(&totalPinned, int64 sz) |> ignore
                ptr
            else
                raise (OutOfMemoryException("Failed to allocate pinned memory"))
        )

    member _.TotalPinned = totalPinned

    interface IDisposable with
        member _.Dispose() =
            for KeyValue(_, ptr) in cache do
                HIPNative.hipHostFree(ptr) |> ignore

/// ROCm GPU実装
type ROCmOperations(?deviceId: int) =
    let deviceId = defaultArg deviceId 0
    let mutable stream = 0n
    let memoryPool = new GpuMemoryPool(100)  // 100MB初期プール
    let pinnedCache = new PinnedMemoryCache()
    let mutable deviceProps = Unchecked.defaultof<HipDeviceProperties>

    // 初期化
    do
        // HIP初期化
        let initResult = HIPNative.hipInit(0)
        if initResult <> 0 then
            failwithf "Failed to initialize HIP: %d" initResult

        // デバイス数確認
        let mutable count = 0
        if HIPNative.hipGetDeviceCount(&count) <> 0 || count = 0 then
            failwith "No GPU devices found"

        // デバイス設定
        if HIPNative.hipSetDevice(deviceId) <> 0 then
            failwithf "Failed to set GPU device %d" deviceId

        // デバイスプロパティ取得
        let propsPtr = Marshal.AllocHGlobal(Marshal.SizeOf<HipDeviceProperties>())
        try
            if HIPNative.hipGetDeviceProperties(propsPtr, deviceId) = 0 then
                deviceProps <- Marshal.PtrToStructure<HipDeviceProperties>(propsPtr)
        finally
            Marshal.FreeHGlobal(propsPtr)

        // ストリーム作成
        if HIPNative.hipStreamCreate(&stream) <> 0 then
            failwith "Failed to create HIP stream"

    /// デバイスへのデータ転送
    let copyToDevice(data: BitBlock64[], size: int) =
        let bytes = size * sizeof<uint64>
        let pinnedPtr = pinnedCache.GetBuffer(bytes)

        // ピン留めメモリにコピー
        let uint64Array = Array.map (fun (b: BitBlock64) -> b.Bits) data
        Marshal.Copy(uint64Array, 0, pinnedPtr, size)

        // GPUメモリ確保
        let devicePtr = memoryPool.Allocate(int64 bytes)

        // 非同期転送
        if HIPNative.hipMemcpyAsync(devicePtr, pinnedPtr, uint64 bytes,
                                    int HipMemcpyKind.HostToDevice, stream) <> 0 then
            memoryPool.Free(devicePtr)
            failwith "Failed to copy data to device"

        devicePtr

    /// デバイスからのデータ取得
    let copyFromDevice(devicePtr: nativeint, size: int) =
        let bytes = size * sizeof<uint64>
        let pinnedPtr = pinnedCache.GetBuffer(bytes)

        // 非同期転送
        if HIPNative.hipMemcpyAsync(pinnedPtr, devicePtr, uint64 bytes,
                                    int HipMemcpyKind.DeviceToHost, stream) <> 0 then
            failwith "Failed to copy data from device"

        // 同期
        if HIPNative.hipStreamSynchronize(stream) <> 0 then
            failwith "Stream synchronization failed"

        // BitBlock64配列に変換
        let uint64Array = Array.zeroCreate<uint64> size
        Marshal.Copy(pinnedPtr, uint64Array, 0, size)
        Array.map (fun bits -> { Bits = bits }) uint64Array

    /// 行列積GPU実装
    let matrixMultiplyGPU (left: BitBlock64[]) (right: BitBlock64[])
                         (rows: int) (cols: int) (inner: int) =

        let d_left = copyToDevice(left, rows * inner)
        let d_right = copyToDevice(right, inner * cols)
        let d_result = memoryPool.Allocate(int64(rows * cols * sizeof<uint64>))

        try
            // GPUカーネル実行
            if HIPNative.f2_matrix_multiply_gpu(d_left, d_right, d_result,
                                               rows, cols, inner, stream) <> 0 then
                failwith "GPU matrix multiplication failed"

            copyFromDevice(d_result, rows * cols)
        finally
            memoryPool.Free(d_left)
            memoryPool.Free(d_right)
            memoryPool.Free(d_result)

    /// 行列等価性チェックGPU実装
    let matrixEqualsGPU (matrix1: BitBlock64[]) (matrix2: BitBlock64[]) (size: int) =
        let d_matrix1 = copyToDevice(matrix1, size)
        let d_matrix2 = copyToDevice(matrix2, size)
        let d_differences = memoryPool.Allocate(int64 sizeof<int>)

        try
            // 差分カウント用メモリをゼロクリア
            if HIPNative.hipMemset(d_differences, 0, uint64 sizeof<int>) <> 0 then
                failwith "Failed to clear differences counter"

            // GPUカーネル実行
            if HIPNative.f2_matrix_equals_gpu(d_matrix1, d_matrix2,
                                             size, d_differences, stream) <> 0 then
                failwith "GPU matrix comparison failed"

            // 同期
            if HIPNative.hipStreamSynchronize(stream) <> 0 then
                failwith "Stream synchronization failed"

            // 結果取得
            let pinnedPtr = pinnedCache.GetBuffer(sizeof<int>)
            if HIPNative.hipMemcpy(pinnedPtr, d_differences, uint64 sizeof<int>,
                                  int HipMemcpyKind.DeviceToHost) <> 0 then
                failwith "Failed to copy differences"

            let differences = Marshal.ReadInt32(pinnedPtr)
            (differences = 0, differences)
        finally
            memoryPool.Free(d_matrix1)
            memoryPool.Free(d_matrix2)
            memoryPool.Free(d_differences)

    /// バッチ行列積GPU実装
    let batchMatrixMultiplyGPU (matrices: (BitBlock64[] * BitBlock64[])[])
                              (dimensions: (int * int * int)[]) =

        // バッチデータの準備
        let totalElements =
            matrices |> Array.sumBy (fun (l, r) -> l.Length + r.Length)

        let allData = Array.zeroCreate<uint64> totalElements
        let offsets = Array.zeroCreate<int> (matrices.Length * 3)

        let mutable dataOffset = 0
        for i in 0 .. matrices.Length - 1 do
            let (left, right) = matrices.[i]
            let (rows, cols, inner) = dimensions.[i]

            // オフセット記録
            offsets.[i * 3] <- dataOffset  // 左行列開始位置
            offsets.[i * 3 + 1] <- dataOffset + left.Length  // 右行列開始位置
            offsets.[i * 3 + 2] <- dataOffset + left.Length + right.Length  // 結果開始位置

            // データコピー
            for j in 0 .. left.Length - 1 do
                allData.[dataOffset + j] <- left.[j].Bits
            dataOffset <- dataOffset + left.Length

            for j in 0 .. right.Length - 1 do
                allData.[dataOffset + j] <- right.[j].Bits
            dataOffset <- dataOffset + right.Length

        // GPU転送
        let d_matrices = copyToDevice(Array.map (fun x -> { Bits = x }) allData, allData.Length)
        let d_dimensions = memoryPool.Allocate(int64(dimensions.Length * 3 * sizeof<int>))
        let d_offsets = memoryPool.Allocate(int64(offsets.Length * sizeof<int>))
        let resultSize = dimensions |> Array.sumBy (fun (r, c, _) -> r * c)
        let d_results = memoryPool.Allocate(int64(resultSize * sizeof<uint64>))

        try
            // 次元情報とオフセット転送
            let dimArray = dimensions |> Array.collect (fun (r, c, k) -> [| r; c; k |])
            let pinnedDims = pinnedCache.GetBuffer(dimArray.Length * sizeof<int>)
            Marshal.Copy(dimArray, 0, pinnedDims, dimArray.Length)

            if HIPNative.hipMemcpy(d_dimensions, pinnedDims,
                                  uint64(dimArray.Length * sizeof<int>),
                                  int HipMemcpyKind.HostToDevice) <> 0 then
                failwith "Failed to copy dimensions"

            let pinnedOffsets = pinnedCache.GetBuffer(offsets.Length * sizeof<int>)
            Marshal.Copy(offsets, 0, pinnedOffsets, offsets.Length)

            if HIPNative.hipMemcpy(d_offsets, pinnedOffsets,
                                  uint64(offsets.Length * sizeof<int>),
                                  int HipMemcpyKind.HostToDevice) <> 0 then
                failwith "Failed to copy offsets"

            // GPUカーネル実行
            if HIPNative.f2_batch_matrix_multiply_gpu(
                d_matrices, d_dimensions, d_offsets,
                d_results, matrices.Length, stream) <> 0 then
                failwith "Batch matrix multiplication failed"

            // 結果取得
            let allResults = copyFromDevice(d_results, resultSize)

            // 結果を個別の配列に分割
            let results = Array.zeroCreate matrices.Length
            let mutable resultOffset = 0

            for i in 0 .. matrices.Length - 1 do
                let (r, c, _) = dimensions.[i]
                let size = r * c
                results.[i] <- Array.sub allResults resultOffset size
                resultOffset <- resultOffset + size

            results
        finally
            memoryPool.Free(d_matrices)
            memoryPool.Free(d_dimensions)
            memoryPool.Free(d_offsets)
            memoryPool.Free(d_results)

    /// トポロジカルランク計算GPU実装
    let computeTopologicalRankGPU (matrix: BitBlock64[]) (rows: int) (cols: int) =
        let d_matrix = copyToDevice(matrix, rows * cols)
        let d_rank = memoryPool.Allocate(int64 sizeof<int>)
        let maxPivots = min rows cols
        let d_pivots = memoryPool.Allocate(int64(maxPivots * sizeof<int>))

        try
            // カーネル実行
            if HIPNative.f2_compute_topological_rank_gpu(
                d_matrix, rows, cols, d_rank, d_pivots, stream) <> 0 then
                failwith "Topological rank computation failed"

            // 同期
            if HIPNative.hipStreamSynchronize(stream) <> 0 then
                failwith "Stream synchronization failed"

            // ランク取得
            let pinnedRank = pinnedCache.GetBuffer(sizeof<int>)
            if HIPNative.hipMemcpy(pinnedRank, d_rank, uint64 sizeof<int>,
                                  int HipMemcpyKind.DeviceToHost) <> 0 then
                failwith "Failed to copy rank"

            let rank = Marshal.ReadInt32(pinnedRank)

            // ピボット取得
            let pinnedPivots = pinnedCache.GetBuffer(rank * sizeof<int>)
            if HIPNative.hipMemcpy(pinnedPivots, d_pivots, uint64(rank * sizeof<int>),
                                  int HipMemcpyKind.DeviceToHost) <> 0 then
                failwith "Failed to copy pivots"

            let pivots = Array.zeroCreate<int> rank
            Marshal.Copy(pinnedPivots, pivots, 0, rank)

            (rank, pivots)
        finally
            memoryPool.Free(d_matrix)
            memoryPool.Free(d_rank)
            memoryPool.Free(d_pivots)

    interface IF2Operations with
        member _.MatrixMultiply left right rows cols inner =
            matrixMultiplyGPU left right rows cols inner

        member _.MatrixEquals matrix1 matrix2 size =
            matrixEqualsGPU matrix1 matrix2 size

        member _.BatchMatrixMultiply matrices dimensions =
            batchMatrixMultiplyGPU matrices dimensions

        member _.ComputeTopologicalRank matrix rows cols =
            computeTopologicalRankGPU matrix rows cols

        member _.ParallelXor vectors =
            let len = vectors.[0].Length
            let numVectors = vectors.Length

            // フラット化
            let flatData = Array.zeroCreate<BitBlock64>(numVectors * len)
            for i in 0 .. numVectors - 1 do
                Array.blit vectors.[i] 0 flatData (i * len) len

            let d_vectors = copyToDevice(flatData, flatData.Length)
            let d_result = memoryPool.Allocate(int64(len * sizeof<uint64>))

            try
                if HIPNative.f2_parallel_xor_gpu(
                    d_vectors, numVectors, len, d_result, stream) <> 0 then
                    failwith "Parallel XOR failed"

                copyFromDevice(d_result, len)
            finally
                memoryPool.Free(d_vectors)
                memoryPool.Free(d_result)

        member _.PopCount value =
            // GPUでは個別のポピュレーションカウントは効率的でないのでCPU実装
            let mutable v = value
            let mutable count = 0
            while v <> 0UL do
                count <- count + 1
                v <- v &&& (v - 1UL)
            count

        member _.DeviceInfo() =
            let name = System.Text.Encoding.ASCII.GetString(deviceProps.name)
                        |> fun s -> s.TrimEnd(char 0)
            {
                DeviceType = GPU(name, deviceProps.multiProcessorCount, "ROCm")
                MaxThreads = deviceProps.maxThreadsPerBlock * deviceProps.multiProcessorCount
                MaxMemory = int64 deviceProps.totalGlobalMem
                BitwiseOpsPerClock = 64 * deviceProps.warpSize * deviceProps.multiProcessorCount
                SupportsInt64Atomics = true
                WarpSize = Some deviceProps.warpSize
            }

        member _.MemoryInfo() =
            let stats = memoryPool.Statistics
            {
                TotalAllocated = stats.TotalAllocated
                InUse = stats.CurrentlyUsed
                PoolSize = stats.TotalAllocated - stats.CurrentlyUsed
                PinnedMemory = Some pinnedCache.TotalPinned
            }

        member _.Dispose() =
            if stream <> 0n then
                HIPNative.hipStreamDestroy(stream) |> ignore
            (memoryPool :> IDisposable).Dispose()
            (pinnedCache :> IDisposable).Dispose()
