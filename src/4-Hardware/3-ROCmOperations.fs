// src/4-Hardware/3-ROCmOperations.fs
namespace E8.Hardware.GPU

open System
open System.Runtime.InteropServices
open E8.Algebra
open E8.Hardware

/// HIPカーネルへのP/Invoke定義
module HIPInterop =

    [<DllImport("f2_hip.so", CallingConvention = CallingConvention.Cdecl)>]
    extern int hip_init(int deviceId)

    [<DllImport("f2_hip.so", CallingConvention = CallingConvention.Cdecl)>]
    extern int hip_cleanup()

    [<DllImport("f2_hip.so", CallingConvention = CallingConvention.Cdecl)>]
    extern int f2_matrix_multiply_gpu(
        IntPtr d_left,
        IntPtr d_right,
        IntPtr d_result,
        int rows,
        int cols,
        int inner,
        IntPtr stream
    )

    [<DllImport("f2_hip.so", CallingConvention = CallingConvention.Cdecl)>]
    extern int f2_topological_rank_gpu(
        IntPtr d_matrix,
        int rows,
        int cols,
        IntPtr d_rank,
        IntPtr d_pivots,
        IntPtr stream
    )

    [<DllImport("f2_hip.so", CallingConvention = CallingConvention.Cdecl)>]
    extern int f2_parallel_xor_gpu(
        IntPtr d_vectors,
        int num_vectors,
        int vector_length,
        IntPtr d_result,
        IntPtr stream
    )

    [<DllImport("f2_hip.so", CallingConvention = CallingConvention.Cdecl)>]
    extern IntPtr hip_malloc(uint64 size)

    [<DllImport("f2_hip.so", CallingConvention = CallingConvention.Cdecl)>]
    extern int hip_free(IntPtr ptr)

    [<DllImport("f2_hip.so", CallingConvention = CallingConvention.Cdecl)>]
    extern int hip_memcpy_h2d(IntPtr dst, IntPtr src, uint64 size, IntPtr stream)

    [<DllImport("f2_hip.so", CallingConvention = CallingConvention.Cdecl)>]
    extern int hip_memcpy_d2h(IntPtr dst, IntPtr src, uint64 size, IntPtr stream)

    [<DllImport("f2_hip.so", CallingConvention = CallingConvention.Cdecl)>]
    extern IntPtr hip_stream_create()

    [<DllImport("f2_hip.so", CallingConvention = CallingConvention.Cdecl)>]
    extern int hip_stream_destroy(IntPtr stream)

    [<DllImport("f2_hip.so", CallingConvention = CallingConvention.Cdecl)>]
    extern int hip_stream_synchronize(IntPtr stream)

    [<DllImport("f2_hip.so", CallingConvention = CallingConvention.Cdecl)>]
    extern int hip_get_device_properties(int deviceId, IntPtr properties)

/// ROCm/HIP GPU実装
type ROCmOperations(deviceId: int) =

    let mutable disposed = false
    let mutable totalAllocated = 0L
    let mutable currentInUse = 0L
    let stream = HIPInterop.hip_stream_create()

    // デバイス初期化
    do
        let result = HIPInterop.hip_init(deviceId)
        if result <> 0 then
            failwithf "Failed to initialize HIP device %d: error %d" deviceId result

    /// GPU上にメモリを確保
    let allocateDevice (size: int) : IntPtr =
        let ptr = HIPInterop.hip_malloc(uint64 size * 8UL)
        if ptr = IntPtr.Zero then
            failwith "Failed to allocate device memory"
        System.Threading.Interlocked.Add(&totalAllocated, int64 size * 8L) |> ignore
        System.Threading.Interlocked.Add(&currentInUse, int64 size * 8L) |> ignore
        ptr

    /// GPUメモリを解放
    let freeDevice (ptr: IntPtr) (size: int) =
        if ptr <> IntPtr.Zero then
            HIPInterop.hip_free(ptr) |> ignore
            System.Threading.Interlocked.Add(&currentInUse, -int64 size * 8L) |> ignore

    /// ホストからデバイスへコピー
    let copyToDevice (hostArray: uint64[]) : IntPtr =
        let size = hostArray.Length
        let devicePtr = allocateDevice size

        use pinned = GCHandle.Alloc(hostArray, GCHandleType.Pinned)
        let hostPtr = pinned.AddrOfPinnedObject()

        let result = HIPInterop.hip_memcpy_h2d(devicePtr, hostPtr, uint64 size * 8UL, stream)
        if result <> 0 then
            freeDevice devicePtr size
            failwith "Failed to copy data to device"

        devicePtr

    /// デバイスからホストへコピー
    let copyFromDevice (devicePtr: IntPtr) (size: int) : uint64[] =
        let hostArray = Array.zeroCreate size

        use pinned = GCHandle.Alloc(hostArray, GCHandleType.Pinned)
        let hostPtr = pinned.AddrOfPinnedObject()

        let result = HIPInterop.hip_memcpy_d2h(hostPtr, devicePtr, uint64 size * 8UL, stream)
        if result <> 0 then
            failwith "Failed to copy data from device"

        HIPInterop.hip_stream_synchronize(stream) |> ignore
        hostArray

    interface IF2Operations with

        member _.MatrixMultiply left right rows cols inner =
            let leftArray = HardwareUtils.bitBlocksToUInt64Array left
            let rightArray = HardwareUtils.bitBlocksToUInt64Array right

            let wordsPerRow = (cols + 63) / 64
            let resultSize = rows * wordsPerRow

            // デバイスメモリ確保とコピー
            let d_left = copyToDevice leftArray
            let d_right = copyToDevice rightArray
            let d_result = allocateDevice resultSize

            try
                // カーネル実行
                let result = HIPInterop.f2_matrix_multiply_gpu(
                    d_left, d_right, d_result,
                    rows, cols, inner, stream
                )

                if result <> 0 then
                    failwith "GPU matrix multiplication failed"

                // 結果取得
                let resultArray = copyFromDevice d_result resultSize
                HardwareUtils.uint64ArrayToBitBlocks resultArray

            finally
                freeDevice d_left leftArray.Length
                freeDevice d_right rightArray.Length
                freeDevice d_result resultSize

        member _.MatrixEquals matrix1 matrix2 size =
            // CPUで実行（小さい操作なのでGPU転送のオーバーヘッドを避ける）
            let arr1 = HardwareUtils.bitBlocksToUInt64Array matrix1
            let arr2 = HardwareUtils.bitBlocksToUInt64Array matrix2

            let mutable differences = 0
            for i in 0 .. size - 1 do
                if arr1.[i] <> arr2.[i] then
                    differences <- differences + 1

            (differences = 0, differences)

        member this.BatchMatrixMultiply matrices dimensions =
            // 個別に処理（将来的にバッチカーネルを実装可能）
            matrices
            |> Array.mapi (fun i (left, right) ->
                let (rows, cols, inner) = dimensions.[i]
                (this :> IF2Operations).MatrixMultiply left right rows cols inner
            )

        member _.ComputeTopologicalRank matrix rows cols =
            let arr = HardwareUtils.bitBlocksToUInt64Array matrix

            let d_matrix = copyToDevice arr
            let d_rank = allocateDevice 1
            let maxPivots = min rows cols
            let d_pivots = allocateDevice maxPivots

            try
                let result = HIPInterop.f2_topological_rank_gpu(
                    d_matrix, rows, cols,
                    d_rank, d_pivots, stream
                )

                if result <> 0 then
                    failwith "GPU topological rank computation failed"

                let rankArray = copyFromDevice d_rank 1
                let rank = int rankArray.[0]
                let pivotsArray = copyFromDevice d_pivots rank

                (rank, Array.map int pivotsArray)

            finally
                freeDevice d_matrix arr.Length
                freeDevice d_rank 1
                freeDevice d_pivots maxPivots

        member _.ParallelXor vectors =
            if Array.isEmpty vectors || Array.isEmpty vectors.[0] then [||]
            else
                let length = vectors.[0].Length
                let numVectors = vectors.Length

                // フラット化
                let flatArray =
                    vectors
                    |> Array.collect HardwareUtils.bitBlocksToUInt64Array

                let d_vectors = copyToDevice flatArray
                let d_result = allocateDevice length

                try
                    let result = HIPInterop.f2_parallel_xor_gpu(
                        d_vectors, numVectors, length,
                        d_result, stream
                    )

                    if result <> 0 then
                        failwith "GPU parallel XOR failed"

                    let resultArray = copyFromDevice d_result length
                    HardwareUtils.uint64ArrayToBitBlocks resultArray

                finally
                    freeDevice d_vectors flatArray.Length
                    freeDevice d_result length

        member _.PopCount value =
            // CPUで実行（単一値の操作）
            let rec count v acc =
                if v = 0UL then acc
                else count (v &&& (v - 1UL)) (acc + 1)
            count value 0

        member _.DeviceInfo() =
            // 簡略化（実際はHIPから取得）
            {
                DeviceType = GPU(
                    model = "AMD GPU",
                    computeUnits = 60,
                    architecture = "RDNA2"
                )
                MaxThreads = 2048
                MaxMemory = 8L * 1024L * 1024L * 1024L
                BitwiseOpsPerClock = 64
                SupportsInt64Atomics = true
                WarpSize = Some 64
            }

        member _.MemoryInfo() =
            {
                TotalAllocated = totalAllocated
                InUse = currentInUse
                PoolSize = 0L
                PinnedMemory = Some currentInUse
            }

        member _.Dispose() =
            if not disposed then
                HIPInterop.hip_stream_synchronize(stream) |> ignore
                HIPInterop.hip_stream_destroy(stream) |> ignore
                HIPInterop.hip_cleanup() |> ignore
                disposed <- true

    interface IDisposable with
        member this.Dispose() =
            (this :> IF2Operations).Dispose()