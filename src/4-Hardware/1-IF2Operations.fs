// src/4-Hardware/1-IF2Operations.fs
namespace E8.Hardware

open System
open E8.Algebra

/// F₂演算の共通インターフェース - ACE理論の計算基盤
type IF2Operations =
    /// ビットパックされた行列積（F₂上）
    abstract member MatrixMultiply:
        left: BitBlock64[] ->
        right: BitBlock64[] ->
        rows: int ->
        cols: int ->
        inner: int ->
        BitBlock64[]

    /// 行列の等価性チェック（プロジェクター検証用）
    abstract member MatrixEquals:
        matrix1: BitBlock64[] ->
        matrix2: BitBlock64[] ->
        size: int ->
        bool * int  // (等しいか, 差分の数)

    /// バッチ行列積（局所計算の並列実行用）
    abstract member BatchMatrixMultiply:
        matrices: (BitBlock64[] * BitBlock64[])[] ->
        dimensions: (int * int * int)[] ->
        BitBlock64[][]

    /// トポロジカルランク計算（Renorm-F2の中核）
    abstract member ComputeTopologicalRank:
        matrix: BitBlock64[] ->
        rows: int ->
        cols: int ->
        int * int[]  // (rank, pivot indices)

    /// ビット並列XOR（環境テンソル更新用）
    abstract member ParallelXor:
        vectors: BitBlock64[][] ->
        BitBlock64[]

    /// ポピュレーションカウント（パリティ計算）
    abstract member PopCount:
        value: uint64 ->
        int

    /// デバイス情報
    abstract member DeviceInfo: unit -> DeviceCapabilities

    /// メモリ管理情報
    abstract member MemoryInfo: unit -> MemoryStatus

    /// リソース解放
    abstract member Dispose: unit -> unit

/// デバイス能力情報
and DeviceCapabilities = {
    DeviceType: DeviceType
    MaxThreads: int
    MaxMemory: int64
    BitwiseOpsPerClock: int
    SupportsInt64Atomics: bool
    WarpSize: int option  // GPUの場合のウェーブフロントサイズ
}

and DeviceType =
    | CPU of cores: int * avx512: bool * avx2: bool
    | GPU of model: string * computeUnits: int * architecture: string

and MemoryStatus = {
    TotalAllocated: int64
    InUse: int64
    PoolSize: int64
    PinnedMemory: int64 option
}

/// ハードウェア操作のヘルパー関数
module HardwareUtils =

    /// BitBlock64配列をuint64配列に変換
    let inline bitBlocksToUInt64Array (blocks: BitBlock64[]) : uint64[] =
        Array.map (fun (b: BitBlock64) -> b.Bits) blocks

    /// uint64配列をBitBlock64配列に変換
    let inline uint64ArrayToBitBlocks (arr: uint64[]) : BitBlock64[] =
        Array.map (fun bits -> { Bits = bits }) arr

    /// 行列サイズからワード数を計算
    let inline computeMatrixWords (rows: int) (cols: int) : int =
        let wordsPerRow = (cols + 63) / 64
        rows * wordsPerRow

    /// デバイス選択ヒューリスティック
    let selectOptimalDevice (matrixSize: int) (availableDevices: DeviceCapabilities list) =
        match availableDevices with
        | [] -> failwith "No compute devices available"
        | [single] -> single
        | devices ->
            // 簡単なヒューリスティック: 大きい行列はGPU、小さい行列はCPU
            if matrixSize > 1024 * 1024 then
                devices |> List.tryFind (fun d ->
                    match d.DeviceType with
                    | GPU _ -> true
                    | _ -> false)
                |> Option.defaultValue devices.[0]
            else
                devices |> List.tryFind (fun d ->
                    match d.DeviceType with
                    | CPU _ -> true
                    | _ -> false)
                |> Option.defaultValue devices.[0]