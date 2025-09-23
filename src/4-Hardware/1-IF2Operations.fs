// 4-Hardware/1-IF2Operations.fs
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
 