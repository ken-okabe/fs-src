// 5-Observable/1-ObservableBase.fs
namespace E8.Observable

open System
open System.Threading.Tasks
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.Ace

/// 観測量の基本型（すべてF₂のパリティとして統一）
type ObservableValue = {
    /// パリティ値（0 or 1）
    Parity: F2
    /// 測定に寄与した要素数
    ContributionCount: int
    /// 信頼度（収束度に基づく）
    Confidence: float
}

/// 観測量の種類
type ObservableKind =
    | LocalMagnetization
    | LocalParity
    | TwoPointCorrelation of distance: int * int
    | ConnectedCorrelation of distance: int * int
    | WilsonLoop of contour: (int * int) list
    | StringOrder of path: (int * int) list
    | TopologicalEntropy
    | PlaquetteOperator of size: int
    | VortexDensity
    | ChiralOrder

/// 観測量の測定位置情報
type MeasurementLocation = {
    /// 主測定点
    PrimaryPosition: int * int
    /// 補助測定点（相関関数など）
    SecondaryPositions: (int * int) list
    /// 測定領域の半径
    Radius: int
}

/// 観測量の完全な結果
type ObservableResult = {
    /// 観測量の種類
    Kind: ObservableKind
    /// 測定値
    Value: ObservableValue
    /// 測定位置
    Location: MeasurementLocation
    /// 計算時間
    ComputationTimeMs: float
    /// 使用したデバイス
    DeviceUsed: string
    /// タイムスタンプ
    Timestamp: DateTime
}

/// 観測量計算の共通インターフェース
type IObservable =
    /// 観測量を計算
    abstract member Compute:
        environment: ACE_CTMRG.SymmetricEnvironment ->
        peps: Tensor5F2 ->
        location: MeasurementLocation ->
        ops: IF2Operations ->
        ObservableValue

    /// バッチ計算
    abstract member ComputeBatch:
        environment: ACE_CTMRG.SymmetricEnvironment ->
        peps: Tensor5F2 ->
        locations: MeasurementLocation[] ->
        ops: IF2Operations ->
        ObservableValue[]

    /// 観測量の種類
    abstract member Kind: ObservableKind

/// パリティ計算の基本実装
module ParityComputation =

    /// ビット列のパリティを計算（XORの累積）
    let computeParity (bits: uint64[]) =
        let mutable parity = 0UL
        for bit in bits do
            parity <- parity ^^^ bit

        if parity = 0UL then F2.Zero else F2.One

    /// F₂要素のリストからパリティを計算
    let computeF2Parity (elements: F2 list) =
        elements |> List.fold (fun acc elem ->
            if elem = F2.One then
                if acc = F2.Zero then F2.One else F2.Zero
            else
                acc
        ) F2.Zero

    /// 環境テンソルの局所領域からパリティを抽出
    let extractLocalParity (env: ACE_CTMRG.SymmetricEnvironment)
                          (centerX: int, centerY: int)
                          (radius: int) =

        let chi = env.Chi
        let mutable parityBits = []

        for dx in -radius .. radius do
            for dy in -radius .. radius do
                let x = (centerX + dx + chi) % chi
                let y = (centerY + dy + chi) % chi
                let idx = x * chi + y

                if idx < env.UniqueCorner.Length then
                    parityBits <- env.UniqueCorner.[idx].Bits :: parityBits

        computeParity (List.toArray parityBits)

    /// テンソル縮約結果のパリティ
    let contractAndComputeParity (tensor1: BitBlock64[])
                                 (tensor2: BitBlock64[])
                                 (contractDims: int list)
                                 (ops: IF2Operations) =

        // 縮約の実行
        let contracted =
            if contractDims.IsEmpty then
                // 要素ごとの積
                Array.map2 (fun t1 t2 ->
                    { Bits = t1.Bits &&& t2.Bits }) tensor1 tensor2
            else
                // 実際の縮約（簡略化された実装）
                let size = min tensor1.Length tensor2.Length
                Array.init size (fun i ->
                    { Bits = tensor1.[i % tensor1.Length].Bits &&&
                             tensor2.[i % tensor2.Length].Bits })

        // パリティ計算
        let bits = contracted |> Array.map (fun b -> b.Bits)
        computeParity bits

/// 観測量計算のユーティリティ
module ObservableUtils =

    /// 環境テンソルから転送行列を構築してスペクトル情報を取得
    let getSpectralInfo (env: ACE_CTMRG.SymmetricEnvironment)
                        (peps: Tensor5F2)
                        (ops: IF2Operations) =

        // 転送行列の構築
        let transfer = ProjectorSpectral.constructTransferMatrix
                           env peps (ProjectorSpectral.Horizontal env.Chi) ops

        // プロジェクター検証
        let verified = ProjectorSpectral.verifyProjectorHypothesis transfer ops

        match verified.SpectralInfo with
        | Some info ->
            {|
                SpectralGap = info.SpectralGap
                Rank = info.Rank
                IsProjector = verified.ProjectorStatus
                              |> Option.map (fun s -> s.IsProjector)
                              |> Option.defaultValue false
            |}
        | None ->
            {|
                SpectralGap = F2.Zero
                Rank = 0
                IsProjector = false
            |}

    /// 収束度から信頼度を計算
    let computeConfidence (env: ACE_CTMRG.SymmetricEnvironment) =
        if env.Iteration = 0 then
            0.0
        else
            // 収束履歴から信頼度を推定
            let recentNorms =
                env.History.Norms
                |> Array.rev
                |> Array.truncate 5

            if recentNorms.Length < 2 then
                float env.Iteration / 100.0
            else
                // ノルムの変動が小さいほど高信頼度
                let variance =
                    let mean = recentNorms |> Array.averageBy float
                    recentNorms
                    |> Array.sumBy (fun n ->
                        let diff = float n - mean
                        diff * diff)
                    |> fun sum -> sum / float recentNorms.Length

                1.0 / (1.0 + variance)

    /// D4対称性を考慮した測定
    let measureWithSymmetry (env: ACE_CTMRG.SymmetricEnvironment)
                           (measureFunc: ACE_CTMRG.SymmetricEnvironment -> F2)
                           (symmetryOps: int) =

        // 8つの対称操作（D4群）の平均
        let measurements = ResizeArray<F2>()

        for rotation in 0 .. 3 do
            let rotated = ProjectorSpectral.rotateEnvironment env rotation
            measurements.Add(measureFunc rotated)

        // 反転も考慮
        for flip in 0 .. 1 do
            let flipped = flipEnvironment env flip
            measurements.Add(measureFunc flipped)

        // 多数決（F₂なので偶奇で決定）
        let ones = measurements |> Seq.filter (fun m -> m = F2.One) |> Seq.length
        if ones >= 4 then F2.One else F2.Zero

    /// 環境の反転
    and flipEnvironment (env: ACE_CTMRG.SymmetricEnvironment) (axis: int) =
        let chi = env.Chi
        let flippedCorner = Array.copy env.UniqueCorner

        if axis = 0 then
            // 水平反転
            for i in 0 .. chi - 1 do
                for j in 0 .. chi / 2 - 1 do
                    let idx1 = i * chi + j
                    let idx2 = i * chi + (chi - 1 - j)
                    if idx1 < flippedCorner.Length && idx2 < flippedCorner.Length then
                        let temp = flippedCorner.[idx1]
                        flippedCorner.[idx1] <- flippedCorner.[idx2]
                        flippedCorner.[idx2] <- temp
        else
            // 垂直反転
            for i in 0 .. chi / 2 - 1 do
                for j in 0 .. chi - 1 do
                    let idx1 = i * chi + j
                    let idx2 = (chi - 1 - i) * chi + j
                    if idx1 < flippedCorner.Length && idx2 < flippedCorner.Length then
                        let temp = flippedCorner.[idx1]
                        flippedCorner.[idx1] <- flippedCorner.[idx2]
                        flippedCorner.[idx2] <- temp

        { env with UniqueCorner = flippedCorner }

/// 観測量計算のバッチプロセッサ
type ObservableBatchProcessor(ops: IF2Operations) =

    let batchSize =
        match ops.DeviceInfo().DeviceType with
        | GPU(_, computeUnits, _) -> computeUnits * 256
        | CPU(cores, _, _) -> cores * 16

    /// バッチ処理の実行
    member _.ProcessBatch<'T when 'T :> IObservable>
                         (observable: 'T)
                         (env: ACE_CTMRG.SymmetricEnvironment)
                         (peps: Tensor5F2)
                         (locations: MeasurementLocation[]) =

        let timer = System.Diagnostics.Stopwatch.StartNew()

        // バッチに分割
        let batches =
            locations
            |> Array.chunkBySize batchSize

        let results = ResizeArray<ObservableResult>()

        for batch in batches do
            // 並列計算
            let values = observable.ComputeBatch env peps batch ops

            // 結果の構築
            for i in 0 .. batch.Length - 1 do
                let result = {
                    Kind = observable.Kind
                    Value = values.[i]
                    Location = batch.[i]
                    ComputationTimeMs = timer.Elapsed.TotalMilliseconds / float batch.Length
                    DeviceUsed =
                        match ops.DeviceInfo().DeviceType with
                        | CPU(cores, avx512, _) -> sprintf "CPU(%d cores, AVX512=%b)" cores avx512
                        | GPU(model, _, _) -> sprintf "GPU(%s)" model
                    Timestamp = DateTime.UtcNow
                }
                results.Add(result)

        timer.Stop()
        results.ToArray()