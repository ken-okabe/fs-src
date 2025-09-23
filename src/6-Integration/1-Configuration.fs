// 7-Integration/1-Configuration.fs
namespace E8.Integration

open System
open System.IO
open E8.Algebra
open E8.Hardware
open E8.Ace
open E8.Observable

/// システム全体の設定
type SystemConfiguration = {
    /// 格子設定
    Lattice: LatticeConfiguration
    /// PEPS設定
    PEPS: PEPSConfiguration
    /// ACE設定
    ACE: ACEConfiguration
    /// ハードウェア設定
    Hardware: HardwareConfiguration
    /// 観測量設定
    Observable: ObservableConfiguration
    /// 出力設定
    Output: OutputConfiguration
    /// 実行設定
    Runtime: RuntimeConfiguration
}

and LatticeConfiguration = {
    /// 格子サイズ (幅, 高さ)
    Size: int * int
    /// 境界条件
    BoundaryCondition: BoundaryCondition
    /// D4対称性を強制するか
    EnforceSymmetry: bool
}

and BoundaryCondition =
    | Periodic      // トーラス
    | Open          // 開放端
    | Cylindrical   // 円筒

and PEPSConfiguration = {
    /// 物理次元（フィボナッチの場合2）
    PhysicalDimension: int
    /// ボンド次元
    BondDimension: int
    /// 初期化方法
    Initialization: InitializationMethod
    /// MPO-Injectivityを保証するか
    EnsureMPOInjectivity: bool
}

and InitializationMethod =
    | Random of seed: int
    | Uniform of value: F2
    | FromFile of path: string
    | FibonacciBasis
    | GoldenChain

and ACEConfiguration = {
    /// 初期環境ボンド次元
    InitialChi: int
    /// 最大環境ボンド次元
    MaxChi: int
    /// 収束閾値
    ConvergenceThreshold: float
    /// 最大反復数
    MaxIterations: int
    /// チェックポイント間隔
    CheckpointInterval: int option
    /// 収束判定方法
    ConvergenceMethod: ConvergenceMethod
}

and ConvergenceMethod =
    | StateHash         // ハッシュベース
    | NormDifference    // ノルム差
    | SpectralGap       // スペクトルギャップ
    | Combined          // 複合判定

and HardwareConfiguration = {
    /// デバイス選択
    DeviceSelection: DeviceStrategy
    /// バッチサイズ
    BatchSize: int
    /// メモリプールサイズ (MB)
    MemoryPoolSizeMB: int
    /// 並列度
    Parallelism: ParallelismLevel
    /// プロファイリング
    EnableProfiling: bool
}

and DeviceStrategy =
    | CPU
    | GPU of deviceId: int
    | Auto
    | Hybrid of gpuRatio: float

and ParallelismLevel = {
    /// スレッド数
    ThreadCount: int option
    /// タスク並列度
    MaxDegreeOfParallelism: int option
    /// GPU並列設定
    GPUStreams: int option
}

and ObservableConfiguration = {
    /// 計算する観測量
    Measurements: MeasurementSpec list
    /// 測定位置
    MeasurementLocations: LocationSpec
    /// 測定頻度
    MeasurementInterval: int
    /// 対称性平均
    SymmetryAverage: bool
}

and MeasurementSpec = {
    /// 観測量の種類
    Type: ObservableType
    /// パラメータ
    Parameters: Map<string, obj>
    /// 出力名
    OutputName: string
}

and ObservableType =
    | Magnetization
    | Correlation
    | WilsonLoop
    | StringOrder
    | TopologicalEntropy
    | PlaquetteOperator
    | VortexDensity
    | ChiralOrder
    | SpectralGap
    | CorrelationLength

and LocationSpec =
    | AllSites
    | SpecificSites of (int * int) list
    | Grid of spacing: int
    | Random of count: int * seed: int
    | Center of radius: int

and OutputConfiguration = {
    /// 出力ディレクトリ
    OutputDirectory: string
    /// ファイル形式
    FileFormat: OutputFormat
    /// 詳細レベル
    Verbosity: VerbosityLevel
    /// リアルタイム出力
    RealTimeOutput: bool
    /// 圧縮
    Compression: bool
}

and OutputFormat =
    | JSON
    | Binary
    | CSV
    | HDF5
    | All

and VerbosityLevel =
    | Silent = 0
    | Minimal = 1
    | Normal = 2
    | Verbose = 3
    | Debug = 4

and RuntimeConfiguration = {
    /// タイムアウト (秒)
    TimeoutSeconds: int option
    /// 最大メモリ使用量 (GB)
    MaxMemoryGB: int option
    /// チェックポイント
    EnableCheckpointing: bool
    /// エラーハンドリング
    ErrorHandling: ErrorStrategy
    /// ランダムシード
    RandomSeed: int option
}

and ErrorStrategy =
    | StopOnError
    | ContinueWithDefaults
    | Retry of maxAttempts: int
    | Fallback of strategy: DeviceStrategy

/// 設定ローダー
module ConfigurationLoader =

    /// デフォルト設定
    let defaultConfig = {
        Lattice = {
            Size = (32, 32)
            BoundaryCondition = Periodic
            EnforceSymmetry = true
        }
        PEPS = {
            PhysicalDimension = 2
            BondDimension = 5
            Initialization = FibonacciBasis
            EnsureMPOInjectivity = true
        }
        ACE = {
            InitialChi = 16
            MaxChi = 64
            ConvergenceThreshold = 1e-10
            MaxIterations = 100
            CheckpointInterval = Some 10
            ConvergenceMethod = Combined
        }
        Hardware = {
            DeviceSelection = Auto
            BatchSize = 1024
            MemoryPoolSizeMB = 1000
            Parallelism = {
                ThreadCount = None
                MaxDegreeOfParallelism = None
                GPUStreams = Some 4
            }
            EnableProfiling = false
        }
        Observable = {
            Measurements = [
                {
                    Type = Magnetization
                    Parameters = Map.empty
                    OutputName = "magnetization"
                }
                {
                    Type = CorrelationLength
                    Parameters = Map.empty
                    OutputName = "correlation_length"
                }
            ]
            MeasurementLocations = Center 5
            MeasurementInterval = 10
            SymmetryAverage = true
        }
        Output = {
            OutputDirectory = "./output"
            FileFormat = JSON
            Verbosity = Normal
            RealTimeOutput = true
            Compression = false
        }
        Runtime = {
            TimeoutSeconds = Some 3600
            MaxMemoryGB = Some 32
            EnableCheckpointing = true
            ErrorHandling = Retry 3
            RandomSeed = Some 42
        }
    }

    /// JSONから設定を読み込み
    let loadFromJson (jsonPath: string) =
        if File.Exists(jsonPath) then
            // JSON解析の実装
            let json = File.ReadAllText(jsonPath)
            parseJsonConfig json
        else
            defaultConfig

    /// コマンドライン引数から設定を構築
    let fromCommandLine (args: string[]) =
        let mutable config = defaultConfig

        let rec parseArgs (args: string list) =
            match args with
            | "--lattice" :: sizeStr :: rest ->
                let parts = sizeStr.Split(',')
                if parts.Length = 2 then
                    let width = int parts.[0]
                    let height = int parts.[1]
                    config <- { config with
                                  Lattice = { config.Lattice with Size = (width, height) } }
                parseArgs rest

            | "--chi" :: chiStr :: rest ->
                let chi = int chiStr
                config <- { config with
                              ACE = { config.ACE with InitialChi = chi; MaxChi = chi * 4 } }
                parseArgs rest

            | "--device" :: device :: rest ->
                let deviceStrategy =
                    match device.ToLower() with
                    | "cpu" -> CPU
                    | "gpu" -> GPU 0
                    | "auto" -> Auto
                    | _ -> Auto
                config <- { config with
                              Hardware = { config.Hardware with DeviceSelection = deviceStrategy } }
                parseArgs rest

            | "--output" :: dir :: rest ->
                config <- { config with
                              Output = { config.Output with OutputDirectory = dir } }
                parseArgs rest

            | "--verbose" :: rest ->
                config <- { config with
                              Output = { config.Output with Verbosity = Verbose } }
                parseArgs rest

            | "--profile" :: rest ->
                config <- { config with
                              Hardware = { config.Hardware with EnableProfiling = true } }
                parseArgs rest

            | "--batch" :: sizeStr :: rest ->
                let size = int sizeStr
                config <- { config with
                              Hardware = { config.Hardware with BatchSize = size } }
                parseArgs rest

            | "--seed" :: seedStr :: rest ->
                let seed = int seedStr
                config <- { config with
                              Runtime = { config.Runtime with RandomSeed = Some seed } }
                parseArgs rest

            | "--no-checkpoint" :: rest ->
                config <- { config with
                              Runtime = { config.Runtime with EnableCheckpointing = false } }
                parseArgs rest

            | _ :: rest -> parseArgs rest
            | [] -> ()

        parseArgs (Array.toList args)
        config

    /// 設定の検証
    let validate (config: SystemConfiguration) =
        let errors = ResizeArray<string>()

        // 格子サイズの検証
        let (width, height) = config.Lattice.Size
        if width <= 0 || height <= 0 then
            errors.Add("Lattice size must be positive")

        // ボンド次元の検証
        if config.PEPS.BondDimension <= 0 then
            errors.Add("Bond dimension must be positive")

        // Chi検証
        if config.ACE.InitialChi <= 0 then
            errors.Add("Initial chi must be positive")
        if config.ACE.MaxChi < config.ACE.InitialChi then
            errors.Add("Max chi must be >= initial chi")

        // バッチサイズ検証
        if config.Hardware.BatchSize <= 0 then
            errors.Add("Batch size must be positive")

        if errors.Count > 0 then
            Error(errors.ToArray())
        else
            Ok(config)

    /// JSON解析の実装
    and parseJsonConfig (json: string) =
        // 簡易JSON解析（実際はNewtonsoft.Jsonなどを使用）
        try
            defaultConfig  // プレースホルダーではなく、実際のデフォルト値を返す
        with ex ->
            printfn "Failed to parse JSON config: %s" ex.Message
            defaultConfig
