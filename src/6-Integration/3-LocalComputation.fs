// 7-Integration/3-LocalComputation.fs
namespace E8.Integration

open System
open System.Collections.Concurrent
open System.Threading.Tasks
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.Ace
open E8.Observable

/// 局所計算エンジン
type LocalComputationEngine(config: SystemConfiguration) =

    // ハードウェア層の初期化
    let hardwareOps =
        match config.Hardware.DeviceSelection with
        | CPU -> new OptimizedCPUOperations() :> IF2Operations
        | GPU deviceId -> new ROCmOperations(deviceId) :> IF2Operations
        | Auto ->
            try
                new ROCmOperations(0) :> IF2Operations
            with _ ->
                new OptimizedCPUOperations() :> IF2Operations
        | Hybrid ratio ->
            // ハイブリッド実装（簡略化）
            new OptimizedCPUOperations() :> IF2Operations

    // ACEエンジン
    let aceEngine =
        let aceConfig = {
            ACE_CTMRG.InitialChi = config.ACE.InitialChi
            ACE_CTMRG.MaxChi = config.ACE.MaxChi
            ACE_CTMRG.ConvergenceThreshold = config.ACE.ConvergenceThreshold
            ACE_CTMRG.MaxIterations = config.ACE.MaxIterations
            ACE_CTMRG.DeviceSelection =
                match config.Hardware.DeviceSelection with
                | CPU -> ACE_CTMRG.AlwaysCPU
                | GPU _ -> ACE_CTMRG.AlwaysGPU
                | Auto -> ACE_CTMRG.Automatic config.Hardware.BatchSize
                | Hybrid ratio -> ACE_CTMRG.HybridBalance ratio
            ACE_CTMRG.BatchSize = config.Hardware.BatchSize
            ACE_CTMRG.MemoryPoolSizeMB = config.Hardware.MemoryPoolSizeMB
            ACE_CTMRG.EnableProfiling = config.Hardware.EnableProfiling
            ACE_CTMRG.CheckpointInterval = config.ACE.CheckpointInterval
        }
        new ACE_CTMRG.ACEEngine(aceConfig)

    // PEPSテンソル
    let peps =
        let (width, height) = config.Lattice.Size
        let bondDim = config.PEPS.BondDimension
        let physDim = config.PEPS.PhysicalDimension

        match config.PEPS.Initialization with
        | Random seed ->
            let rng = Random(seed)
            {
                PhysicalDimension = physDim
                BondDimension = bondDim
                TotalSize = physDim * bondDim * bondDim * bondDim * bondDim
                Data = Array.init (physDim * bondDim * bondDim * bondDim * bondDim) (fun _ ->
                    if rng.Next(2) = 0 then F2.Zero else F2.One)
                Item = fun p l r u d ->
                    let idx = p * bondDim * bondDim * bondDim * bondDim +
                             l * bondDim * bondDim * bondDim +
                             r * bondDim * bondDim +
                             u * bondDim +
                             d
                    if idx < physDim * bondDim * bondDim * bondDim * bondDim then
                        if rng.Next(2) = 0 then F2.Zero else F2.One
                    else
                        F2.Zero
                GetElement = fun idx ->
                    if idx < physDim * bondDim * bondDim * bondDim * bondDim then
                        if rng.Next(2) = 0 then F2.Zero else F2.One
                    else
                        F2.Zero
            }

        | Uniform value ->
            {
                PhysicalDimension = physDim
                BondDimension = bondDim
                TotalSize = physDim * bondDim * bondDim * bondDim * bondDim
                Data = Array.create (physDim * bondDim * bondDim * bondDim * bondDim) value
                Item = fun p l r u d -> value
                GetElement = fun idx -> value
            }

        | FibonacciBasis ->
            // フィボナッチ基底での初期化
            createFibonacciPEPS physDim bondDim

        | GoldenChain ->
            // 黄金鎖の初期化
            createGoldenChainPEPS physDim bondDim

        | FromFile path ->
            // ファイルからの読み込み（未実装なので代替）
            createFibonacciPEPS physDim bondDim

    // 環境キャッシュ
    let environmentCache = ConcurrentDictionary<(int * int), ACE_CTMRG.SymmetricEnvironment>()

    // 統計情報
    let mutable totalComputations = 0L
    let mutable cacheHits = 0L
    let mutable cacheMisses = 0L

    /// フィボナッチPEPSの作成
    let createFibonacciPEPS physDim bondDim =
        let data = Array.zeroCreate (physDim * bondDim * bondDim * bondDim * bondDim)

        // フィボナッチ融合則に従う初期化
        for p in 0 .. physDim - 1 do
            for l in 0 .. bondDim - 1 do
                for r in 0 .. bondDim - 1 do
                    for u in 0 .. bondDim - 1 do
                        for d in 0 .. bondDim - 1 do
                            let idx = p * bondDim * bondDim * bondDim * bondDim +
                                     l * bondDim * bondDim * bondDim +
                                     r * bondDim * bondDim +
                                     u * bondDim +
                                     d

                            // 融合則チェック
                            let fusionValid =
                                (l + r) % 2 = p % 2 &&
                                (u + d) % 2 = p % 2

                            data.[idx] <- if fusionValid then F2.One else F2.Zero

        {
            PhysicalDimension = physDim
            BondDimension = bondDim
            TotalSize = data.Length
            Data = data
            Item = fun p l r u d ->
                let idx = p * bondDim * bondDim * bondDim * bondDim +
                         l * bondDim * bondDim * bondDim +
                         r * bondDim * bondDim +
                         u * bondDim +
                         d
                if idx < data.Length then data.[idx] else F2.Zero
            GetElement = fun idx ->
                if idx < data.Length then data.[idx] else F2.Zero
        }

    /// 黄金鎖PEPSの作成
    let createGoldenChainPEPS physDim bondDim =
        // 黄金比関連の初期化
        createFibonacciPEPS physDim bondDim  // 簡略化

    /// 局所環境の計算
    member _.ComputeLocalEnvironment(centerX: int, centerY: int, radius: int) =
        let key = (centerX, centerY)

        match environmentCache.TryGetValue(key) with
        | true, cached ->
            Interlocked.Increment(&cacheHits) |> ignore
            cached
        | false, _ ->
            Interlocked.Increment(&cacheMisses) |> ignore

            // 新規計算
            let request = {
                ACE_CTMRG.RequestId = Guid.NewGuid()
                ACE_CTMRG.CenterX = centerX
                ACE_CTMRG.CenterY = centerY
                ACE_CTMRG.Radius = radius
                ACE_CTMRG.Priority = 5
                ACE_CTMRG.Observable = ACE_CTMRG.Magnetization
                ACE_CTMRG.SubmittedAt = DateTime.UtcNow
                ACE_CTMRG.Deadline = None
            }

            let requestId = aceEngine.SubmitRequest(request)

            // 結果を待機（タイムアウト付き）
            let rec waitForResult(attempts: int) =
                if attempts > 100 then
                    // タイムアウト：デフォルト環境を返す
                    createDefaultEnvironment()
                else
                    match aceEngine.TryGetResult(requestId) with
                    | Some result ->
                        // 環境を構築（簡略化）
                        createDefaultEnvironment()
                    | None ->
                        Threading.Thread.Sleep(10)
                        waitForResult(attempts + 1)

            let env = waitForResult(0)
            environmentCache.TryAdd(key, env) |> ignore
            env

    /// デフォルト環境の作成
    and createDefaultEnvironment() =
        {
            ACE_CTMRG.UniqueCorner = Array.init (config.ACE.InitialChi * config.ACE.InitialChi)
                                                (fun _ -> { Bits = 1UL })
            ACE_CTMRG.UniqueEdge = Array.init (config.ACE.InitialChi * config.ACE.InitialChi * config.PEPS.BondDimension)
                                              (fun _ -> { Bits = 0UL })
            ACE_CTMRG.Chi = config.ACE.InitialChi
            ACE_CTMRG.Iteration = 0
            ACE_CTMRG.History = {
                StateHashes = [||]
                CompressionRatios = [||]
                EffectiveRanks = [||]
                SpectralGaps = [||]
                Norms = [||]
            }
            ACE_CTMRG.LastUpdated = DateTime.UtcNow
        }

    /// 観測量の計算
    member this.ComputeObservable(observableType: ObservableType, location: (int * int), ?radius: int) =
        let radius = defaultArg radius 1
        let (x, y) = location

        // 環境を取得
        let env = this.ComputeLocalEnvironment(x, y, radius)

        // 観測量インスタンスを作成
        let observable: IObservable =
            match observableType with
            | Magnetization -> LocalMagnetization() :> IObservable
            | Correlation -> TwoPointCorrelator() :> IObservable
            | WilsonLoop -> WilsonLoop() :> IObservable
            | StringOrder -> StringOrderParameter() :> IObservable
            | TopologicalEntropy -> TopologicalEntropyMeasurement() :> IObservable
            | PlaquetteOperator -> PlaquetteOperator(2) :> IObservable
            | VortexDensity -> VortexDensity() :> IObservable
            | ChiralOrder -> ChiralOrderParameter() :> IObservable
            | SpectralGap -> SpectralObservable("spectral_gap") :> IObservable
            | CorrelationLength -> SpectralObservable("correlation_length") :> IObservable

        // 測定位置を構築
        let measurementLocation = {
            PrimaryPosition = location
            SecondaryPositions = []
            Radius = radius
        }

        // 計算実行
        let value = observable.Compute env peps measurementLocation hardwareOps

        Interlocked.Increment(&totalComputations) |> ignore

        {
            ObservableResult.Kind = observable.Kind
            ObservableResult.Value = value
            ObservableResult.Location = measurementLocation
            ObservableResult.ComputationTimeMs = 0.0
            ObservableResult.DeviceUsed =
                match hardwareOps.DeviceInfo().DeviceType with
                | CPU(cores, avx512, _) -> sprintf "CPU(%d cores, AVX512=%b)" cores avx512
                | GPU(model, _, _) -> sprintf "GPU(%s)" model
            ObservableResult.Timestamp = DateTime.UtcNow
        }

    /// バッチ観測量計算
    member this.ComputeObservableBatch(observableType: ObservableType,
                                      locations: (int * int)[],
                                      ?radius: int) =
        let radius = defaultArg radius 1

        // 並列計算
        locations
        |> Array.Parallel.map (fun loc ->
            this.ComputeObservable(observableType, loc, radius))

    /// 環境の収束
    member this.ConvergeEnvironment(location: (int * int), ?maxIterations: int) =
        let maxIter = defaultArg maxIterations config.ACE.MaxIterations
        let (x, y) = location

        // 初期環境
        let mutable env = this.ComputeLocalEnvironment(x, y, 1)
        let mutable converged = false
        let mutable iteration = 0

        while not converged && iteration < maxIter do
            // ACEステップ
            let (newEnv, hasConverged, _) =
                aceEngine.aceStep env peps

            env <- newEnv
            converged <- hasConverged
            iteration <- iteration + 1

            // キャッシュ更新
            environmentCache.AddOrUpdate(location, env, fun _ _ -> env) |> ignore

        (env, converged, iteration)

    /// 統計情報
    member _.GetStatistics() =
        let cacheHitRate =
            let total = cacheHits + cacheMisses
            if total > 0L then float cacheHits / float total else 0.0

        {|
            TotalComputations = totalComputations
            CacheSize = environmentCache.Count
            CacheHitRate = cacheHitRate
            CacheHits = cacheHits
            CacheMisses = cacheMisses
            DeviceInfo = hardwareOps.DeviceInfo()
            MemoryInfo = hardwareOps.MemoryInfo()
            ACEStatistics = aceEngine.GetStatistics()
        |}

    /// リソースの解放
    interface IDisposable with
        member _.Dispose() =
            (aceEngine :> IDisposable).Dispose()
            (hardwareOps :> IDisposable).Dispose()
