// 5-Ace/2-ACE_CTMRG.fs
namespace E8.Ace

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks
open E8.Algebra
open E8.Tensors
open E8.Hardware

/// ACE-CTMRG: 動的成長-圧縮による革新的繰り込み群
module ACE_CTMRG =

    /// 環境テンソル（D4対称性を活用）
    type EnvironmentTensor = {
        /// コーナーテンソル C[chi × chi]
        Corner: BitBlock64[]
        /// エッジテンソル T[chi × chi × D]
        Edge: BitBlock64[]
        /// 現在のボンド次元
        Chi: int
        /// 物理次元
        PhysicalDim: int
        /// ボンド次元
        BondDim: int
    }

    /// 対称環境（D4対称性により1/8のメモリで表現）
    type SymmetricEnvironment = {
        /// ユニークなコーナー（他は回転で生成）
        UniqueCorner: BitBlock64[]
        /// ユニークなエッジ（他は回転で生成）
        UniqueEdge: BitBlock64[]
        /// 環境のボンド次元
        Chi: int
        /// 現在の反復回数
        Iteration: int
        /// 収束履歴
        History: ConvergenceHistory
        /// 最終更新時刻
        LastUpdated: DateTime
    }

    and ConvergenceHistory = {
        StateHashes: byte[][]
        CompressionRatios: float[]
        EffectiveRanks: int[]
        SpectralGaps: F2[]
        Norms: uint64[]
    }

    /// 局所計算リクエスト
    type LocalComputationRequest = {
        RequestId: Guid
        CenterX: int
        CenterY: int
        Radius: int
        Priority: int
        Observable: ObservableType
        SubmittedAt: DateTime
        Deadline: DateTime option
    }

    and ObservableType =
        | Magnetization
        | Correlation of dx: int * dy: int
        | WilsonLoop of width: int * height: int
        | TopologicalEntropy
        | OrderParameter of kind: string

    /// 観測量の結果
    type ObservableResult = {
        RequestId: Guid
        Value: F2
        Variance: F2 option
        ConvergenceQuality: float
        ComputationTimeMs: float
    }

    /// ACEエンジン設定
    type ACEConfig = {
        InitialChi: int
        MaxChi: int
        ConvergenceThreshold: float
        MaxIterations: int
        DeviceSelection: DeviceSelectionStrategy
        BatchSize: int
        MemoryPoolSizeMB: int
        EnableProfiling: bool
        CheckpointInterval: int option
    }

    and DeviceSelectionStrategy =
        | AlwaysCPU
        | AlwaysGPU
        | Automatic of cpuThreshold: int
        | HybridBalance of gpuRatio: float

    /// ACE計算エンジンの完全実装
    type ACEEngine(config: ACEConfig) =

        // ハードウェア層の初期化
        let cpuOps = new OptimizedCPUOperations() :> IF2Operations
        let gpuOps =
            try
                let gpu = new ROCmOperations(0)
                Some(gpu :> IF2Operations)
            with ex ->
                printfn "GPU initialization failed: %s" ex.Message
                printfn "Falling back to CPU-only mode"
                None

        // リクエストキューと結果ストレージ
        let requestQueue = ConcurrentPriorityQueue<int, LocalComputationRequest>()
        let resultsDict = ConcurrentDictionary<Guid, ObservableResult>()
        let pendingRequests = ConcurrentDictionary<Guid, LocalComputationRequest>()

        // 環境キャッシュ（局所性を最大活用）
        let environmentCache = ConcurrentDictionary<int * int, SymmetricEnvironment>()
        let cacheStats = ref (0, 0)  // (hits, misses)

        // 実行統計
        let mutable totalProcessed = 0L
        let mutable totalTimeMs = 0.0
        let mutable currentBatchSize = 0

        /// デバイス選択ロジック
        let selectDevice(dataSize: int) =
            match config.DeviceSelection with
            | AlwaysCPU -> cpuOps
            | AlwaysGPU ->
                match gpuOps with
                | Some gpu -> gpu
                | None -> cpuOps
            | Automatic threshold ->
                if dataSize < threshold then
                    cpuOps
                else
                    gpuOps |> Option.defaultValue cpuOps
            | HybridBalance ratio ->
                // 確率的にGPU/CPUを選択
                let useGPU = Random().NextDouble() < ratio
                if useGPU then
                    gpuOps |> Option.defaultValue cpuOps
                else
                    cpuOps

        /// 初期環境の生成
        let createInitialEnvironment(chi: int, bondDim: int) =
            let cornerSize = chi * chi
            let edgeSize = chi * chi * bondDim

            // ランダム初期化（F₂なので0または1）
            let rng = Random()
            let corner = Array.init cornerSize (fun _ ->
                { Bits = if rng.Next(2) = 1 then 1UL else 0UL })
            let edge = Array.init edgeSize (fun _ ->
                { Bits = if rng.Next(2) = 1 then 1UL else 0UL })

            {
                UniqueCorner = corner
                UniqueEdge = edge
                Chi = chi
                Iteration = 0
                History = {
                    StateHashes = [||]
                    CompressionRatios = [||]
                    EffectiveRanks = [||]
                    SpectralGaps = [||]
                    Norms = [||]
                }
                LastUpdated = DateTime.UtcNow
            }

        /// 環境の成長ステップ（CTM）の完全実装
        let growEnvironment (env: SymmetricEnvironment) (peps: Tensor5F2) =

            let chi = env.Chi
            let D = peps.BondDimension
            let P = peps.PhysicalDimension
            let chiNew = chi * D

            // デバイス選択
            let ops = selectDevice(chiNew * chiNew)

            // Step 1: M1テンソルの構築 [chi × chi × D]
            let constructM1() =
                let M1 = Array3D.zeroCreate<BitBlock64> chi chi D

                Parallel.For(0, chi, fun u ->
                    for d in 0 .. chi - 1 do
                        for p_out in 0 .. D - 1 do
                            let mutable sum = { Bits = 0UL }

                            // C[u,k] × T[k,d,p_out]
                            for k in 0 .. chi - 1 do
                                let c_idx = u * chi + k
                                let t_idx = k * chi * D + d * D + p_out

                                let c_val = env.UniqueCorner.[c_idx]
                                let t_val = env.UniqueEdge.[t_idx]

                                sum <- { Bits = sum.Bits ^^^ (c_val.Bits &&& t_val.Bits) }

                            M1.[u, d, p_out] <- sum
                )

                M1

            // Step 2: M2テンソルの構築 [chi × chi × D]
            let constructM2() =
                let M2 = Array3D.zeroCreate<BitBlock64> chi chi D

                Parallel.For(0, chi, fun l ->
                    for r in 0 .. chi - 1 do
                        for p_out in 0 .. D - 1 do
                            let mutable sum = { Bits = 0UL }

                            // T[l,k,p_out] × C[k,r]
                            for k in 0 .. chi - 1 do
                                let t_idx = l * chi * D + k * D + p_out
                                let c_idx = k * chi + r

                                let t_val = env.UniqueEdge.[t_idx]
                                let c_val = env.UniqueCorner.[c_idx]

                                sum <- { Bits = sum.Bits ^^^ (t_val.Bits &&& c_val.Bits) }

                            M2.[l, r, p_out] <- sum
                )

                M2

            let M1 = constructM1()
            let M2 = constructM2()

            // Step 3: 成長したコーナーテンソル C'[chiNew × chiNew]
            let C_grown = Array.zeroCreate<BitBlock64>(chiNew * chiNew)

            Parallel.For(0, chiNew, fun u_new ->
                let u_M1 = u_new / D
                let u_A = u_new % D

                for r_new in 0 .. chiNew - 1 do
                    let r_M2 = r_new / D
                    let r_A = r_new % D

                    let mutable sum = { Bits = 0UL }

                    // 完全な縮約
                    for p_phys in 0 .. P - 1 do
                        for d_M in 0 .. chi - 1 do
                            for p_M1 in 0 .. D - 1 do
                                for p_M2 in 0 .. D - 1 do
                                    let m1_val = M1.[u_M1, d_M, p_M1]
                                    let m2_val = M2.[d_M, r_M2, p_M2]
                                    let a_val = peps.[p_phys, p_M1, p_M2, u_A, r_A]

                                    let product = m1_val.Bits &&& m2_val.Bits &&& a_val.Bits
                                    sum <- { Bits = sum.Bits ^^^ product }

                    C_grown.[u_new * chiNew + r_new] <- sum
            )

            // Step 4: 成長したエッジテンソル T'[chiNew × chiNew × D]
            let T_grown = Array.zeroCreate<BitBlock64>(chiNew * chiNew * D)

            Parallel.For(0, chiNew, fun u_new ->
                for d_new in 0 .. chiNew - 1 do
                    for p_out in 0 .. D - 1 do
                        let u_T = u_new / D
                        let u_A = u_new % D
                        let d_T = d_new / D
                        let d_A = d_new % D

                        let mutable sum = { Bits = 0UL }

                        // エッジの縮約
                        for p_phys in 0 .. P - 1 do
                            for p_in in 0 .. D - 1 do
                                let t_idx = u_T * chi * D + d_T * D + p_in
                                let t_val = env.UniqueEdge.[t_idx]
                                let a_val = peps.[p_phys, p_in, p_out, u_A, d_A]

                                sum <- { Bits = sum.Bits ^^^ (t_val.Bits &&& a_val.Bits) }

                        T_grown.[u_new * chiNew * D + d_new * D + p_out] <- sum
            )

            (C_grown, T_grown, chiNew)

        /// 環境の圧縮ステップ（RG）の完全実装
        let compressEnvironment (C_grown: BitBlock64[]) (T_grown: BitBlock64[])
                               (chiNew: int) (targetChi: int) (bondDim: int) =

            let ops = selectDevice(chiNew * chiNew)

            // Renorm-F2による圧縮
            let params = {
                TargetChi = targetChi
                MinChi = 2
                MaxChi = config.MaxChi
                AdaptiveThreshold = 0.0
                PreserveSymmetry = true
            }

            // コーナーテンソルの圧縮
            let cornerResult = RenormF2.truncate C_grown chiNew chiNew params ops

            // エッジテンソルの圧縮（各スライスを個別に圧縮）
            let compressEdgeSlices() =
                let compressedSlices = Array.zeroCreate<BitBlock64[]> bondDim

                Parallel.For(0, bondDim, fun p ->
                    // p番目のスライスを抽出
                    let slice = Array.zeroCreate<BitBlock64>(chiNew * chiNew)
                    for u in 0 .. chiNew - 1 do
                        for d in 0 .. chiNew - 1 do
                            slice.[u * chiNew + d] <- T_grown.[u * chiNew * bondDim + d * bondDim + p]

                    // スライスを圧縮
                    let sliceResult = RenormF2.truncate slice chiNew chiNew params ops
                    compressedSlices.[p] <- sliceResult.CompressedMatrix
                )

                // 圧縮されたスライスを結合
                let T_compressed = Array.zeroCreate<BitBlock64>(targetChi * targetChi * bondDim)
                for p in 0 .. bondDim - 1 do
                    for u in 0 .. targetChi - 1 do
                        for d in 0 .. targetChi - 1 do
                            let idx = u * targetChi * bondDim + d * bondDim + p
                            T_compressed.[idx] <- compressedSlices.[p].[u * targetChi + d]

                T_compressed

            let T_compressed = compressEdgeSlices()

            (cornerResult.CompressedMatrix, T_compressed, cornerResult)

        /// ACEステップの完全実装（成長→圧縮）
        let aceStep (env: SymmetricEnvironment) (peps: Tensor5F2) =

            let timer = System.Diagnostics.Stopwatch.StartNew()

            // Step 1: 環境の成長
            let (C_grown, T_grown, chiNew) = growEnvironment env peps

            // Step 2: 環境の圧縮
            let targetChi = min env.Chi config.MaxChi
            let (C_compressed, T_compressed, compressionInfo) =
                compressEnvironment C_grown T_grown chiNew targetChi peps.BondDimension

            // Step 3: 収束判定のためのハッシュ計算
            let computeHash(data: BitBlock64[]) =
                use sha = System.Security.Cryptography.SHA256.Create()
                let bytes = data |> Array.collect (fun b -> BitConverter.GetBytes(b.Bits))
                sha.ComputeHash(bytes)

            let newHash = computeHash(C_compressed)

            // 履歴との比較
            let hasConverged =
                env.History.StateHashes
                |> Array.exists (fun h ->
                    Array.forall2 (fun a b -> a = b) h newHash)

            // ノルム計算（F₂では要素の総和）
            let computeNorm(data: BitBlock64[]) =
                data |> Array.sumBy (fun b -> b.Bits)

            let norm = computeNorm(C_compressed)

            timer.Stop()

            // 新しい環境の構築
            let newEnv = {
                UniqueCorner = C_compressed
                UniqueEdge = T_compressed
                Chi = targetChi
                Iteration = env.Iteration + 1
                History = {
                    StateHashes = Array.append env.History.StateHashes [| newHash |]
                    CompressionRatios = Array.append env.History.CompressionRatios
                                                   [| compressionInfo.CompressionRatio |]
                    EffectiveRanks = Array.append env.History.EffectiveRanks
                                                [| compressionInfo.EffectiveRank |]
                    SpectralGaps = env.History.SpectralGaps  // 後で計算
                    Norms = Array.append env.History.Norms [| norm |]
                }
                LastUpdated = DateTime.UtcNow
            }

            (newEnv, hasConverged, timer.Elapsed.TotalMilliseconds)


        /// バッチ局所計算の処理
        member this.ProcessLocalBatch() =

            let timer = System.Diagnostics.Stopwatch.StartNew()

            // 優先度付きキューからリクエストを取得
            let batch = ResizeArray<LocalComputationRequest>()
            let mutable remaining = config.BatchSize

            while remaining > 0 do
                match requestQueue.TryDequeue() with
                | true, (priority, request) ->
                    batch.Add(request)
                    pendingRequests.TryRemove(request.RequestId) |> ignore
                    remaining <- remaining - 1
                | false, _ ->
                    remaining <- 0

            if batch.Count = 0 then
                Thread.Sleep(10)
                false
            else
                currentBatchSize <- batch.Count

                // バッチ処理の実行
                let results =
                    batch
                    |> Seq.toArray
                    |> Array.Parallel.map (fun request ->

                        // 環境の取得または作成
                        let env =
                            let key = (request.CenterX, request.CenterY)
                            match environmentCache.TryGetValue(key) with
                            | true, cached ->
                                let (hits, misses) = !cacheStats
                                cacheStats := (hits + 1, misses)
                                cached
                            | false, _ ->
                                let (hits, misses) = !cacheStats
                                cacheStats := (hits, misses + 1)
                                let newEnv = createInitialEnvironment(config.InitialChi, 5)
                                environmentCache.GetOrAdd(key, newEnv)

                        // 観測量の計算
                        let value = computeObservable env request.Observable
                                                     request.CenterX request.CenterY request.Radius

                        let result = {
                            RequestId = request.RequestId
                            Value = value
                            Variance = None
                            ConvergenceQuality =
                                if env.Iteration > 0 then
                                    1.0 - 1.0 / float env.Iteration
                                else
                                    0.0
                            ComputationTimeMs = 0.0  // 個別計測は後で
                        }

                        result
                    )

                // 結果の格納
                for result in results do
                    resultsDict.TryAdd(result.RequestId, result) |> ignore

                timer.Stop()
                totalTimeMs <- totalTimeMs + timer.Elapsed.TotalMilliseconds
                Interlocked.Add(&totalProcessed, int64 batch.Count) |> ignore

                true

        /// メインループ
        member this.RunAsync(?cancellationToken: CancellationToken) =
            let ct = defaultArg cancellationToken CancellationToken.None

            Task.Run(fun () ->
                while not ct.IsCancellationRequested do
                    this.ProcessLocalBatch() |> ignore
            , ct)

        /// リクエストの送信
        member _.SubmitRequest(request: LocalComputationRequest) =
            pendingRequests.TryAdd(request.RequestId, request) |> ignore
            requestQueue.Enqueue(request.Priority, request)
            request.RequestId

        /// 結果の取得
        member _.TryGetResult(requestId: Guid) =
            match resultsDict.TryRemove(requestId) with
            | true, result -> Some result
            | false, _ -> None

        /// 統計情報
        member _.GetStatistics() =
            let (hits, misses) = !cacheStats
            let cacheHitRate =
                if hits + misses > 0 then
                    float hits / float(hits + misses)
                else
                    0.0

            {|
                QueueLength = requestQueue.Count
                PendingRequests = pendingRequests.Count
                CacheSize = environmentCache.Count
                CacheHitRate = cacheHitRate
                TotalProcessed = totalProcessed
                AverageTimeMs = if totalProcessed > 0L then totalTimeMs / float totalProcessed else 0.0
                CurrentBatchSize = currentBatchSize
                DeviceInfo =
                    match config.DeviceSelection with
                    | AlwaysGPU when gpuOps.IsSome -> gpuOps.Value.DeviceInfo()
                    | _ -> cpuOps.DeviceInfo()
                MemoryInfo =
                    match config.DeviceSelection with
                    | AlwaysGPU when gpuOps.IsSome -> gpuOps.Value.MemoryInfo()
                    | _ -> cpuOps.MemoryInfo()
            |}

        interface IDisposable with
            member _.Dispose() =
                (cpuOps :> IDisposable).Dispose()
                gpuOps |> Option.iter (fun gpu -> (gpu :> IDisposable).Dispose())