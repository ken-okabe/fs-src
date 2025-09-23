// 7-Integration/2-RequestQueue.fs
namespace E8.Integration

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks
open E8.Ace
open E8.Observable

/// 優先度付きリクエストキュー
type PriorityRequestQueue<'T>() =

    // 優先度付きキュー（高優先度が先）
    let queues =
        [| for _ in 0 .. 9 -> ConcurrentQueue<'T>() |]

    let mutable totalCount = 0
    let semaphore = new SemaphoreSlim(0)

    /// リクエストを追加
    member _.Enqueue(priority: int, item: 'T) =
        let priority = max 0 (min 9 priority)
        queues.[9 - priority].Enqueue(item)
        Interlocked.Increment(&totalCount) |> ignore
        semaphore.Release() |> ignore

    /// リクエストを取得（ブロッキング）
    member _.DequeueAsync(?timeout: TimeSpan) =
        async {
            let timeout = defaultArg timeout TimeSpan.MaxValue
            let! success = semaphore.WaitAsync(timeout) |> Async.AwaitTask

            if success then
                // 高優先度から順に探索
                for queue in queues do
                    match queue.TryDequeue() with
                    | true, item ->
                        Interlocked.Decrement(&totalCount) |> ignore
                        return Some item
                    | false, _ -> ()
                return None
            else
                return None
        }

    /// リクエストを取得（非ブロッキング）
    member _.TryDequeue() =
        if totalCount > 0 then
            for queue in queues do
                match queue.TryDequeue() with
                | true, item ->
                    Interlocked.Decrement(&totalCount) |> ignore
                    return Some item
                | false, _ -> ()
            None
        else
            None

    /// 現在のキューサイズ
    member _.Count = totalCount

    /// 優先度別のカウント
    member _.GetPriorityCounts() =
        [| for i in 0 .. 9 ->
            (9 - i, queues.[i].Count) |]

/// 計算リクエスト
type ComputationRequest = {
    /// リクエストID
    Id: Guid
    /// リクエストタイプ
    Type: RequestType
    /// 優先度 (0-9, 9が最高)
    Priority: int
    /// タイムスタンプ
    SubmittedAt: DateTime
    /// デッドライン
    Deadline: DateTime option
    /// コールバック
    Callback: ComputationResult -> unit
}

and RequestType =
    | EnvironmentUpdate of location: (int * int)
    | ObservableMeasurement of observable: ObservableKind * location: MeasurementLocation
    | SpectralAnalysis of construction: ProjectorSpectral.TransferMatrixConstruction
    | Checkpoint
    | Shutdown

and ComputationResult =
    | EnvironmentResult of ACE_CTMRG.SymmetricEnvironment
    | ObservableResult of ObservableResult
    | SpectralResult of ProjectorSpectral.TransferMatrix
    | CheckpointResult of success: bool
    | Error of Exception

/// リクエストスケジューラー
type RequestScheduler(config: SystemConfiguration) =

    let requestQueue = PriorityRequestQueue<ComputationRequest>()
    let activeRequests = ConcurrentDictionary<Guid, ComputationRequest>()
    let completedRequests = ConcurrentDictionary<Guid, ComputationResult>()

    let mutable isRunning = false
    let mutable workerTask: Task option = None
    let cancellationSource = new CancellationTokenSource()

    // 統計情報
    let mutable totalProcessed = 0L
    let mutable totalErrors = 0L
    let mutable totalTimeMs = 0.0

    /// リクエストを送信
    member _.SubmitRequest(requestType: RequestType, ?priority: int, ?deadline: DateTime) =
        let priority = defaultArg priority 5
        let request = {
            Id = Guid.NewGuid()
            Type = requestType
            Priority = priority
            SubmittedAt = DateTime.UtcNow
            Deadline = deadline
            Callback = fun _ -> ()  // デフォルトコールバック
        }

        activeRequests.TryAdd(request.Id, request) |> ignore
        requestQueue.Enqueue(priority, request)
        request.Id

    /// リクエストを送信（コールバック付き）
    member _.SubmitRequestWithCallback(requestType: RequestType, callback: ComputationResult -> unit,
                                      ?priority: int, ?deadline: DateTime) =
        let priority = defaultArg priority 5
        let request = {
            Id = Guid.NewGuid()
            Type = requestType
            Priority = priority
            SubmittedAt = DateTime.UtcNow
            Deadline = deadline
            Callback = callback
        }

        activeRequests.TryAdd(request.Id, request) |> ignore
        requestQueue.Enqueue(priority, request)
        request.Id

    /// 結果を取得
    member _.TryGetResult(requestId: Guid) =
        match completedRequests.TryRemove(requestId) with
        | true, result -> Some result
        | false, _ -> None

    /// 結果を待機
    member _.WaitForResult(requestId: Guid, ?timeout: TimeSpan) =
        let timeout = defaultArg timeout (TimeSpan.FromSeconds(30.0))
        let endTime = DateTime.UtcNow.Add(timeout)

        let rec wait() =
            if DateTime.UtcNow > endTime then
                None
            else
                match completedRequests.TryRemove(requestId) with
                | true, result -> Some result
                | false, _ ->
                    Thread.Sleep(10)
                    wait()

        wait()

    /// スケジューラーを開始
    member this.Start() =
        if not isRunning then
            isRunning <- true
            workerTask <- Some(Task.Run(fun () ->
                this.ProcessingLoop(cancellationSource.Token)
            ))

    /// スケジューラーを停止
    member _.Stop() =
        if isRunning then
            isRunning <- false
            cancellationSource.Cancel()

            match workerTask with
            | Some task ->
                task.Wait(5000) |> ignore
            | None -> ()

    /// 処理ループ
    member private this.ProcessingLoop(cancellationToken: CancellationToken) =
        while not cancellationToken.IsCancellationRequested && isRunning do
            try
                // タイムアウト付きでリクエスト取得
                let requestOpt =
                    requestQueue.DequeueAsync(TimeSpan.FromMilliseconds(100.0))
                    |> Async.RunSynchronously

                match requestOpt with
                | Some request ->
                    // デッドラインチェック
                    let shouldProcess =
                        match request.Deadline with
                        | Some deadline -> DateTime.UtcNow < deadline
                        | None -> true

                    if shouldProcess then
                        this.ProcessRequest(request)
                    else
                        // デッドライン超過
                        let result = Error(TimeoutException("Request deadline exceeded"))
                        this.CompleteRequest(request, result)

                | None -> ()

            with ex ->
                printfn "Error in processing loop: %s" ex.Message
                Interlocked.Increment(&totalErrors) |> ignore

    /// リクエストを処理
    member private this.ProcessRequest(request: ComputationRequest) =
        let timer = System.Diagnostics.Stopwatch.StartNew()

        try
            let result =
                match request.Type with
                | EnvironmentUpdate location ->
                    this.ProcessEnvironmentUpdate(location)

                | ObservableMeasurement(observable, location) ->
                    this.ProcessObservableMeasurement(observable, location)

                | SpectralAnalysis construction ->
                    this.ProcessSpectralAnalysis(construction)

                | Checkpoint ->
                    this.ProcessCheckpoint()

                | Shutdown ->
                    isRunning <- false
                    CheckpointResult true

            timer.Stop()
            totalTimeMs <- totalTimeMs + timer.Elapsed.TotalMilliseconds
            Interlocked.Increment(&totalProcessed) |> ignore

            this.CompleteRequest(request, result)

        with ex ->
            Interlocked.Increment(&totalErrors) |> ignore
            this.CompleteRequest(request, Error ex)

    /// 環境更新の処理
    member private _.ProcessEnvironmentUpdate(location: (int * int)) =
        // 実際の環境更新処理（簡略化）
        let env = {
            ACE_CTMRG.UniqueCorner = [| { Bits = 1UL } |]
            ACE_CTMRG.UniqueEdge = [| { Bits = 0UL } |]
            ACE_CTMRG.Chi = 16
            ACE_CTMRG.Iteration = 1
            ACE_CTMRG.History = {
                StateHashes = [||]
                CompressionRatios = [||]
                EffectiveRanks = [||]
                SpectralGaps = [||]
                Norms = [||]
            }
            ACE_CTMRG.LastUpdated = DateTime.UtcNow
        }
        EnvironmentResult env

    /// 観測量測定の処理
    member private _.ProcessObservableMeasurement(observable: ObservableKind, location: MeasurementLocation) =
        let result = {
            ObservableResult.Kind = observable
            ObservableResult.Value = {
                Parity = F2.Zero
                ContributionCount = 0
                Confidence = 1.0
            }
            ObservableResult.Location = location
            ObservableResult.ComputationTimeMs = 0.0
            ObservableResult.DeviceUsed = "CPU"
            ObservableResult.Timestamp = DateTime.UtcNow
        }
        ObservableResult result

    /// スペクトル解析の処理
    member private _.ProcessSpectralAnalysis(construction) =
        let transfer = {
            ProjectorSpectral.Data = [| { Bits = 0UL } |]
            ProjectorSpectral.Dimension = 1
            ProjectorSpectral.ProjectorStatus = None
            ProjectorSpectral.SpectralInfo = None
            ProjectorSpectral.EnvironmentChi = 16
            ProjectorSpectral.ConstructedAt = DateTime.UtcNow
        }
        SpectralResult transfer

    /// チェックポイントの処理
    member private _.ProcessCheckpoint() =
        // チェックポイント処理の実装
        CheckpointResult true

    /// リクエスト完了処理
    member private _.CompleteRequest(request: ComputationRequest, result: ComputationResult) =
        activeRequests.TryRemove(request.Id) |> ignore
        completedRequests.TryAdd(request.Id, result) |> ignore

        // コールバック実行
        try
            request.Callback(result)
        with ex ->
            printfn "Callback error: %s" ex.Message

    /// 統計情報取得
    member _.GetStatistics() =
        {|
            QueueLength = requestQueue.Count
            ActiveRequests = activeRequests.Count
            CompletedRequests = completedRequests.Count
            TotalProcessed = totalProcessed
            TotalErrors = totalErrors
            AverageTimeMs = if totalProcessed > 0L then totalTimeMs / float totalProcessed else 0.0
            ErrorRate = if totalProcessed > 0L then float totalErrors / float totalProcessed else 0.0
            PriorityCounts = requestQueue.GetPriorityCounts()
        |}
