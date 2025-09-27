// 2-RequestQueue.fs
// Defines the asynchronous request scheduling system for dispatching and managing
// computation tasks within the E8-PEPS project.
namespace E8.Integration

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks
open E8.Ace
open E8.Observable

/// A thread-safe, priority-based queue for computation requests.
type PriorityRequestQueue<'T>() =

    // An array of concurrent queues, indexed by priority (9 = highest).
    let queues =
        [|
            for _ in 0 .. 9 -> ConcurrentQueue<'T>()
        |]

    let mutable totalCount = 0
    let semaphore = new SemaphoreSlim(0)

    /// Enqueues an item with a specified priority.
    member _.Enqueue(priority: int, item: 'T) =
        let priority = max 0 (min 9 priority)
        queues.[9 - priority].Enqueue(item)
        Interlocked.Increment(&totalCount) |> ignore
        semaphore.Release() |> ignore

    /// Asynchronously dequeues an item, waiting if the queue is empty.
    member _.DequeueAsync(?timeout: TimeSpan) =
        async {
            let timeout = defaultArg timeout TimeSpan.MaxValue
            let! success = semaphore.WaitAsync(timeout) |> Async.AwaitTask

            if success then
                // Scan from highest to lowest priority.
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

    /// The total number of items in the queue.
    member _.Count = totalCount

/// Represents a computation request sent to the scheduler.
/// This type is now strictly aligned with the algebraic types from Layer 5.
type ComputationRequest = {
    /// A unique identifier for the request.
    Id: Guid
    /// The specific type of computation requested.
    Type: RequestType
    /// The priority of the request (0-9, 9 is highest).
    Priority: int
    /// The time the request was submitted.
    SubmittedAt: DateTime
    /// An optional deadline for the request.
    Deadline: DateTime option
    /// A callback function to be invoked with the result.
    Callback: ComputationResult -> unit
}

and RequestType =
    | EnvironmentConvergence of location: (int * int)
    | ObservableMeasurement of kind: ObservableKind * location: MeasurementLocation
    | SpectralAnalysisRequest of location: (int*int)
    | Checkpoint
    | Shutdown

and ComputationResult =
    | EnvironmentResult of ACE_CTMRG.SymmetricEnvironment
    | ObservableResult of ObservableResult
    | SpectralResult of ProjectorSpectral.SpectralInfo
    | CheckpointResult of success: bool
    | Error of Exception

/// The main request scheduler for managing and executing computation tasks.
type RequestScheduler(config: SystemConfiguration) =

    let requestQueue = PriorityRequestQueue<ComputationRequest>()
    let activeRequests = ConcurrentDictionary<Guid, ComputationRequest>()
    let completedRequests = ConcurrentDictionary<Guid, ComputationResult>()

    let mutable isRunning = false
    let mutable workerTask: Task option = None
    let cancellationSource = new CancellationTokenSource()

    // A placeholder for the actual computation engine, which will be passed during execution.
    let mutable engine: LocalComputationEngine = null

    /// Submits a request to the queue and returns its unique ID.
    member _.SubmitRequest(requestType: RequestType, ?priority: int, ?deadline: DateTime) =
        let priority = defaultArg priority 5
        let request = {
            Id = Guid.NewGuid()
            Type = requestType
            Priority = priority
            SubmittedAt = DateTime.UtcNow
            Deadline = deadline
            Callback = fun _ -> () // Default no-op callback
        }

        activeRequests.TryAdd(request.Id, request) |> ignore
        requestQueue.Enqueue(priority, request)
        request.Id

    /// Starts the scheduler's processing loop in a background task.
    member this.Start(computationEngine: LocalComputationEngine) =
        engine <- computationEngine
        if not isRunning then
            isRunning <- true
            workerTask <- Some(Task.Run(fun () ->
                this.ProcessingLoop(cancellationSource.Token)
            ))

    /// Stops the scheduler's processing loop.
    member _.Stop() =
        if isRunning then
            isRunning <- false
            cancellationSource.Cancel()
            match workerTask with
            | Some task -> task.Wait(5000) |> ignore
            | None -> ()

    /// The main processing loop that dequeues and processes requests.
    member private this.ProcessingLoop(cancellationToken: CancellationToken) =
        while not cancellationToken.IsCancellationRequested && isRunning do
            try
                let requestOpt =
                    requestQueue.DequeueAsync(TimeSpan.FromMilliseconds(100.0))
                    |> Async.RunSynchronously

                match requestOpt with
                | Some request ->
                    if request.Deadline.IsNone || DateTime.UtcNow < request.Deadline.Value then
                        this.ProcessRequest(request)
                    else
                        let result = Error(TimeoutException("Request deadline exceeded"))
                        this.CompleteRequest(request, result)
                | None -> ()
            with ex ->
                // Log the exception in a real implementation
                ()

    /// Processes a single request by calling the appropriate computation engine function.
    member private this.ProcessRequest(request: ComputationRequest) =
        try
            let result =
                match request.Type with
                | EnvironmentConvergence location ->
                    let (env, _, _) = engine.ConvergeEnvironment(location)
                    EnvironmentResult env
                | ObservableMeasurement (kind, location) ->
                    let obsResult = engine.ComputeObservable(kind, location)
                    ObservableResult obsResult
                | SpectralAnalysisRequest location ->
                    // This is a simplified path; a full implementation might have its own function.
                    let obsResult = engine.ComputeObservable(SpectralGap, location)
                    match obsResult.Value.Value with
                    | Parity p -> SpectralResult { LeadingEigenvalue=p; SpectralGap=p; Degeneracy=obsResult.Value.AlgebraicConfidence; MinimalPolynomialDegree=obsResult.Value.AlgebraicConfidence }
                    | _ -> Error(InvalidCastException("Could not cast to spectral info."))
                | Checkpoint ->
                    CheckpointResult true // Placeholder
                | Shutdown ->
                    isRunning <- false
                    CheckpointResult true

            this.CompleteRequest(request, result)
        with ex ->
            this.CompleteRequest(request, Error ex)

    /// Completes a request by storing the result and invoking the callback.
    member private _.CompleteRequest(request: ComputationRequest, result: ComputationResult) =
        activeRequests.TryRemove(request.Id) |> ignore
        completedRequests.TryAdd(request.Id, result) |> ignore
        try
            request.Callback(result)
        with ex ->
            // Log callback exception
            ()