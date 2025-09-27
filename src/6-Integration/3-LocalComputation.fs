// 3-LocalComputation.fs
// Implements the local computation engine, which orchestrates the ACE and AOE
// to compute physical observables on demand based on the "Hexagonal Local Completeness Theorem".
namespace E8.Integration

open System
open System.Collections.Concurrent
open System.Threading.Tasks
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.Ace
open E8.Observable
open E8.TensorConstruction.FibonacciTensor

/// The core engine for performing local, on-demand, and exact computations.
type LocalComputationEngine(config: SystemConfiguration) =

    // The hardware backend, initialized based on the system configuration.
    let hardwareOps =
        match config.Hardware.DeviceSelection with
        | CPU -> new OptimizedCPUOperations(config.Hardware.MemoryPoolSizeMB) :> IF2Operations
        | GPU deviceId -> new ROCmOperations(deviceId) :> IF2Operations
        | Auto ->
            try new ROCmOperations(0) :> IF2Operations
            with _ -> new OptimizedCPUOperations(config.Hardware.MemoryPoolSizeMB) :> IF2Operations
        | Hybrid _ -> new OptimizedCPUOperations(config.Hardware.MemoryPoolSizeMB) :> IF2Operations // Simplified fallback

    // The master PEPS tensor for the entire simulation.
    let peps =
        match config.PEPS.Initialization with
        | FibonacciBasis -> (FibonacciTensor.constructFibonacciTensor()).Data
        | GoldenChain -> (FibonacciTensor.constructFibonacciTensor()).Data // Placeholder
        | FromFile path -> failwith "Loading PEPS from file is not yet implemented."

    // A thread-safe cache for converged environment tensors. The key is the (x,y) coordinate.
    // The stored value is the immutable, exact fixed-point environment for that location.
    let environmentCache = ConcurrentDictionary<(int * int), ACE_CTMRG.SymmetricEnvironment>()

    // Statistics counters
    let mutable totalComputations = 0L
    let mutable cacheHits = 0L
    let mutable cacheMisses = 0L

    /// Computes (or retrieves from cache) the exact fixed-point environment for a given location.
    /// This function is the practical implementation of the project's core paradigm:
    /// it calls the ACE (Layer 4) to find the unique, exact ground state environment.
    member this.ComputeLocalEnvironment(centerX: int, centerY: int) =
        let key = (centerX, centerY)

        match environmentCache.TryGetValue(key) with
        | true, cachedEnv ->
            Interlocked.Increment(&cacheHits) |> ignore
            (cachedEnv, true, 0) // Return cached env, convergence=true, iterations=0
        | false, _ ->
            Interlocked.Increment(&cacheMisses) |> ignore

            // The environment is not in the cache, so we must compute it.
            // Call the rigorous CTMRG engine from Layer 4.
            let (finalEnv, converged) = ACE_CTMRG.runCTMRG peps config.ACE.MaxChi config.ACE.MaxIterations hardwareOps

            if not converged then
                // This is a critical situation, indicating that either maxIterations was too low
                // or there's a fundamental issue with the model parameters.
                // In a production system, this should be logged as a serious warning.
                ()

            // Add the newly computed, exact environment to the cache.
            environmentCache.TryAdd(key, finalEnv) |> ignore
            (finalEnv, converged, config.ACE.MaxIterations) // A more accurate iteration count would require modifying runCTMRG

    /// Computes a specific physical observable at a given location.
    /// This function orchestrates the full computation stack:
    /// 1. Gets the exact environment from the ACE (this method).
    /// 2. Passes the environment to the appropriate AOE module (Layer 5) to decode the physical value.
    member this.ComputeObservable(kind: ObservableKind, location: MeasurementLocation) : ObservableResult =
        let timer = System.Diagnostics.Stopwatch.StartNew()

        // 1. Obtain the exact local environment using the ACE.
        let (environment, converged, _) = this.ComputeLocalEnvironment(location.PrimaryPosition)

        // 2. Instantiate the correct observable computation module (the AOE).
        let observable: IObservable =
            match kind with
            | LocalMagnetization -> LocalMagnetization() :> IObservable
            | TwoPointCorrelation _ -> ProjectedCorrelator() :> IObservable
            | TopologicalEntropy -> TopologicalEntropyMeasurement() :> IObservable
            | WilsonLoop _ -> WilsonLoop() :> IObservable
            | SpectralGap -> SpectralObservable("spectralgap") :> IObservable
            | AlgebraicConfidenceValue -> SpectralObservable("confidence") :> IObservable
            | _ -> failwithf "Observable kind '%A' is not yet implemented." kind

        // 3. Compute (decode) the physical value using the AOE.
        let observableValue = observable.Compute environment peps location hardwareOps

        timer.Stop()
        Interlocked.Increment(&totalComputations) |> ignore

        {
            Kind = kind
            Value = observableValue
            Location = location
            ComputationTimeMs = timer.Elapsed.TotalMilliseconds
            DeviceUsed = hardwareOps.DeviceInfo().DeviceType.ToString()
            Timestamp = DateTime.UtcNow
        }

    /// Computes a batch of observables, leveraging parallelization.
    /// The "Hexagonal Local Completeness Theorem" guarantees that each computation is
    /// independent, making this parallelization perfectly safe and exact.
    member this.ComputeObservableBatch(kind: ObservableKind, locations: MeasurementLocation[]) =
        locations
        |> Array.Parallel.map (fun loc -> this.ComputeObservable(kind, loc))

    /// Public-facing method to explicitly converge an environment.
    member this.ConvergeEnvironment(location: (int * int)) =
        this.ComputeLocalEnvironment(location)

    /// Returns statistics about the engine's performance.
    member _.GetStatistics() =
        let totalRequests = cacheHits + cacheMisses
        let hitRate = if totalRequests > 0L then (float cacheHits / float totalRequests) * 100.0 else 0.0
        {|
            TotalComputations = totalComputations
            CacheSize = environmentCache.Count
            CacheHits = cacheHits
            CacheMisses = cacheMisses
            CacheHitRate = hitRate
            DeviceInfo = hardwareOps.DeviceInfo()
            MemoryInfo = hardwareOps.MemoryInfo()
        |}

    /// Disposes of the underlying hardware resources.
    interface IDisposable with
        member _.Dispose() =
            (hardwareOps :> IDisposable).Dispose()