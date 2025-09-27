// 1-Configuration.fs
// Defines the global system configuration, simplified and aligned with the
// rigorous hexagonal lattice theory.
namespace E8.Integration

open System
open System.IO
open E8.Algebra
open E8.Hardware
open E8.Ace
open E8.Observable

/// Represents the complete configuration for the E8-PEPS simulation.
type SystemConfiguration = {
    /// Lattice settings, now specific to the hexagonal lattice.
    Lattice: LatticeConfiguration
    /// PEPS tensor settings.
    PEPS: PEPSConfiguration
    /// ACE (Algebraic Computation Engine) settings.
    ACE: ACEConfiguration
    /// Hardware backend settings.
    Hardware: HardwareConfiguration
    /// Observable measurement settings.
    Observable: ObservableConfiguration
    /// Output and logging settings.
    Output: OutputConfiguration
    /// Global runtime settings.
    Runtime: RuntimeConfiguration
}

and LatticeConfiguration = {
    /// The size of the lattice grid (width, height).
    Size: int * int
    /// The boundary conditions of the lattice.
    BoundaryCondition: BoundaryCondition
}

and BoundaryCondition =
    | Periodic // Torus
    | Open     // Open boundaries

and PEPSConfiguration = {
    /// Physical dimension (fixed at 2 for Fibonacci anyons).
    PhysicalDimension: int
    /// Virtual bond dimension.
    BondDimension: int
    /// The initialization method, now restricted to physically meaningful ones.
    Initialization: InitializationMethod
}

and InitializationMethod =
    | FromFile of path: string
    | FibonacciBasis
    | GoldenChain

and ACEConfiguration = {
    /// The initial bond dimension for the environment.
    InitialChi: int
    /// The maximum bond dimension for the environment.
    MaxChi: int
    /// The number of identical state hashes required for strict convergence.
    ConvergenceThreshold: int
    /// The maximum number of iterations before stopping.
    MaxIterations: int
    /// The interval (in iterations) for saving checkpoints.
    CheckpointInterval: int option
    /// The method for determining convergence.
    ConvergenceMethod: ConvergenceMethod
}

and ConvergenceMethod =
    | StateHash         // Based on exact, bit-level matching of environment tensors.
    | NormDifference    // Legacy, not used in F2.
    | SpectralGap       // Based on the spectral gap reaching its maximum value (1).
    | Combined          // A combination of methods.

and HardwareConfiguration = {
    /// The strategy for selecting a computation device.
    DeviceSelection: DeviceStrategy
    /// The batch size for parallel computations.
    BatchSize: int
    /// The size of the memory pool in megabytes.
    MemoryPoolSizeMB: int
    /// Parallelism settings for multi-threading and GPU streams.
    Parallelism: ParallelismLevel
    /// Enables hardware performance profiling.
    EnableProfiling: bool
}

and DeviceStrategy =
    | CPU
    | GPU of deviceId: int
    | Auto
    | Hybrid of gpuRatio: float

and ParallelismLevel = {
    /// The number of CPU threads to use.
    ThreadCount: int option
    /// The maximum degree of parallelism for tasks.
    MaxDegreeOfParallelism: int option
    /// The number of parallel streams for GPU computation.
    GPUStreams: int option
}

and ObservableConfiguration = {
    /// A list of observables to be measured.
    Measurements: MeasurementSpec list
    /// A specification for the locations to perform measurements.
    MeasurementLocations: LocationSpec
    /// The frequency (in iterations) of measurements.
    MeasurementInterval: int
}

and MeasurementSpec = {
    /// The kind of observable to measure.
    Type: ObservableType
    /// Parameters for the observable (e.g., the contour for a Wilson loop).
    Parameters: Map<string, obj>
    /// The name for the output file.
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
    /// The directory for output files.
    OutputDirectory: string
    /// The format for output files.
    FileFormat: OutputFormat
    /// The level of detail for logging.
    Verbosity: VerbosityLevel
    /// Enables real-time flushing of output files.
    RealTimeOutput: bool
    /// Enables compression for output files.
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
    /// A timeout for the entire simulation in seconds.
    TimeoutSeconds: int option
    /// The maximum memory usage in gigabytes.
    MaxMemoryGB: int option
    /// Enables checkpointing to resume simulations.
    EnableCheckpointing: bool
    /// The strategy for handling errors.
    ErrorHandling: ErrorStrategy
    /// A global random seed for any stochastic processes.
    RandomSeed: int option
}

and ErrorStrategy =
    | StopOnError
    | ContinueWithDefaults
    | Retry of maxAttempts: int
    | Fallback of strategy: DeviceStrategy

/// Provides default configurations and loading mechanisms.
module ConfigurationLoader =

    /// The default configuration, reflecting the hexagonal lattice theory.
    let defaultConfig = {
        Lattice = {
            Size = (32, 32)
            BoundaryCondition = Periodic
        }
        PEPS = {
            PhysicalDimension = 2
            BondDimension = 3 // D must be >= 3 for non-trivial physics in some models
            Initialization = FibonacciBasis
        }
        ACE = {
            InitialChi = 16
            MaxChi = 64
            ConvergenceThreshold = 3 // Strict convergence needs fewer steps
            MaxIterations = 1000
            CheckpointInterval = Some 100
            ConvergenceMethod = StateHash
        }
        Hardware = {
            DeviceSelection = Auto
            BatchSize = 1024
            MemoryPoolSizeMB = 1024
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
                    Type = TopologicalEntropy
                    Parameters = Map.empty
                    OutputName = "topological_entropy"
                }
                {
                    Type = CorrelationLength
                    Parameters = Map.empty
                    OutputName = "correlation_length"
                }
            ]
            MeasurementLocations = Center 5
            MeasurementInterval = 10
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
            ErrorHandling = StopOnError
            RandomSeed = Some 42
        }
    }

    /// Loads a configuration from a JSON file (placeholder implementation).
    let loadFromJson (jsonPath: string) =
        if File.Exists(jsonPath) then
            // A full implementation would parse the JSON and override defaults.
            let json = File.ReadAllText(jsonPath)
            // For now, we return the default config.
            defaultConfig
        else
            defaultConfig

    /// Overrides the default configuration with command-line arguments.
    let fromCommandLine (args: string[]) =
        let mutable config = defaultConfig
        // A full implementation would parse arguments like "--chi 32" etc.
        config

    /// Validates a configuration to ensure logical consistency.
    let validate (config: SystemConfiguration) =
        let errors = ResizeArray<string>()

        let (width, height) = config.Lattice.Size
        if width <= 0 || height <= 0 then
            errors.Add("Lattice size must be positive")

        if config.PEPS.BondDimension <= 0 then
            errors.Add("Bond dimension must be positive")

        if config.ACE.InitialChi <= 0 then
            errors.Add("Initial chi must be positive")
        if config.ACE.MaxChi < config.ACE.InitialChi then
            errors.Add("Max chi must be >= initial chi")

        if config.Hardware.BatchSize <= 0 then
            errors.Add("Batch size must be positive")

        if errors.Count > 0 then
            Error(errors.ToArray())
        else
            Ok(config)