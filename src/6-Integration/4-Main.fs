// 4-Main.fs
// This is the main entry point for the E8-PEPS simulation application.
// It orchestrates all underlying layers to execute a computation based on user configuration.
namespace E8.Integration

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open E8.Algebra
open E8.Hardware
open E8.Ace
open E8.Observable

/// The main application module.
module Main =

    /// Defines the execution modes for the simulation.
    type ExecutionMode =
        | SinglePoint of location: (int * int)
        | FullLattice
        | CustomLocations of locations: (int * int) list
        | Benchmark

    /// Encapsulates the full context required for a simulation run.
    type ExecutionContext = {
        Config: SystemConfiguration
        Engine: LocalComputationEngine
        OutputWriter: OutputWriter
        StartTime: DateTime
        CancellationToken: CancellationTokenSource
    }

    /// Handles the writing of simulation results to various output formats.
    and OutputWriter(config: OutputConfiguration) =

        let outputDir = config.OutputDirectory
        let mutable fileHandles = Map.empty<string, StreamWriter>

        do
            if not (Directory.Exists(outputDir)) then
                Directory.CreateDirectory(outputDir) |> ignore

        /// Gets or creates a file handle for a given output name.
        member private this.GetFileHandle(name: string) =
            match Map.tryFind name fileHandles with
            | Some handle -> handle
            | None ->
                let fileName =
                    match config.FileFormat with
                    | JSON -> sprintf "%s.json" name
                    | CSV -> sprintf "%s.csv" name
                    | _ -> sprintf "%s.txt" name // Fallback

                let path = Path.Combine(outputDir, fileName)
                let writer = new StreamWriter(path, false)
                fileHandles <- Map.add name writer fileHandles
                writer

        /// Writes an observable result to the specified output file.
        member this.WriteObservable(result: ObservableResult, name: string) =
            let writer = this.GetFileHandle(name)

            // Convert the algebraic value to a string representation
            let valueStr, gsd_or_confidence =
                match result.Value.Value with
                | Parity p -> (if p = F2.One then "1" else "0"), result.Value.AlgebraicConfidence
                | GroundStateDegeneracy gsd -> gsd.ToString(), result.Value.AlgebraicConfidence
                | WilsonLoopPhase p -> (if p = F2.One then "1" else "0"), result.Value.AlgebraicConfidence
                | CorrelationParity p -> (if p = F2.One then "1" else "0"), result.Value.AlgebraicConfidence

            match config.FileFormat with
            | JSON ->
                let json = sprintf """{"timestamp":"%O","kind":"%A","value":%s,"location":[%d,%d],"algebraic_confidence":%d}"""
                                  result.Timestamp
                                  result.Kind
                                  valueStr
                                  (fst result.Location.PrimaryPosition)
                                  (snd result.Location.PrimaryPosition)
                                  gsd_or_confidence
                writer.WriteLine(json)
            | CSV ->
                if writer.BaseStream.Position = 0L then
                    writer.WriteLine("timestamp,kind,value,x,y,algebraic_confidence")

                writer.WriteLine(sprintf "%O,%A,%s,%d,%d,%d"
                                 result.Timestamp
                                 result.Kind
                                 valueStr
                                 (fst result.Location.PrimaryPosition)
                                 (snd result.Location.PrimaryPosition)
                                 gsd_or_confidence)
            | _ -> writer.WriteLine(result.ToString())

            if config.RealTimeOutput then
                writer.Flush()

        /// Writes a log message.
        member this.WriteLog(message: string, level: VerbosityLevel) =
            if level <= config.Verbosity then
                let logWriter = this.GetFileHandle("simulation_log")
                let timestamp = DateTime.UtcNow.ToString("yyyy-MM-dd HH:mm:ss.fff")
                logWriter.WriteLine(sprintf "[%s] [%A] %s" timestamp level message)

                if config.RealTimeOutput then
                    logWriter.Flush()

                if config.Verbosity >= Normal then
                    printfn "[%s] %s" (level.ToString().ToUpper()) message

        interface IDisposable with
            member _.Dispose() =
                for KeyValue(_, writer) in fileHandles do
                    writer.Flush()
                    writer.Dispose()

    /// The main execution function that orchestrates the simulation.
    let execute (mode: ExecutionMode) (config: SystemConfiguration) =
        use cts = new CancellationTokenSource()
        use outputWriter = new OutputWriter(config.Output)
        use engine = new LocalComputationEngine(config)

        let context = {
            Config = config
            Engine = engine
            OutputWriter = outputWriter
            StartTime = DateTime.UtcNow
            CancellationToken = cts
        }

        outputWriter.WriteLog("=== E8-PEPS Hexagonal Lattice Simulation Started ===", Minimal)
        outputWriter.WriteLog(sprintf "Configuration loaded. Mode: %A" mode, Verbose)

        Console.CancelKeyPress.Add(fun args ->
            outputWriter.WriteLog("Interruption requested by user.", Minimal)
            cts.Cancel()
            args.Cancel <- true
        )

        try
            match mode with
            | SinglePoint location ->
                let loc = { PrimaryPosition = location; SecondaryPositions = []; Radius = 1 }
                for measurement in config.Observable.Measurements do
                    outputWriter.WriteLog(sprintf "Computing single observable '%s' at %A" measurement.OutputName location, Normal)
                    let result = engine.ComputeObservable(measurement.Type, loc)
                    outputWriter.WriteObservable(result, measurement.OutputName)

            | FullLattice ->
                let (width, height) = config.Lattice.Size
                outputWriter.WriteLog(sprintf "Computing full %dx%d lattice..." width height, Normal)
                let allLocations =
                    [| for x in 0..width-1 do
                           for y in 0..height-1 -> { PrimaryPosition = (x,y); SecondaryPositions=[]; Radius=1 } |]

                for measurement in config.Observable.Measurements do
                    outputWriter.WriteLog(sprintf "Computing observable '%s' for all sites." measurement.OutputName, Normal)
                    let results = engine.ComputeObservableBatch(measurement.Type, allLocations)
                    for result in results do
                        outputWriter.WriteObservable(result, measurement.OutputName)
            | _ -> failwith "Other execution modes are not fully implemented in this version."

            outputWriter.WriteLog("=== Computation Completed Successfully ===", Minimal)
            0 // Success code
        with
        | :? OperationCanceledException ->
            outputWriter.WriteLog("=== Computation Cancelled ===", Minimal)
            1 // Cancellation code
        | ex ->
            outputWriter.WriteLog(sprintf "=== FATAL ERROR: %s ===" ex.Message, Minimal)
            outputWriter.WriteLog(sprintf "StackTrace: %s" ex.StackTrace, Debug)
            -1 // Error code
        finally
            let stats = engine.GetStatistics()
            outputWriter.WriteLog(sprintf "Engine Statistics: Cache Hit Rate: %.2f%%, Total Computations: %d" stats.CacheHitRate stats.TotalComputations, Minimal)
            let elapsed = DateTime.UtcNow - context.StartTime
            outputWriter.WriteLog(sprintf "Total execution time: %O" elapsed, Minimal)

    /// Parses command-line arguments to determine the execution mode and configuration.
    let parseArgs (args: string[]) =
        // This is a simplified parser. A production version would use a robust library.
        let mutable mode = FullLattice
        let config =
            if Array.tryFind (fun arg -> arg = "--single") args |> Option.isSome then
                mode <- SinglePoint(0,0) // Default location
            ConfigurationLoader.fromCommandLine args

        (mode, config)

    /// The main entry point of the application.
    [<EntryPoint>]
    let main args =
        printfn "╔══════════════════════════════════════════╗"
        printfn "║     E8-PEPS MPO-iPEPS Local Compute      ║"
        printfn "║     (Hexagonal Lattice Final Version)    ║"
        printfn "╚══════════════════════════════════════════╝"

        try
            let (mode, config) = parseArgs args
            match ConfigurationLoader.validate config with
            | Ok validConfig -> execute mode validConfig
            | Error errors ->
                printfn "ERROR: Invalid configuration."
                for error in errors do printfn " - %s" error
                -1
        with ex ->
            printfn "An unexpected error occurred: %s" ex.Message
            -1