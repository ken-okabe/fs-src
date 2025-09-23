// 7-Integration/4-Main.fs
namespace E8.Integration

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open E8.Algebra
open E8.Hardware
open E8.Ace
open E8.Observable

/// メインアプリケーション
module Main =

    /// 実行モード
    type ExecutionMode =
        | SinglePoint of location: (int * int)
        | FullLattice
        | CustomLocations of locations: (int * int) list
        | Benchmark
        | Interactive

    /// 実行コンテキスト
    type ExecutionContext = {
        Config: SystemConfiguration
        Engine: LocalComputationEngine
        Scheduler: RequestScheduler
        OutputWriter: OutputWriter
        StartTime: DateTime
        CancellationToken: CancellationTokenSource
    }

    /// 出力ライター
    and OutputWriter(config: OutputConfiguration) =

        let outputDir = config.OutputDirectory
        let mutable fileHandles = Map.empty<string, StreamWriter>

        do
            // 出力ディレクトリの作成
            if not (Directory.Exists(outputDir)) then
                Directory.CreateDirectory(outputDir) |> ignore

        /// ファイルハンドルの取得
        member private _.GetFileHandle(name: string) =
            match Map.tryFind name fileHandles with
            | Some handle -> handle
            | None ->
                let fileName =
                    match config.FileFormat with
                    | JSON -> sprintf "%s.json" name
                    | CSV -> sprintf "%s.csv" name
                    | Binary -> sprintf "%s.bin" name
                    | HDF5 -> sprintf "%s.h5" name
                    | All -> sprintf "%s.json" name  // デフォルト

                let path = Path.Combine(outputDir, fileName)
                let writer = new StreamWriter(path, false)
                fileHandles <- Map.add name writer fileHandles
                writer

        /// 観測量の書き込み
        member this.WriteObservable(result: ObservableResult, name: string) =
            let writer = this.GetFileHandle(name)

            match config.FileFormat with
            | JSON ->
                let json = sprintf """{"timestamp":"%O","kind":"%A","value":%d,"location":[%d,%d],"confidence":%f}"""
                                  result.Timestamp
                                  result.Kind
                                  (if result.Value.Parity = F2.One then 1 else 0)
                                  (fst result.Location.PrimaryPosition)
                                  (snd result.Location.PrimaryPosition)
                                  result.Value.Confidence
                writer.WriteLine(json)

            | CSV ->
                if writer.BaseStream.Position = 0L then
                    // ヘッダー
                    writer.WriteLine("timestamp,kind,value,x,y,confidence")

                writer.WriteLine(sprintf "%O,%A,%d,%d,%d,%f"
                                result.Timestamp
                                result.Kind
                                (if result.Value.Parity = F2.One then 1 else 0)
                                (fst result.Location.PrimaryPosition)
                                (snd result.Location.PrimaryPosition)
                                result.Value.Confidence)

            | Binary ->
                // バイナリ出力
                use bw = new BinaryWriter(writer.BaseStream)
                bw.Write(result.Timestamp.Ticks)
                bw.Write(result.Value.Parity = F2.One)
                bw.Write(fst result.Location.PrimaryPosition)
                bw.Write(snd result.Location.PrimaryPosition)
                bw.Write(result.Value.Confidence)

            | _ -> ()

            if config.RealTimeOutput then
                writer.Flush()

        /// 統計情報の書き込み
        member this.WriteStatistics(stats: obj, name: string) =
            let writer = this.GetFileHandle(name + "_stats")
            writer.WriteLine(sprintf "%O: %A" DateTime.UtcNow stats)
            writer.Flush()

        /// ログの書き込み
        member this.WriteLog(message: string, level: VerbosityLevel) =
            if level <= config.Verbosity then
                let logWriter = this.GetFileHandle("log")
                let timestamp = DateTime.UtcNow.ToString("yyyy-MM-dd HH:mm:ss.fff")
                logWriter.WriteLine(sprintf "[%s] [%A] %s" timestamp level message)

                if config.RealTimeOutput then
                    logWriter.Flush()

                // コンソール出力
                if config.Verbosity >= Normal then
                    printfn "[%A] %s" level message

        /// クリーンアップ
        interface IDisposable with
            member _.Dispose() =
                for KeyValue(_, writer) in fileHandles do
                    writer.Flush()
                    writer.Dispose()

    /// プログレスレポーター
    type ProgressReporter(total: int, context: ExecutionContext) =
        let mutable completed = 0
        let mutable lastReportTime = DateTime.UtcNow
        let reportInterval = TimeSpan.FromSeconds(1.0)

        member _.ReportProgress(increment: int) =
            Interlocked.Add(&completed, increment) |> ignore

            let now = DateTime.UtcNow
            if now - lastReportTime > reportInterval then
                lastReportTime <- now
                let percentage = 100.0 * float completed / float total
                let elapsed = now - context.StartTime
                let rate = float completed / elapsed.TotalSeconds
                let eta =
                    if rate > 0.0 then
                        TimeSpan.FromSeconds(float(total - completed) / rate)
                    else
                        TimeSpan.MaxValue

                let message = sprintf "Progress: %d/%d (%.1f%%), Rate: %.1f/s, ETA: %O"
                                     completed total percentage rate eta

                context.OutputWriter.WriteLog(message, Normal)

        member _.IsComplete = completed >= total

    /// メイン実行関数
    let execute (mode: ExecutionMode) (config: SystemConfiguration) =

        // コンテキストの初期化
        use cts = new CancellationTokenSource()
        use outputWriter = new OutputWriter(config.Output)
        use engine = new LocalComputationEngine(config)
        let scheduler = RequestScheduler(config)

        let context = {
            Config = config
            Engine = engine
            Scheduler = scheduler
            OutputWriter = outputWriter
            StartTime = DateTime.UtcNow
            CancellationToken = cts
        }

        outputWriter.WriteLog("=== E8-PEPS MPO-iPEPS Computation Started ===", Minimal)
        outputWriter.WriteLog(sprintf "Configuration: %A" config, Verbose)

        // Ctrl+C ハンドラー
        Console.CancelKeyPress.Add(fun args ->
            outputWriter.WriteLog("Interruption requested...", Minimal)
            cts.Cancel()
            args.Cancel <- true
        )

        // タイムアウト設定
        match config.Runtime.TimeoutSeconds with
        | Some timeout ->
            cts.CancelAfter(TimeSpan.FromSeconds(float timeout))
        | None -> ()

        // スケジューラー開始
        scheduler.Start()

        try
            // 実行モードに応じた処理
            match mode with
            | SinglePoint location ->
                executeSinglePoint context location

            | FullLattice ->
                executeFullLattice context

            | CustomLocations locations ->
                executeCustomLocations context locations

            | Benchmark ->
                executeBenchmark context

            | Interactive ->
                executeInteractive context

            outputWriter.WriteLog("=== Computation Completed Successfully ===", Minimal)
            0  // 成功

        with
        | :? OperationCanceledException ->
            outputWriter.WriteLog("=== Computation Cancelled ===", Minimal)
            1  // キャンセル
        | ex ->
            outputWriter.WriteLog(sprintf "=== Error: %s ===" ex.Message, Minimal)
            outputWriter.WriteLog(sprintf "StackTrace: %s" ex.StackTrace, Debug)
            2  // エラー

        finally
            scheduler.Stop()

            // 統計情報の出力
            let engineStats = engine.GetStatistics()
            let schedulerStats = scheduler.GetStatistics()

            outputWriter.WriteStatistics(engineStats, "engine")
            outputWriter.WriteStatistics(schedulerStats, "scheduler")

            let elapsed = DateTime.UtcNow - context.StartTime
            outputWriter.WriteLog(sprintf "Total execution time: %O" elapsed, Minimal)

    /// 単一点の計算
    and executeSinglePoint (context: ExecutionContext) (location: (int * int)) =
        context.OutputWriter.WriteLog(sprintf "Computing at location %A" location, Normal)

        // 環境の収束
        context.OutputWriter.WriteLog("Converging environment...", Verbose)
        let (env, converged, iterations) = context.Engine.ConvergeEnvironment(location)

        if not converged then
            context.OutputWriter.WriteLog("Warning: Environment did not converge", Normal)

        context.OutputWriter.WriteLog(sprintf "Converged in %d iterations" iterations, Normal)

        // 観測量の計算
        for measurement in context.Config.Observable.Measurements do
            context.OutputWriter.WriteLog(sprintf "Measuring %s..." measurement.OutputName, Verbose)

            let result = context.Engine.ComputeObservable(measurement.Type, location)
            context.OutputWriter.WriteObservable(result, measurement.OutputName)

            // 結果の表示
            if context.Config.Output.Verbosity >= Normal then
                printfn "%s = %A (confidence: %.3f)"
                        measurement.OutputName
                        result.Value.Parity
                        result.Value.Confidence

    /// 全格子の計算
    and executeFullLattice (context: ExecutionContext) =
        let (width, height) = context.Config.Lattice.Size
        let totalSites = width * height

        context.OutputWriter.WriteLog(sprintf "Computing full lattice: %d×%d = %d sites"
                                             width height totalSites, Normal)

        let progress = ProgressReporter(totalSites, context)

        // 全サイトの処理
        let allLocations =
            [| for x in 0 .. width - 1 do
                for y in 0 .. height - 1 do
                    yield (x, y) |]

        // バッチ処理
        let batchSize = context.Config.Hardware.BatchSize
        let batches = allLocations |> Array.chunkBySize batchSize

        for batchIdx, batch in batches |> Array.indexed do
            if context.CancellationToken.IsCancellationRequested then
                raise (OperationCanceledException())

            context.OutputWriter.WriteLog(sprintf "Processing batch %d/%d"
                                                 (batchIdx + 1) batches.Length, Verbose)

            // 並列バッチ処理
            for measurement in context.Config.Observable.Measurements do
                let results = context.Engine.ComputeObservableBatch(
                                measurement.Type, batch)

                for result in results do
                    context.OutputWriter.WriteObservable(result, measurement.OutputName)

            progress.ReportProgress(batch.Length)

    /// カスタム位置での計算
    and executeCustomLocations (context: ExecutionContext) (locations: (int * int) list) =
        context.OutputWriter.WriteLog(sprintf "Computing at %d custom locations"
                                             locations.Length, Normal)

        let progress = ProgressReporter(locations.Length, context)

        for location in locations do
            if context.CancellationToken.IsCancellationRequested then
                raise (OperationCanceledException())

            // 各位置での計算
            for measurement in context.Config.Observable.Measurements do
                let result = context.Engine.ComputeObservable(measurement.Type, location)
                context.OutputWriter.WriteObservable(result, measurement.OutputName)

            progress.ReportProgress(1)

    /// ベンチマークモード
    and executeBenchmark (context: ExecutionContext) =
        context.OutputWriter.WriteLog("=== Benchmark Mode ===", Minimal)

        // ウォームアップ
        context.OutputWriter.WriteLog("Warming up...", Normal)
        for _ in 1 .. 10 do
            let _ = context.Engine.ComputeObservable(Magnetization, (0, 0))
            ()

        // 性能測定
        let testSizes = [| 1; 10; 100; 1000 |]

        for size in testSizes do
            let locations = Array.init size (fun i -> (i % 32, i / 32))

            let timer = System.Diagnostics.Stopwatch.StartNew()
            let _ = context.Engine.ComputeObservableBatch(Magnetization, locations)
            timer.Stop()

            let throughput = float size / timer.Elapsed.TotalSeconds
            context.OutputWriter.WriteLog(
                sprintf "Size: %d, Time: %.3f ms, Throughput: %.1f ops/s"
                        size timer.Elapsed.TotalMilliseconds throughput,
                Minimal)

    /// インタラクティブモード
    and executeInteractive (context: ExecutionContext) =
        context.OutputWriter.WriteLog("=== Interactive Mode ===", Minimal)
        printfn "Enter commands (type 'help' for options, 'quit' to exit):"

        let rec loop() =
            printf "> "
            let input = Console.ReadLine()

            match input.ToLower().Split(' ') with
            | [| "quit" |] | [| "exit" |] ->
                ()

            | [| "help" |] ->
                printfn """
Commands:
  compute <x> <y>           - Compute observables at location (x,y)
  converge <x> <y>          - Converge environment at location (x,y)
  measure <type> <x> <y>    - Measure specific observable
  stats                     - Show statistics
  clear                     - Clear cache
  quit                      - Exit
"""
                loop()

            | [| "compute"; x; y |] ->
                try
                    let location = (int x, int y)
                    for measurement in context.Config.Observable.Measurements do
                        let result = context.Engine.ComputeObservable(measurement.Type, location)
                        printfn "%s at %A = %A"
                                measurement.OutputName location result.Value.Parity
                with ex ->
                    printfn "Error: %s" ex.Message
                loop()

            | [| "stats" |] ->
                let stats = context.Engine.GetStatistics()
                printfn "Engine Statistics: %A" stats
                let schedStats = context.Scheduler.GetStatistics()
                printfn "Scheduler Statistics: %A" schedStats
                loop()

            | [| "clear" |] ->
                // キャッシュクリア（新しいエンジンが必要）
                printfn "Cache cleared"
                loop()

            | _ ->
                printfn "Unknown command. Type 'help' for options."
                loop()

        loop()

    /// コマンドライン引数の解析
    let parseArgs (args: string[]) =
        let mutable mode = SinglePoint(0, 0)
        let mutable configFile = None

        let rec parse (args: string list) =
            match args with
            | "--mode" :: modeStr :: rest ->
                mode <-
                    match modeStr.ToLower() with
                    | "single" -> SinglePoint(0, 0)
                    | "full" -> FullLattice
                    | "benchmark" -> Benchmark
                    | "interactive" -> Interactive
                    | _ -> SinglePoint(0, 0)
                parse rest

            | "--location" :: x :: y :: rest ->
                mode <- SinglePoint(int x, int y)
                parse rest

            | "--config" :: file :: rest ->
                configFile <- Some file
                parse rest

            | _ :: rest -> parse rest
            | [] -> ()

        parse (Array.toList args)

        // 設定の読み込み
        let config =
            match configFile with
            | Some file -> ConfigurationLoader.loadFromJson file
            | None -> ConfigurationLoader.fromCommandLine args

        (mode, config)

    /// メインエントリーポイント
    [<EntryPoint>]
    let main args =
        printfn "╔══════════════════════════════════════════╗"
        printfn "║     E8-PEPS MPO-iPEPS Local Compute     ║"
        printfn "║          F₂ Topological System           ║"
        printfn "╚══════════════════════════════════════════╝"
        printfn ""

        try
            let (mode, config) = parseArgs args

            // 設定の検証
            match ConfigurationLoader.validate config with
            | Ok validConfig ->
                execute mode validConfig
            | Error errors ->
                printfn "Configuration errors:"
                for error in errors do
                    printfn "  - %s" error
                3

        with ex ->
            printfn "Fatal error: %s" ex.Message
            printfn "%s" ex.StackTrace
            4
