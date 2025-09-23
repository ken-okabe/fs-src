namespace E8.Tests.Benchmarks

open System
open System.Diagnostics
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open E8.Algebra
open E8.Hardware
open E8.Integration

/// 性能ベンチマーク
[<MemoryDiagnoser>]
[<SimpleJob>]
type PerformanceBenchmark() =

    let mutable cpuOps: IF2Operations = null
    let mutable gpuOps: IF2Operations option = None
    let mutable testMatrix = [||]
    let matrixDim = 256

    [<GlobalSetup>]
    member _.Setup() =
        cpuOps <- new OptimizedCPUOperations() :> IF2Operations

        gpuOps <-
            try
                Some(new ROCmOperations(0) :> IF2Operations)
            with _ ->
                None

        // テスト行列の生成
        let rng = Random(42)
        testMatrix <- Array.init (matrixDim * matrixDim) (fun _ ->
            { Bits = uint64(rng.Next()) })

    [<Benchmark(Baseline = true)>]
    member _.MatrixMultiplyCPU() =
        cpuOps.MatrixMultiply testMatrix testMatrix matrixDim matrixDim matrixDim

    [<Benchmark>]
    member _.MatrixMultiplyGPU() =
        match gpuOps with
        | Some ops ->
            ops.MatrixMultiply testMatrix testMatrix matrixDim matrixDim matrixDim
        | None ->
            [||]

    [<Benchmark>]
    member _.TopologicalRankCPU() =
        cpuOps.ComputeTopologicalRank testMatrix matrixDim matrixDim

    [<Benchmark>]
    member _.ParallelXorCPU() =
        let vectors = Array.init 10 (fun _ -> testMatrix)
        cpuOps.ParallelXor vectors

    [<GlobalCleanup>]
    member _.Cleanup() =
        (cpuOps :> IDisposable).Dispose()
        gpuOps |> Option.iter (fun ops -> (ops :> IDisposable).Dispose())

/// ベンチマーク実行
module PerformanceBenchmarkRunner =

    [<EntryPoint>]
    let main argv =
        let summary = BenchmarkRunner.Run<PerformanceBenchmark>()
        0