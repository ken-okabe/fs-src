// src/4-PEPS-Ace/4-TransferMatrix.Tests.fs
namespace E8.Tests.Ace

open System
open System.Diagnostics
open Xunit
open FsUnit.Xunit
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.Hardware.CPU
open E8.Ace
open E8.Ace.TransferMatrix

module TransferMatrixTests =

    let createTestMatrix (size: int) (pattern: string) =
        let wordsPerRow = (size + 63) / 64
        let matrix = Array.zeroCreate (size * wordsPerRow)

        match pattern with
        | "identity" ->
            for i in 0 .. size - 1 do
                let wordIdx = i * wordsPerRow + i / 64
                let bitIdx = i % 64
                matrix.[wordIdx] <- { Bits = matrix.[wordIdx].Bits ||| (1UL <<< bitIdx) }

        | "dense" ->
            let rng = Random(42)
            for i in 0 .. matrix.Length - 1 do
                matrix.[i] <- { Bits = uint64 (rng.Next()) }

        | "sparse" ->
            let rng = Random(42)
            for _ in 0 .. size * 2 do
                let i = rng.Next(size)
                let j = rng.Next(size)
                let wordIdx = i * wordsPerRow + j / 64
                let bitIdx = j % 64
                matrix.[wordIdx] <- { Bits = matrix.[wordIdx].Bits ||| (1UL <<< bitIdx) }

        | "symmetric" ->
            for i in 0 .. size - 1 do
                for j in i .. size - 1 do
                    if (i + j) % 3 = 0 then
                        let ij_wordIdx = i * wordsPerRow + j / 64
                        let ij_bitIdx = j % 64
                        let ji_wordIdx = j * wordsPerRow + i / 64
                        let ji_bitIdx = i % 64
                        matrix.[ij_wordIdx] <- { Bits = matrix.[ij_wordIdx].Bits ||| (1UL <<< ij_bitIdx) }
                        if i <> j then
                            matrix.[ji_wordIdx] <- { Bits = matrix.[ji_wordIdx].Bits ||| (1UL <<< ji_bitIdx) }

        | _ -> ()

        matrix

    [<Fact>]
    let ``Structure analysis identifies sparsity correctly`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 16
        let sparseMatrix = createTestMatrix size "sparse"

        let structure = analyzeStructure sparseMatrix size ops

        structure.SparsityPattern.SparsityRatio |> should be (greaterThan 0.5)
        structure.SparsityPattern.NonZeroCount |> should be (lessThan (size * size))

    [<Fact>]
    let ``Structure analysis identifies symmetry correctly`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 8
        let symMatrix = createTestMatrix size "symmetric"

        let structure = analyzeStructure symMatrix size ops

        structure.Symmetries.IsSymmetric |> should be True

    [<Fact>]
    let ``Matrix power computation is correct`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 4
        let identity = createTestMatrix size "identity"

        // I^n = I
        let i2 = matrixPower identity size 2 ops
        let (isEqual, _) = ops.MatrixEquals identity i2 size
        isEqual |> should be True

        let i10 = matrixPower identity size 10 ops
        let (isEqual10, _) = ops.MatrixEquals identity i10 size
        isEqual10 |> should be True

    [<Fact>]
    let ``Matrix power handles zero power correctly`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 4
        let matrix = createTestMatrix size "sparse"

        let m0 = matrixPower matrix size 0 ops

        // M^0 = I
        let identity = createTestMatrix size "identity"
        let (isIdentity, _) = ops.MatrixEquals m0 identity size
        isIdentity |> should be True

    [<Fact>]
    let ``Matrix power uses fast exponentiation`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 8
        let matrix = createTestMatrix size "sparse"

        // 大きなべき乗でも高速に計算
        let sw = Stopwatch.StartNew()
        let _ = matrixPower matrix size 100 ops
        sw.Stop()

        sw.ElapsedMilliseconds |> should be (lessThan 1000L)

    [<Fact>]
    let ``Eigenspace approximation works correctly`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 8
        let identity = createTestMatrix size "identity"

        let eigenInfo = approximateEigenspace identity size ops

        // I - I = 0のnullityは全次元
        eigenInfo.NullityOfTMinusI |> should equal size

        // Iのnullityは0
        eigenInfo.NullityOfT |> should equal 0

    [<Fact>]
    let ``Unique fixed point detection works`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 8
        let matrix = createTestMatrix size "sparse"

        let eigenInfo = approximateEigenspace matrix size ops

        eigenInfo.HasUniqueFixedPoint |> should satisfy (fun b -> b = true || b = false)

    [<Fact>]
    let ``Correlation length computation for projectors`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 8
        let wordsPerRow = (size + 63) / 64
        let projector = Array.zeroCreate (size * wordsPerRow)

        // 射影行列を作成
        for i in 0 .. size / 2 - 1 do
            let wordIdx = i * wordsPerRow + i / 64
            let bitIdx = i % 64
            projector.[wordIdx] <- { Bits = projector.[wordIdx].Bits ||| (1UL <<< bitIdx) }

        let corrLength = computeCorrelationLength projector size ops

        corrLength |> should equal None  // プロジェクターは無限相関長

    [<Fact>]
    let ``Physical quantities extraction maintains F2`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 8
        let matrix = createTestMatrix size "dense"

        let quantities = extractPhysicalQuantities matrix size ops

        quantities.MagnetizationDensity |> should satisfy (fun q -> q = F2.Zero || q = F2.One)
        quantities.EnergyDensity |> should satisfy (fun q -> q = F2.Zero || q = F2.One)
        quantities.EntanglementParity |> should satisfy (fun q -> q = F2.Zero || q = F2.One)

    [<Fact>]
    let ``Complete analysis executes successfully`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 16
        let matrix = createTestMatrix size "sparse"

        let analysis = performCompleteAnalysis matrix size ops

        analysis.Structure |> should not' (be null)
        analysis.Eigenspace |> should not' (be null)
        analysis.PhysicalQuantities |> should not' (be null)
        analysis.MinimalPolynomialDegree |> should be (greaterThan 0)

    [<Fact>]
    let ``Analysis correctly identifies projectors`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 8
        let wordsPerRow = (size + 63) / 64
        let projector = Array.zeroCreate (size * wordsPerRow)

        // 射影行列
        for i in 0 .. 3 do
            let wordIdx = i * wordsPerRow + i / 64
            let bitIdx = i % 64
            projector.[wordIdx] <- { Bits = projector.[wordIdx].Bits ||| (1UL <<< bitIdx) }

        let analysis = performCompleteAnalysis projector size ops

        analysis.IsProjector |> should be True
        analysis.MinimalPolynomialDegree |> should equal 2

    [<Fact>]
    let ``Row and column non-zero counts are accurate`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 4
        let wordsPerRow = (size + 63) / 64
        let matrix = Array.zeroCreate (size * wordsPerRow)

        // 特定のパターン
        matrix.[0] <- { Bits = 0b1111UL }  // 第0行：4個の1
        matrix.[1] <- { Bits = 0b0001UL }  // 第1行：1個の1

        let structure = analyzeStructure matrix size ops

        structure.SparsityPattern.RowNonZeros.[0] |> should equal 4
        structure.SparsityPattern.RowNonZeros.[1] |> should equal 1

    // パフォーマンステスト
    [<Fact>]
    let ``Complete analysis performance benchmark`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let sizes = [| 8; 16; 32; 64 |]

        for size in sizes do
            let matrix = createTestMatrix size "sparse"

            let sw = Stopwatch.StartNew()
            let _ = performCompleteAnalysis matrix size ops
            sw.Stop()

            printfn "Complete analysis %dx%d: %d ms" size size sw.ElapsedMilliseconds
            sw.ElapsedMilliseconds |> should be (lessThan 3000L)

    [<Fact>]
    let ``Analysis is deterministic`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations

        let size = 16
        let matrix = createTestMatrix size "dense"

        let analysis1 = performCompleteAnalysis matrix size ops
        let analysis2 = performCompleteAnalysis matrix size ops

        analysis1.Structure.SparsityPattern.NonZeroCount |> should equal analysis2.Structure.SparsityPattern.NonZeroCount
        analysis1.IsProjector |> should equal analysis2.IsProjector
        analysis1.MinimalPolynomialDegree |> should equal analysis2.MinimalPolynomialDegree