// 4-Hardware/2-CPUOperations.fs
namespace E8.Hardware.CPU

open System
open System.Runtime.Intrinsics
open System.Runtime.Intrinsics.X86
open System.Runtime.CompilerServices
open System.Threading.Tasks
open E8.Algebra
open E8.Hardware

/// AVX512/AVX2最適化CPU実装
type OptimizedCPUOperations() =

    // ハードウェア能力検出
    let hasAVX512 = Avx512F.IsSupported
    let hasAVX2 = Avx2.IsSupported
    let vectorSize =
        if hasAVX512 then 512 / 64  // 8要素
        elif hasAVX2 then 256 / 64   // 4要素
        else 1                        // スカラー

    let parallelism = Environment.ProcessorCount

    // メモリ統計
    let mutable totalAllocated = 0L
    let mutable inUse = 0L

    /// ポピュレーションカウント（1ビットの数を数える）
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline popcnt (value: uint64) =
        if Popcnt.X64.IsSupported then
            int(Popcnt.X64.PopCount(value))
        else
            // ソフトウェア実装
            let mutable v = value
            let mutable count = 0
            while v <> 0UL do
                count <- count + 1
                v <- v &&& (v - 1UL)
            count

    /// AVX512を使用した8要素並列XOR
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline vector8Xor (a: Vector512<uint64>) (b: Vector512<uint64>) =
        Avx512F.Xor(a, b)

    /// AVX2を使用した4要素並列XOR
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline vector4Xor (a: Vector256<uint64>) (b: Vector256<uint64>) =
        Avx2.Xor(a, b)

    /// F₂行列積の完全実装
    let matrixMultiplyCore (left: BitBlock64[]) (right: BitBlock64[])
                          (rows: int) (cols: int) (inner: int) =

        let result = Array.zeroCreate<BitBlock64>(rows * cols)

        // 行ごとに並列処理
        Parallel.For(0, rows, fun row ->

            if hasAVX512 && inner >= 8 then
                // AVX512最適化パス
                for col in 0 .. cols - 1 do
                    let mutable acc = Vector512<uint64>.Zero

                    // 8要素ずつ処理
                    let chunks = inner / 8
                    for chunk in 0 .. chunks - 1 do
                        // 左行列の行ベクトルをロード
                        let leftVals = Array.init 8 (fun i ->
                            left.[row * inner + chunk * 8 + i].Bits)
                        let leftVec = Vector512.Create(leftVals.[0], leftVals.[1],
                                                       leftVals.[2], leftVals.[3],
                                                       leftVals.[4], leftVals.[5],
                                                       leftVals.[6], leftVals.[7])

                        // 右行列の列ベクトルをロード
                        let rightVals = Array.init 8 (fun i ->
                            right.[(chunk * 8 + i) * cols + col].Bits)
                        let rightVec = Vector512.Create(rightVals.[0], rightVals.[1],
                                                        rightVals.[2], rightVals.[3],
                                                        rightVals.[4], rightVals.[5],
                                                        rightVals.[6], rightVals.[7])

                        // ビット単位AND
                        let andResult = Avx512F.And(leftVec, rightVec)

                        // XORで累積
                        acc <- vector8Xor acc andResult

                    // 残りの要素を処理
                    let mutable scalarAcc = 0UL
                    for k in (chunks * 8) .. inner - 1 do
                        let leftVal = left.[row * inner + k].Bits
                        let rightVal = right.[k * cols + col].Bits
                        scalarAcc <- scalarAcc ^^^ (leftVal &&& rightVal)

                    // ベクトル結果を集約
                    let mutable finalResult = scalarAcc
                    for i in 0 .. 7 do
                        finalResult <- finalResult ^^^ acc.GetElement(i)

                    // パリティ（F₂への射影）
                    result.[row * cols + col] <- { Bits = uint64(popcnt finalResult &&& 1) }

            elif hasAVX2 && inner >= 4 then
                // AVX2最適化パス
                for col in 0 .. cols - 1 do
                    let mutable acc = Vector256<uint64>.Zero

                    let chunks = inner / 4
                    for chunk in 0 .. chunks - 1 do
                        let leftVals = Array.init 4 (fun i ->
                            left.[row * inner + chunk * 4 + i].Bits)
                        let leftVec = Vector256.Create(leftVals.[0], leftVals.[1],
                                                       leftVals.[2], leftVals.[3])

                        let rightVals = Array.init 4 (fun i ->
                            right.[(chunk * 4 + i) * cols + col].Bits)
                        let rightVec = Vector256.Create(rightVals.[0], rightVals.[1],
                                                        rightVals.[2], rightVals.[3])

                        let andResult = Avx2.And(leftVec, rightVec)
                        acc <- vector4Xor acc andResult

                    let mutable scalarAcc = 0UL
                    for k in (chunks * 4) .. inner - 1 do
                        let leftVal = left.[row * inner + k].Bits
                        let rightVal = right.[k * cols + col].Bits
                        scalarAcc <- scalarAcc ^^^ (leftVal &&& rightVal)

                    let mutable finalResult = scalarAcc
                    for i in 0 .. 3 do
                        finalResult <- finalResult ^^^ acc.GetElement(i)

                    result.[row * cols + col] <- { Bits = uint64(popcnt finalResult &&& 1) }

            else
                // スカラー実装
                for col in 0 .. cols - 1 do
                    let mutable acc = 0UL
                    for k in 0 .. inner - 1 do
                        let leftVal = left.[row * inner + k].Bits
                        let rightVal = right.[k * cols + col].Bits
                        acc <- acc ^^^ (leftVal &&& rightVal)
                    result.[row * cols + col] <- { Bits = uint64(popcnt acc &&& 1) }
        )

        result

    /// 行列の等価性チェック（プロジェクター検証用）
    let matrixEqualsCore (matrix1: BitBlock64[]) (matrix2: BitBlock64[]) (size: int) =
        let mutable isEqual = true
        let mutable differences = 0

        // AVX512で8要素ずつ比較
        if hasAVX512 && size >= 8 then
            let chunks = size / 8

            for chunk in 0 .. chunks - 1 do
                let vals1 = Array.init 8 (fun i -> matrix1.[chunk * 8 + i].Bits)
                let vec1 = Vector512.Create(vals1.[0], vals1.[1], vals1.[2], vals1.[3],
                                           vals1.[4], vals1.[5], vals1.[6], vals1.[7])

                let vals2 = Array.init 8 (fun i -> matrix2.[chunk * 8 + i].Bits)
                let vec2 = Vector512.Create(vals2.[0], vals2.[1], vals2.[2], vals2.[3],
                                           vals2.[4], vals2.[5], vals2.[6], vals2.[7])

                let xorResult = vector8Xor vec1 vec2

                for i in 0 .. 7 do
                    if xorResult.GetElement(i) <> 0UL then
                        isEqual <- false
                        differences <- differences + popcnt(xorResult.GetElement(i))

            // 残りの要素
            for i in (chunks * 8) .. size - 1 do
                if matrix1.[i].Bits <> matrix2.[i].Bits then
                    isEqual <- false
                    differences <- differences + 1
        else
            // スカラー比較
            for i in 0 .. size - 1 do
                if matrix1.[i].Bits <> matrix2.[i].Bits then
                    isEqual <- false
                    differences <- differences + 1

        (isEqual, differences)

    /// トポロジカルランク計算の完全実装
    let computeTopologicalRankCore (matrix: BitBlock64[]) (rows: int) (cols: int) =

        // 作業用コピー（破壊的変更のため）
        let workMatrix = Array.copy matrix

        // 各行と列の重み（非ゼロ要素数）を計算
        let computeWeights() =
            let rowWeights = Array.zeroCreate<int> rows
            let colWeights = Array.zeroCreate<int> cols

            Parallel.For(0, rows, fun row ->
                let mutable weight = 0
                for col in 0 .. cols - 1 do
                    if workMatrix.[row * cols + col].Bits <> 0UL then
                        weight <- weight + 1
                rowWeights.[row] <- weight
            )

            Parallel.For(0, cols, fun col ->
                let mutable weight = 0
                for row in 0 .. rows - 1 do
                    if workMatrix.[row * cols + col].Bits <> 0UL then
                        weight <- weight + 1
                colWeights.[col] <- weight
            )

            (rowWeights, colWeights)

        let mutable pivots = []
        let mutable rank = 0
        let maxRank = min rows cols

        // ピボット選択とガウス消去
        for iteration in 0 .. maxRank - 1 do
            let (rowWeights, colWeights) = computeWeights()

            // 最大スコアのピボット探索
            let mutable maxScore = -1
            let mutable bestPivot = (-1, -1)

            for row in 0 .. rows - 1 do
                if rowWeights.[row] > 0 then
                    for col in 0 .. cols - 1 do
                        if colWeights.[col] > 0 && workMatrix.[row * cols + col].Bits <> 0UL then
                            let score = rowWeights.[row] * colWeights.[col]
                            if score > maxScore then
                                maxScore <- score
                                bestPivot <- (row, col)

            if maxScore > 0 then
                let (pivotRow, pivotCol) = bestPivot
                pivots <- (pivotRow * cols + pivotCol) :: pivots
                rank <- rank + 1

                // ガウス消去（F₂なのでXOR）
                Parallel.For(0, rows, fun row ->
                    if row <> pivotRow && workMatrix.[row * cols + pivotCol].Bits <> 0UL then
                        // 行全体をXOR
                        if hasAVX512 && cols >= 8 then
                            let chunks = cols / 8
                            for chunk in 0 .. chunks - 1 do
                                for i in 0 .. 7 do
                                    let idx = chunk * 8 + i
                                    if idx < cols then
                                        let rowIdx = row * cols + idx
                                        let pivotIdx = pivotRow * cols + idx
                                        workMatrix.[rowIdx] <- {
                                            Bits = workMatrix.[rowIdx].Bits ^^^ workMatrix.[pivotIdx].Bits
                                        }
                        else
                            for col in 0 .. cols - 1 do
                                let rowIdx = row * cols + col
                                let pivotIdx = pivotRow * cols + col
                                workMatrix.[rowIdx] <- {
                                    Bits = workMatrix.[rowIdx].Bits ^^^ workMatrix.[pivotIdx].Bits
                                }
                )

                // ピボット列をゼロクリア（次の反復のため）
                for row in 0 .. rows - 1 do
                    if row <> pivotRow then
                        workMatrix.[row * cols + pivotCol] <- { Bits = 0UL }
            else
                // これ以上ピボットが見つからない
                iteration <- maxRank  // ループ終了

        (rank, List.rev pivots |> List.toArray)

    interface IF2Operations with
        member _.MatrixMultiply left right rows cols inner =
            Interlocked.Add(&inUse, int64(rows * cols * 8)) |> ignore
            let result = matrixMultiplyCore left right rows cols inner
            Interlocked.Add(&inUse, -int64(rows * cols * 8)) |> ignore
            result

        member _.MatrixEquals matrix1 matrix2 size =
            matrixEqualsCore matrix1 matrix2 size

        member _.BatchMatrixMultiply matrices dimensions =
            let results = Array.zeroCreate<BitBlock64[]>(matrices.Length)

            // 並列バッチ処理
            let chunkSize = max 1 (matrices.Length / parallelism)

            Parallel.For(0, parallelism, fun thread ->
                let startIdx = thread * chunkSize
                let endIdx = min matrices.Length ((thread + 1) * chunkSize)

                for i in startIdx .. endIdx - 1 do
                    let (left, right) = matrices.[i]
                    let (r, c, k) = dimensions.[i]
                    results.[i] <- matrixMultiplyCore left right r c k
            )

            results

        member _.ComputeTopologicalRank matrix rows cols =
            computeTopologicalRankCore matrix rows cols

        member _.ParallelXor vectors =
            let len = vectors.[0].Length
            let result = Array.zeroCreate<BitBlock64> len

            if hasAVX512 && len >= 8 then
                let chunks = len / 8

                Parallel.For(0, chunks, fun chunk ->
                    let mutable acc = Vector512<uint64>.Zero

                    for vec in vectors do
                        let vals = Array.init 8 (fun i -> vec.[chunk * 8 + i].Bits)
                        let v = Vector512.Create(vals.[0], vals.[1], vals.[2], vals.[3],
                                                vals.[4], vals.[5], vals.[6], vals.[7])
                        acc <- vector8Xor acc v

                    for i in 0 .. 7 do
                        result.[chunk * 8 + i] <- { Bits = acc.GetElement(i) }
                )

                // 残り要素
                for i in (chunks * 8) .. len - 1 do
                    let mutable acc = 0UL
                    for vec in vectors do
                        acc <- acc ^^^ vec.[i].Bits
                    result.[i] <- { Bits = acc }
            else
                // スカラー実装
                Parallel.For(0, len, fun i ->
                    let mutable acc = 0UL
                    for vec in vectors do
                        acc <- acc ^^^ vec.[i].Bits
                    result.[i] <- { Bits = acc }
                )

            result

        member _.PopCount value = popcnt value

        member _.DeviceInfo() =
            {
                DeviceType = CPU(parallelism, hasAVX512, hasAVX2)
                MaxThreads = parallelism
                MaxMemory = GC.GetTotalMemory(false)
                BitwiseOpsPerClock = vectorSize * 64
                SupportsInt64Atomics = true
                WarpSize = None
            }

        member _.MemoryInfo() =
            {
                TotalAllocated = totalAllocated
                InUse = inUse
                PoolSize = 0L
                PinnedMemory = None
            }

        member _.Dispose() =
            GC.Collect()
            GC.WaitForPendingFinalizers()
