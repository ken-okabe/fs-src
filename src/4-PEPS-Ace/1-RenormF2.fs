// 5-Ace/1-RenormF2.fs
namespace E8.Ace

open System
open System.Threading.Tasks
open E8.Algebra
open E8.Hardware

/// Renorm-F2: F₂上の厳密な代数的次元削減
/// TR-TCI (Topological Rank - Tensor Cross Interpolation)の完全実装
module RenormF2 =

    /// 圧縮パラメータ
    type CompressionParams = {
        TargetChi: int
        MinChi: int
        MaxChi: int
        AdaptiveThreshold: float  // 0.0-1.0の範囲で適応的圧縮率
        PreserveSymmetry: bool    // D4対称性を保持するか
    }

    /// 圧縮結果の詳細情報
    type CompressionResult = {
        CompressedMatrix: BitBlock64[]
        Projector: BitBlock64[]
        EffectiveRank: int
        OriginalRank: int
        CompressionRatio: float
        Pivots: int[]
        RowWeights: int[]
        ColWeights: int[]
        ExecutionTimeMs: float
    }

    /// ピボット選択戦略の実装
    type PivotSelector(ops: IF2Operations) =

        /// 最大接続性戦略：行重み×列重みが最大となるピボットを選択
        member _.SelectByMaximalConnectivity(matrix: BitBlock64[], rows: int, cols: int) =

            // 行と列の重み計算
            let computeWeights() =
                let rowWeights = Array.zeroCreate<int> rows
                let colWeights = Array.zeroCreate<int> cols

                // 並列で重み計算
                Parallel.For(0, rows, fun row ->
                    let mutable weight = 0
                    for col in 0 .. cols - 1 do
                        if matrix.[row * cols + col].Bits <> 0UL then
                            weight <- weight + 1
                    rowWeights.[row] <- weight
                )

                Parallel.For(0, cols, fun col ->
                    let mutable weight = 0
                    for row in 0 .. rows - 1 do
                        if matrix.[row * cols + col].Bits <> 0UL then
                            weight <- weight + 1
                    colWeights.[col] <- weight
                )

                (rowWeights, colWeights)

            let (rowWeights, colWeights) = computeWeights()

            // 最大スコアのピボット探索
            let mutable maxScore = -1
            let mutable bestPivot = (-1, -1)

            for row in 0 .. rows - 1 do
                if rowWeights.[row] > 0 then
                    for col in 0 .. cols - 1 do
                        if colWeights.[col] > 0 && matrix.[row * cols + col].Bits <> 0UL then
                            let score = rowWeights.[row] * colWeights.[col]
                            if score > maxScore then
                                maxScore <- score
                                bestPivot <- (row, col)

            if maxScore > 0 then
                Some(bestPivot, rowWeights, colWeights)
            else
                None

        /// バランス木戦略：行と列の重みのバランスを考慮
        member _.SelectByBalancedTree(matrix: BitBlock64[], rows: int, cols: int) =

            let (rowWeights, colWeights) =
                let rw = Array.zeroCreate<int> rows
                let cw = Array.zeroCreate<int> cols

                for row in 0 .. rows - 1 do
                    for col in 0 .. cols - 1 do
                        if matrix.[row * cols + col].Bits <> 0UL then
                            rw.[row] <- rw.[row] + 1
                            cw.[col] <- cw.[col] + 1

                (rw, cw)

            // バランススコア = min(行重み, 列重み) * (行重み + 列重み)
            let mutable maxScore = -1
            let mutable bestPivot = (-1, -1)

            for row in 0 .. rows - 1 do
                for col in 0 .. cols - 1 do
                    if matrix.[row * cols + col].Bits <> 0UL then
                        let balance = min rowWeights.[row] colWeights.[col]
                        let total = rowWeights.[row] + colWeights.[col]
                        let score = balance * total

                        if score > maxScore then
                            maxScore <- score
                            bestPivot <- (row, col)

            if maxScore > 0 then
                Some(bestPivot, rowWeights, colWeights)
            else
                None

    /// トポロジカルランクに基づく圧縮の完全実装
    let truncate (matrix: BitBlock64[]) (rows: int) (cols: int)
                (params: CompressionParams) (ops: IF2Operations) =

        let timer = System.Diagnostics.Stopwatch.StartNew()

        // トポロジカルランク計算（ハードウェア最適化済み）
        let (fullRank, pivots) = ops.ComputeTopologicalRank matrix rows cols

        // 実効的な圧縮サイズ決定
        let effectiveChi =
            if params.AdaptiveThreshold > 0.0 then
                // 適応的圧縮：ランクの一定割合を保持
                let adaptive = int(float fullRank * params.AdaptiveThreshold)
                min (max adaptive params.MinChi) params.MaxChi
            else
                // 固定サイズ圧縮
                min params.TargetChi fullRank |> min params.MaxChi |> max params.MinChi

        // ピボット選択
        let selectedPivots =
            if pivots.Length >= effectiveChi then
                Array.take effectiveChi pivots
            else
                pivots  // 全ピボットを使用

        // プロジェクター行列の構築
        let buildProjector() =
            let projector = Array.zeroCreate<BitBlock64>(rows * effectiveChi)

            selectedPivots |> Array.iteri (fun newCol pivotIdx ->
                let pivotRow = pivotIdx / cols
                let pivotCol = pivotIdx % cols

                // 基底ベクトルの設定
                for row in 0 .. rows - 1 do
                    if row = pivotRow then
                        projector.[row * effectiveChi + newCol] <- { Bits = 1UL }
                    else
                        // 線形結合の係数（トポロジカル構造を保持）
                        let coeff =
                            if matrix.[row * cols + pivotCol].Bits <> 0UL then
                                { Bits = 1UL }
                            else
                                { Bits = 0UL }
                        projector.[row * effectiveChi + newCol] <- coeff
            )

            projector

        let projector = buildProjector()

        // 圧縮実行: C_new = P^T × C × P
        let compressed =
            // Step 1: temp = P^T × C
            let temp = ops.MatrixMultiply
                           (Array.init (effectiveChi * rows) (fun i ->
                               projector.[(i % rows) * effectiveChi + (i / rows)]))  // 転置
                           matrix effectiveChi cols rows

            // Step 2: C_new = temp × P
            ops.MatrixMultiply temp projector effectiveChi effectiveChi cols

        // D4対称性の保持（必要な場合）
        let finalMatrix =
            if params.PreserveSymmetry then
                enforceD4Symmetry compressed effectiveChi ops
            else
                compressed

        timer.Stop()

        // 詳細な統計情報を計算
        let selector = PivotSelector(ops)
        let weights =
            match selector.SelectByMaximalConnectivity(matrix, rows, cols) with
            | Some(_, rw, cw) -> (rw, cw)
            | None -> (Array.zeroCreate rows, Array.zeroCreate cols)

        {
            CompressedMatrix = finalMatrix
            Projector = projector
            EffectiveRank = effectiveChi
            OriginalRank = fullRank
            CompressionRatio = float effectiveChi / float(rows * cols)
            Pivots = selectedPivots
            RowWeights = fst weights
            ColWeights = snd weights
            ExecutionTimeMs = timer.Elapsed.TotalMilliseconds
        }

    /// D4対称性を強制する
    and enforceD4Symmetry (matrix: BitBlock64[]) (size: int) (ops: IF2Operations) =
        let result = Array.copy matrix

        // 8つの対称操作の平均を取る（F₂では多数決）
        for i in 0 .. size - 1 do
            for j in 0 .. size - 1 do
                let idx = i * size + j
                let mutable votes = 0

                // 元の値
                if matrix.[idx].Bits <> 0UL then votes <- votes + 1

                // 90度回転
                let rot90_i = j
                let rot90_j = size - 1 - i
                if matrix.[rot90_i * size + rot90_j].Bits <> 0UL then votes <- votes + 1

                // 180度回転
                let rot180_i = size - 1 - i
                let rot180_j = size - 1 - j
                if matrix.[rot180_i * size + rot180_j].Bits <> 0UL then votes <- votes + 1

                // 270度回転
                let rot270_i = size - 1 - j
                let rot270_j = i
                if matrix.[rot270_i * size + rot270_j].Bits <> 0UL then votes <- votes + 1

                // 水平反転
                let flip_h_i = i
                let flip_h_j = size - 1 - j
                if matrix.[flip_h_i * size + flip_h_j].Bits <> 0UL then votes <- votes + 1

                // 垂直反転
                let flip_v_i = size - 1 - i
                let flip_v_j = j
                if matrix.[flip_v_i * size + flip_v_j].Bits <> 0UL then votes <- votes + 1

                // 対角反転
                let diag1_i = j
                let diag1_j = i
                if matrix.[diag1_i * size + diag1_j].Bits <> 0UL then votes <- votes + 1

                // 反対角反転
                let diag2_i = size - 1 - j
                let diag2_j = size - 1 - i
                if matrix.[diag2_i * size + diag2_j].Bits <> 0UL then votes <- votes + 1

                // 多数決（4以上なら1）
                result.[idx] <- if votes >= 4 then { Bits = 1UL } else { Bits = 0UL }

        result

    /// バッチ圧縮の完全実装
    let batchTruncate (matrices: BitBlock64[][])
                     (dimensions: (int * int)[])
                     (params: CompressionParams)
                     (ops: IF2Operations) =

        let results = Array.zeroCreate<CompressionResult> matrices.Length

        // 並列バッチ処理
        let parallelOptions = ParallelOptions()
        parallelOptions.MaxDegreeOfParallelism <- Environment.ProcessorCount

        Parallel.For(0, matrices.Length, parallelOptions, fun i ->
            let matrix = matrices.[i]
            let (rows, cols) = dimensions.[i]
            results.[i] <- truncate matrix rows cols params ops
        )

        results

    /// 適応的圧縮（環境に基づいて圧縮率を動的調整）
    let adaptiveTruncate (matrix: BitBlock64[]) (rows: int) (cols: int)
                        (targetMemoryMB: int) (ops: IF2Operations) =

        // メモリ制約から目標サイズを計算
        let currentMemory = int64(rows * cols * 8) / (1024L * 1024L)
        let targetRatio = float targetMemoryMB / float currentMemory

        let params = {
            TargetChi = 0  // 適応的なので無視
            MinChi = 2
            MaxChi = min rows cols
            AdaptiveThreshold = min targetRatio 1.0
            PreserveSymmetry = true
        }

        truncate matrix rows cols params ops