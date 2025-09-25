// src/4-PEPS-Ace/1-RenormF2.fs
namespace E8.Ace

open System
open E8.Algebra
open E8.Hardware

/// Renorm-F2: F₂上の厳密な代数的繰り込みアルゴリズム
module RenormF2 =

    /// トポロジカルランクに基づく重要度スコア
    type ImportanceScore = {
        RowIndex: int
        ColIndex: int
        RowWeight: int
        ColWeight: int
        Score: int
    }

    /// 繰り込み結果
    type RenormalizationResult = {
        /// 左基底行列 U
        LeftBasis: BitBlock64[]
        /// 右基底行列 V
        RightBasis: BitBlock64[]
        /// 実際に保持されたランク
        ActualRank: int
        /// 破棄された情報の量
        DiscardedRank: int
        /// 選択されたピボット
        SelectedPivots: (int * int)[]
    }

    /// 行列のトポロジカルランクを計算
    let computeImportanceScores (matrix: BitBlock64[]) (rows: int) (cols: int) : ImportanceScore[] =
        let wordsPerRow = (cols + 63) / 64
        let scores = ResizeArray<ImportanceScore>()

        // 各行と列の重みを計算
        let rowWeights = Array.zeroCreate rows
        let colWeights = Array.zeroCreate cols

        for i in 0 .. rows - 1 do
            let mutable weight = 0
            for j in 0 .. cols - 1 do
                let wordIdx = i * wordsPerRow + j / 64
                let bitIdx = j % 64
                if (matrix.[wordIdx].Bits >>> bitIdx) &&& 1UL <> 0UL then
                    weight <- weight + 1
                    colWeights.[j] <- colWeights.[j] + 1
            rowWeights.[i] <- weight

        // 非零要素のスコアを計算
        for i in 0 .. rows - 1 do
            for j in 0 .. cols - 1 do
                let wordIdx = i * wordsPerRow + j / 64
                let bitIdx = j % 64
                if (matrix.[wordIdx].Bits >>> bitIdx) &&& 1UL <> 0UL then
                    scores.Add({
                        RowIndex = i
                        ColIndex = j
                        RowWeight = rowWeights.[i]
                        ColWeight = colWeights.[j]
                        Score = rowWeights.[i] * colWeights.[j]
                    })

        scores.ToArray() |> Array.sortByDescending (fun s -> s.Score)

    /// ガウス消去を実行（F₂上）
    let private gaussianElimination (matrix: BitBlock64[]) (pivotRow: int) (pivotCol: int) (rows: int) (cols: int) =
        let wordsPerRow = (cols + 63) / 64
        let pivotWordIdx = pivotRow * wordsPerRow + pivotCol / 64
        let pivotBitIdx = pivotCol % 64

        // ピボット列の他の1を消去
        for i in 0 .. rows - 1 do
            if i <> pivotRow then
                let wordIdx = i * wordsPerRow + pivotCol / 64
                let bitIdx = pivotCol % 64
                if (matrix.[wordIdx].Bits >>> bitIdx) &&& 1UL <> 0UL then
                    // 行全体をXOR
                    for w in 0 .. wordsPerRow - 1 do
                        let idx1 = i * wordsPerRow + w
                        let idx2 = pivotRow * wordsPerRow + w
                        matrix.[idx1] <- { Bits = matrix.[idx1].Bits ^^^ matrix.[idx2].Bits }

    /// Renorm-F2の主要アルゴリズム
    let renormalize (matrix: BitBlock64[]) (rows: int) (cols: int) (targetRank: int) (ops: IF2Operations) : RenormalizationResult =

        // 作業用行列のコピー
        let workMatrix = Array.copy matrix

        // トポロジカルランクによる重要度スコア計算
        let scores = computeImportanceScores workMatrix rows cols

        // ピボット選択
        let selectedPivots = ResizeArray<(int * int)>()
        let usedRows = Array.zeroCreate<bool> rows
        let usedCols = Array.zeroCreate<bool> cols

        let mutable currentRank = 0
        let mutable scoreIndex = 0

        while currentRank < targetRank && scoreIndex < scores.Length do
            let score = scores.[scoreIndex]

            if not usedRows.[score.RowIndex] && not usedCols.[score.ColIndex] then
                selectedPivots.Add((score.RowIndex, score.ColIndex))
                usedRows.[score.RowIndex] <- true
                usedCols.[score.ColIndex] <- true

                // ガウス消去
                gaussianElimination workMatrix score.RowIndex score.ColIndex rows cols
                currentRank <- currentRank + 1

            scoreIndex <- scoreIndex + 1

        // 基底行列の構築
        let leftBasis = Array.zeroCreate (rows * ((targetRank + 63) / 64))
        let rightBasis = Array.zeroCreate (targetRank * ((cols + 63) / 64))

        for i in 0 .. currentRank - 1 do
            let (pivotRow, pivotCol) = selectedPivots.[i]

            // 左基底（U）: 選択された行
            let wordsPerRow = (cols + 63) / 64
            for w in 0 .. wordsPerRow - 1 do
                let srcIdx = pivotRow * wordsPerRow + w
                let dstIdx = i * wordsPerRow + w
                if dstIdx < leftBasis.Length && srcIdx < matrix.Length then
                    leftBasis.[dstIdx] <- matrix.[srcIdx]

            // 右基底（V）: 選択された列（転置）
            for r in 0 .. rows - 1 do
                let srcWordIdx = r * wordsPerRow + pivotCol / 64
                let srcBitIdx = pivotCol % 64
                if srcWordIdx < matrix.Length then
                    let bit = (matrix.[srcWordIdx].Bits >>> srcBitIdx) &&& 1UL
                    if bit = 1UL then
                        let dstWordIdx = i * ((cols + 63) / 64) + r / 64
                        let dstBitIdx = r % 64
                        if dstWordIdx < rightBasis.Length then
                            rightBasis.[dstWordIdx] <-
                                { Bits = rightBasis.[dstWordIdx].Bits ||| (1UL <<< dstBitIdx) }

        // 元の行列のランクを計算
        let (originalRank, _) = ops.ComputeTopologicalRank matrix rows cols

        {
            LeftBasis = leftBasis |> Array.map (fun b -> { Bits = b.Bits })
            RightBasis = rightBasis |> Array.map (fun b -> { Bits = b.Bits })
            ActualRank = currentRank
            DiscardedRank = max 0 (originalRank - currentRank)
            SelectedPivots = selectedPivots.ToArray()
        }

    /// 環境テンソルの繰り込み（CTMRG用）
    let renormalizeEnvironment (corner: BitBlock64[]) (edge: BitBlock64[]) (chi: int) (chiNew: int) (ops: IF2Operations) =

        // コーナーとエッジを結合した転送行列
        let transferSize = chi * chi
        let transfer = Array.zeroCreate transferSize

        // 簡略化：コーナーをそのまま使用
        let cornerSize = min corner.Length transferSize
        Array.Copy(corner, transfer, cornerSize)

        // 繰り込み実行
        let result = renormalize transfer chi chi chiNew ops

        // 新しい環境テンソル
        let newCorner = result.LeftBasis
        let newEdge = result.RightBasis

        (newCorner, newEdge, result)