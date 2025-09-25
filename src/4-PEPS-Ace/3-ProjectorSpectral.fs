// src/4-PEPS-Ace/3-ProjectorSpectral.fs
namespace E8.Ace

open System
open E8.Algebra
open E8.Tensors
open E8.Hardware

/// プロジェクター仮説に基づくスペクトル解析
module ProjectorSpectral =

    /// スペクトル情報
    type SpectralInfo = {
        /// 最大固有値（F₂では常に0か1）
        LeadingEigenvalue: F2
        /// スペクトルギャップ
        SpectralGap: F2
        /// 縮退度
        Degeneracy: int
        /// 最小多項式の次数
        MinimalPolynomialDegree: int
    }

    /// プロジェクター検証結果
    type ProjectorVerification = {
        /// T² = Tを満たすか
        IsProjector: bool
        /// 差分の数（T²とTの）
        DifferenceCount: int
        /// ランク（プロジェクターの場合）
        Rank: int option
        /// トレース（対角和）
        Trace: int
    }

    /// 転送行列のタイプ
    type TransferMatrixType =
        | Horizontal of bondDimension: int
        | Vertical of bondDimension: int
        | Corner of bondDimension: int

    /// 転送行列を構築
    let constructTransferMatrix (env: ACE_CTMRG.SymmetricEnvironment) (peps: Tensor5F2) (matrixType: TransferMatrixType) (ops: IF2Operations) =

        let chi = env.Chi
        let D = peps.D2  // ボンド次元

        let size =
            match matrixType with
            | Horizontal d | Vertical d | Corner d -> chi * d

        let wordsPerRow = (size + 63) / 64
        let transfer = Array.zeroCreate (size * wordsPerRow)

        // 簡略化：環境テンソルから転送行列を構築
        match matrixType with
        | Horizontal _ ->
            // 水平転送行列：左右のエッジを結合
            for i in 0 .. min size env.UniqueEdge.Length - 1 do
                transfer.[i] <- env.UniqueEdge.[i]

        | Vertical _ ->
            // 垂直転送行列：上下のエッジを結合
            for i in 0 .. min size env.UniqueEdge.Length - 1 do
                transfer.[i] <- env.UniqueEdge.[env.UniqueEdge.Length - 1 - i]

        | Corner _ ->
            // コーナー転送行列：コーナーを使用
            for i in 0 .. min size env.UniqueCorner.Length - 1 do
                transfer.[i] <- env.UniqueCorner.[i]

        transfer

    /// プロジェクター仮説を検証：T² = T
    let verifyProjectorHypothesis (transfer: BitBlock64[]) (size: int) (ops: IF2Operations) =

        // T²を計算
        let tSquared = ops.MatrixMultiply transfer transfer size size size

        // T² = Tを検証
        let (isEqual, differences) = ops.MatrixEquals transfer tSquared size

        // ランクを計算（プロジェクターの場合、ランク = トレース）
        let (rank, _) = ops.ComputeTopologicalRank transfer size size

        // トレースを計算（対角要素の和）
        let wordsPerRow = (size + 63) / 64
        let mutable trace = 0
        for i in 0 .. size - 1 do
            let wordIdx = i * wordsPerRow + i / 64
            let bitIdx = i % 64
            if wordIdx < transfer.Length then
                if (transfer.[wordIdx].Bits >>> bitIdx) &&& 1UL = 1UL then
                    trace <- trace + 1

        {
            IsProjector = isEqual
            DifferenceCount = differences
            Rank = if isEqual then Some rank else None
            Trace = trace
        }

    /// スペクトル情報を計算
    let computeSpectralInfo (transfer: BitBlock64[]) (size: int) (isProjector: bool) (ops: IF2Operations) =

        if isProjector then
            // プロジェクターの場合：固有値は0と1のみ
            {
                LeadingEigenvalue = F2.One
                SpectralGap = F2.One  // 1 - 0 = 1
                Degeneracy = 1  // 簡略化
                MinimalPolynomialDegree = 2  // x² - x = 0
            }
        else
            // 一般の場合：Wiedemann法が必要（ここでは簡略化）
            let (rank, _) = ops.ComputeTopologicalRank transfer size size

            {
                LeadingEigenvalue = if rank > 0 then F2.One else F2.Zero
                SpectralGap = F2.Zero  // 不明
                Degeneracy = 1
                MinimalPolynomialDegree = min size (rank + 1)
            }

    /// 最小多項式を計算（Wiedemann法の簡略版）
    let computeMinimalPolynomial (transfer: BitBlock64[]) (size: int) (ops: IF2Operations) =

        // ランダムベクトル
        let rng = Random(42)
        let v = Array.init size (fun _ -> { Bits = uint64 (rng.Next()) })

        // Krylov系列：v, Av, A²v, ...
        let maxDegree = min size 10  // 制限
        let krylov = Array.zeroCreate (maxDegree + 1)
        krylov.[0] <- v

        for i in 1 .. maxDegree do
            krylov.[i] <- ops.MatrixMultiply transfer krylov.[i-1] size size 1

        // 線形依存性を検出（簡略化）
        let mutable degree = maxDegree
        for i in 1 .. maxDegree do
            let (isEqual, _) = ops.MatrixEquals krylov.[i] krylov.[0] size
            if isEqual && degree = maxDegree then
                degree <- i

        degree

    /// 完全なスペクトル解析
    type SpectralAnalysisResult = {
        ProjectorStatus: ProjectorVerification option
        SpectralInfo: SpectralInfo option
        MinimalPolynomialDegree: int
        IsTopologicallyOrdered: bool
    }

    /// 環境のスペクトル解析を実行
    let analyzeEnvironmentSpectrum (env: ACE_CTMRG.SymmetricEnvironment) (peps: Tensor5F2) (ops: IF2Operations) =

        // 水平転送行列
        let hTransfer = constructTransferMatrix env peps (Horizontal peps.D2) ops
        let hSize = env.Chi * peps.D2

        // プロジェクター仮説を検証
        let projectorResult = verifyProjectorHypothesis hTransfer hSize ops

        // スペクトル情報を計算
        let spectralInfo = computeSpectralInfo hTransfer hSize projectorResult.IsProjector ops

        // 最小多項式の次数
        let minPolyDegree =
            if projectorResult.IsProjector then
                2  // x² - x
            else
                computeMinimalPolynomial hTransfer hSize ops

        // トポロジカル秩序の判定
        let isTopological =
            projectorResult.IsProjector &&
            spectralInfo.SpectralGap = F2.One &&
            projectorResult.Rank.IsSome &&
            projectorResult.Rank.Value > 0

        {
            ProjectorStatus = Some projectorResult
            SpectralInfo = Some spectralInfo
            MinimalPolynomialDegree = minPolyDegree
            IsTopologicallyOrdered = isTopological
        }