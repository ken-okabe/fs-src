// 5-Observable/5-SpectralAnalysis.fs
namespace E8.Observable

open System
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.Ace

/// スペクトル解析による物理量計算
module SpectralAnalysis =

    /// 相関長の計算
    let computeCorrelationLength (env: ACE_CTMRG.SymmetricEnvironment)
                                (peps: Tensor5F2)
                                (ops: IF2Operations) =

        // 転送行列を構築
        let transfer = ProjectorSpectral.constructTransferMatrix
                           env peps (ProjectorSpectral.Horizontal env.Chi) ops

        // プロジェクター仮説を検証
        let verified = ProjectorSpectral.verifyProjectorHypothesis transfer ops

        match verified.ProjectorStatus, verified.SpectralInfo with
        | Some status, Some info when status.IsProjector ->
            // プロジェクターなら相関長は1
            1
        | Some _, Some info ->
            // 一般の場合：ξ = -1/log(λ₂/λ₁)
            // F₂では固有値は0か1なので、簡略化
            if info.SpectralGap = F2.One then
                1  // 完全ギャップ
            else
                info.MinimalPolynomialDegree  // 最小多項式の次数で近似
        | _ ->
            Int32.MaxValue  // 無限大（ギャップレス）

    /// スペクトルギャップの計算
    let computeSpectralGap (env: ACE_CTMRG.SymmetricEnvironment)
                          (peps: Tensor5F2)
                          (construction: ProjectorSpectral.TransferMatrixConstruction)
                          (ops: IF2Operations) =

        let transfer = ProjectorSpectral.constructTransferMatrix env peps construction ops
        let verified = ProjectorSpectral.verifyProjectorHypothesis transfer ops

        match verified.SpectralInfo with
        | Some info -> info.SpectralGap
        | None -> F2.Zero

    /// エンタングルメントエントロピー（スペクトルから）
    let computeEntanglementEntropy (env: ACE_CTMRG.SymmetricEnvironment)
                                  (peps: Tensor5F2)
                                  (bipartitionSize: int)
                                  (ops: IF2Operations) =

        // 縦方向の転送行列
        let transferV = ProjectorSpectral.constructTransferMatrix
                            env peps (ProjectorSpectral.Vertical env.Chi) ops

        // 横方向の転送行列
        let transferH = ProjectorSpectral.constructTransferMatrix
                            env peps (ProjectorSpectral.Horizontal env.Chi) ops

        // 両方向のスペクトルを検証
        let verifiedV = ProjectorSpectral.verifyProjectorHypothesis transferV ops
        let verifiedH = ProjectorSpectral.verifyProjectorHypothesis transferH ops

        // エンタングルメントスペクトルから計算
        let computeFromSpectrum (verified: ProjectorSpectral.TransferMatrix) =
            match verified.SpectralInfo with
            | Some info ->
                // フォン・ノイマンエントロピーのF₂版
                let p1 = float info.Rank / float verified.Dimension
                let p0 = 1.0 - p1

                if p1 > 0.0 && p1 < 1.0 then
                    -p1 * Math.Log(p1, 2.0) - p0 * Math.Log(p0, 2.0)
                else
                    0.0
            | None -> 0.0

        // 両方向の平均
        let entropyV = computeFromSpectrum verifiedV
        let entropyH = computeFromSpectrum verifiedH

        // 面積則補正（bipartitionサイズに比例）
        let areaLaw = Math.Log(float bipartitionSize, 2.0)

        // トポロジカル補正項
        let topoCorrection =
            if verifiedV.ProjectorStatus.IsSome &&
               verifiedV.ProjectorStatus.Value.IsProjector then
                // フィボナッチエニオンの補正
                Math.Log((1.0 + Math.Sqrt(5.0)) / 2.0, 2.0)
            else
                0.0

        (entropyV + entropyH) / 2.0 - areaLaw + topoCorrection

    /// 中心電荷の推定
    let estimateCentralCharge (env: ACE_CTMRG.SymmetricEnvironment)
                             (peps: Tensor5F2)
                             (systemSize: int)
                             (ops: IF2Operations) =

        // コーナー転送行列
        let transferC = ProjectorSpectral.constructTransferMatrix
                            env peps (ProjectorSpectral.Corner env.Chi) ops

        let verified = ProjectorSpectral.verifyProjectorHypothesis transferC ops

        match verified.SpectralInfo with
        | Some info when info.MinimalPolynomialDegree = 2 ->
            // プロジェクターの場合
            // フィボナッチCFTの中心電荷 c = 7/10
            0.7
        | Some info ->
            // 一般の場合：最小多項式の次数から推定
            float info.MinimalPolynomialDegree / 10.0
        | None ->
            0.0

    /// 秩序パラメータの相関長
    let computeOrderParameterCorrelation (env: ACE_CTMRG.SymmetricEnvironment)
                                        (peps: Tensor5F2)
                                        (orderType: string)
                                        (ops: IF2Operations) =

        // 転送行列のスペクトル解析
        let correlationLength = computeCorrelationLength env peps ops

        match orderType with
        | "ferromagnetic" ->
            // 強磁性秩序：長距離相関
            if correlationLength = Int32.MaxValue then
                Float.PositiveInfinity
            else
                float correlationLength

        | "antiferromagnetic" ->
            // 反強磁性秩序：交代相関
            if correlationLength = 1 then
                0.0  // 短距離秩序
            else
                float correlationLength * 2.0  // 倍周期

        | "topological" ->
            // トポロジカル秩序：指数的減衰なし
            if correlationLength = 1 then
                Float.PositiveInfinity  // 完全なトポロジカル秩序
            else
                1.0 / float correlationLength

        | _ ->
            float correlationLength

/// スペクトル観測量の統合インターフェース
type SpectralObservable(measureType: string) =

    let observableKind =
        match measureType with
        | "correlation_length" -> LocalParity
        | "spectral_gap" -> LocalParity
        | "entanglement" -> TopologicalEntropy
        | "central_charge" -> TopologicalEntropy
        | _ -> LocalParity

    interface IObservable with
        member _.Kind = observableKind

        member _.Compute environment peps location ops =
            let value =
                match measureType with
                | "correlation_length" ->
                    let length = SpectralAnalysis.computeCorrelationLength
                                     environment peps ops
                    if length = 1 then F2.One else F2.Zero

                | "spectral_gap" ->
                    let gap = SpectralAnalysis.computeSpectralGap
                                  environment peps
                                  (ProjectorSpectral.Horizontal environment.Chi) ops
                    gap

                | "entanglement" ->
                    let entropy = SpectralAnalysis.computeEntanglementEntropy
                                      environment peps location.Radius ops
                    if entropy > 0.5 then F2.One else F2.Zero

                | "central_charge" ->
                    let c = SpectralAnalysis.estimateCentralCharge
                                environment peps environment.Chi ops
                    if abs(c - 0.7) < 0.1 then F2.One else F2.Zero  // c=7/10検出

                | _ -> F2.Zero

            {
                Parity = value
                ContributionCount = environment.Chi * environment.Chi
                Confidence = ObservableUtils.computeConfidence environment
            }

        member this.ComputeBatch environment peps locations ops =
            locations
            |> Array.Parallel.map (fun loc ->
                (this :> IObservable).Compute environment peps loc ops)
