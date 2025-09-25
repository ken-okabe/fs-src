// src/4-PEPS-Ace/2-ACE_CTMRG.fs
namespace E8.Ace

open System
open System.Collections.Generic
open E8.Algebra
open E8.Tensors
open E8.Hardware

/// ACE (Algebraic Computation Engine) CTMRG実装
module ACE_CTMRG =

    /// 環境テンソル
    type Environment = {
        /// コーナー行列 C[corner_type]
        Corners: Map<CornerType, BitBlock64[]>
        /// エッジテンソル T[edge_type]
        Edges: Map<EdgeType, BitBlock64[]>
        /// 現在のボンド次元
        Chi: int
        /// 反復回数
        Iteration: int
    }

    and CornerType =
        | TopLeft | TopRight | BottomLeft | BottomRight

    and EdgeType =
        | Top | Right | Bottom | Left

    /// 対称性を持つ環境（D4群）
    type SymmetricEnvironment = {
        /// ユニークなコーナー（対称性により1つ）
        UniqueCorner: BitBlock64[]
        /// ユニークなエッジ（対称性により1つ）
        UniqueEdge: BitBlock64[]
        /// ボンド次元
        Chi: int
        /// 収束履歴
        History: ConvergenceHistory
    }

    and ConvergenceHistory = {
        /// 各反復での状態ハッシュ
        StateHashes: int[]
        /// 圧縮率の履歴
        CompressionRatios: float[]
        /// 有効ランクの履歴
        EffectiveRanks: int[]
        /// スペクトルギャップの履歴
        SpectralGaps: F2[]
        /// ノルムの履歴
        Norms: uint64[]
    }

    /// CTMRGの設定
    type CTMRGConfig = {
        /// 初期ボンド次元
        InitialChi: int
        /// 最大ボンド次元
        MaxChi: int
        /// 収束閾値（状態が変化しなくなるまでの反復）
        ConvergenceThreshold: int
        /// 最大反復回数
        MaxIterations: int
    }

    /// CTM移動：環境を成長させる
    let private ctmMove (env: SymmetricEnvironment) (peps: Tensor5F2) (ops: IF2Operations) =

        let chi = env.Chi
        let d = peps.D1  // 物理次元
        let D = peps.D2  // ボンド次元

        // 新しい環境のサイズ
        let chiNew = chi * D

        // コーナーの成長
        let grownCorner = Array.zeroCreate (chiNew * chiNew / 64 + 1)

        // 簡略化：既存のコーナーをコピーして拡張
        for i in 0 .. min chi chiNew - 1 do
            for j in 0 .. min chi chiNew - 1 do
                let oldIdx = i * ((chi + 63) / 64) + j / 64
                let oldBit = j % 64
                let newIdx = i * ((chiNew + 63) / 64) + j / 64
                let newBit = j % 64

                if oldIdx < env.UniqueCorner.Length then
                    let bit = (env.UniqueCorner.[oldIdx].Bits >>> oldBit) &&& 1UL
                    if bit = 1UL then
                        grownCorner.[newIdx] <-
                            { Bits = grownCorner.[newIdx].Bits ||| (1UL <<< newBit) }

        // エッジの成長（同様に簡略化）
        let grownEdge = Array.zeroCreate (chiNew * chiNew * d / 64 + 1)

        for i in 0 .. min chi chiNew - 1 do
            for j in 0 .. min chi chiNew - 1 do
                for p in 0 .. d - 1 do
                    let oldIdx = (i * chi + j) * d + p
                    let newIdx = (i * chiNew + j) * d + p

                    if oldIdx / 64 < env.UniqueEdge.Length && newIdx / 64 < grownEdge.Length then
                        let oldWordIdx = oldIdx / 64
                        let oldBitIdx = oldIdx % 64
                        let newWordIdx = newIdx / 64
                        let newBitIdx = newIdx % 64

                        let bit = (env.UniqueEdge.[oldWordIdx].Bits >>> oldBitIdx) &&& 1UL
                        if bit = 1UL then
                            grownEdge.[newWordIdx] <-
                                { Bits = grownEdge.[newWordIdx].Bits ||| (1UL <<< newBitIdx) }

        (grownCorner, grownEdge, chiNew)

    /// RG移動：環境を繰り込む
    let private rgMove (corner: BitBlock64[]) (edge: BitBlock64[]) (chi: int) (targetChi: int) (ops: IF2Operations) =

        // Renorm-F2を使用して繰り込み
        let (newCorner, newEdge, result) = RenormF2.renormalizeEnvironment corner edge chi targetChi ops

        // 圧縮率を計算
        let compressionRatio =
            if result.ActualRank > 0 then
                float result.ActualRank / float chi
            else
                0.0

        (newCorner, newEdge, result.ActualRank, compressionRatio)

    /// 環境の状態をハッシュ化（収束判定用）
    let private computeStateHash (corner: BitBlock64[]) (edge: BitBlock64[]) =
        let mutable hash = 17

        // コーナーのハッシュ
        for i in 0 .. min 10 corner.Length - 1 do
            hash <- hash * 31 + int corner.[i].Bits

        // エッジのハッシュ
        for i in 0 .. min 10 edge.Length - 1 do
            hash <- hash * 31 + int edge.[i].Bits

        hash

    /// CTMRGの1ステップ
    let ctmrgStep (env: SymmetricEnvironment) (peps: Tensor5F2) (targetChi: int) (ops: IF2Operations) =

        // CTM移動：環境を成長
        let (grownCorner, grownEdge, grownChi) = ctmMove env peps ops

        // RG移動：環境を繰り込み
        let (newCorner, newEdge, effectiveRank, compressionRatio) =
            rgMove grownCorner grownEdge grownChi targetChi ops

        // 状態ハッシュを計算
        let stateHash = computeStateHash newCorner newEdge

        // ノルムを計算（簡略化）
        let norm =
            newCorner
            |> Array.sumBy (fun b -> b.Bits)

        // 履歴を更新
        let newHistory = {
            StateHashes = Array.append env.History.StateHashes [| stateHash |]
            CompressionRatios = Array.append env.History.CompressionRatios [| compressionRatio |]
            EffectiveRanks = Array.append env.History.EffectiveRanks [| effectiveRank |]
            SpectralGaps = env.History.SpectralGaps  // 後で計算
            Norms = Array.append env.History.Norms [| norm |]
        }

        // 新しい環境を返す
        {
            UniqueCorner = newCorner
            UniqueEdge = newEdge
            Chi = targetChi
            History = newHistory
        }

    /// 収束判定
    let private hasConverged (history: ConvergenceHistory) (threshold: int) =
        if history.StateHashes.Length < threshold then
            false
        else
            // 最後のN個のハッシュが同じか、サイクルしているか
            let recent =
                history.StateHashes
                |> Array.skip (history.StateHashes.Length - threshold)

            let unique = recent |> Set.ofArray
            unique.Count <= 2  // 固定点または2サイクル

    /// 完全なCTMRGアルゴリズム
    let runCTMRG (peps: Tensor5F2) (config: CTMRGConfig) (ops: IF2Operations) =

        // 初期環境
        let initialEnv = {
            UniqueCorner = Array.init config.InitialChi (fun i -> { Bits = uint64 (i + 1) })
            UniqueEdge = Array.init (config.InitialChi * peps.D1) (fun i -> { Bits = uint64 (i % 2) })
            Chi = config.InitialChi
            History = {
                StateHashes = [||]
                CompressionRatios = [||]
                EffectiveRanks = [||]
                SpectralGaps = [||]
                Norms = [||]
            }
        }

        // 反復実行
        let rec iterate (env: SymmetricEnvironment) (iter: int) =
            if iter >= config.MaxIterations then
                (env, false)  // 最大反復回数に到達
            elif hasConverged env.History config.ConvergenceThreshold then
                (env, true)   // 収束
            else
                let newEnv = ctmrgStep env peps config.MaxChi ops
                iterate newEnv (iter + 1)

        iterate initialEnv 0

    /// 物理量の期待値を計算
    let computeExpectation (env: SymmetricEnvironment) (observable: BitBlock64[]) (ops: IF2Operations) =

        // 簡略化：環境とオブザーバブルの内積
        let mutable result = 0UL

        let minLength = min env.UniqueCorner.Length observable.Length
        for i in 0 .. minLength - 1 do
            result <- result ^^^ (env.UniqueCorner.[i].Bits &&& observable.[i].Bits)

        // ポピュレーションカウントでパリティを計算
        let parity = ops.PopCount result

        if parity % 2 = 0 then F2.Zero else F2.One