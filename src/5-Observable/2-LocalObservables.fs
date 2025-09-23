// 5-Observable/2-LocalObservables.fs
namespace E8.Observable

open System
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.Ace

/// 局所磁化の計算
type LocalMagnetization() =

    interface IObservable with
        member _.Kind = LocalMagnetization

        member _.Compute environment peps location ops =
            let (x, y) = location.PrimaryPosition
            let radius = location.Radius
            let chi = environment.Chi

            // 局所領域のパリティを計算
            let localParity = ParityComputation.extractLocalParity
                                  environment (x, y) radius

            // PEPSテンソルとの縮約
            let mutable contributionCount = 0
            let mutable pepsContribution = 0UL

            for dx in -radius .. radius do
                for dy in -radius .. radius do
                    let px = (x + dx + peps.BondDimension) % peps.BondDimension
                    let py = (y + dy + peps.BondDimension) % peps.BondDimension

                    // 物理インデックスを0に固定（スピンアップ）
                    let pepsValue = peps.[0, px, py, px, py]
                    if pepsValue = F2.One then
                        pepsContribution <- pepsContribution ^^^ 1UL
                        contributionCount <- contributionCount + 1

            // 最終的なパリティ
            let finalParity =
                if (uint64 localParity + pepsContribution) % 2UL = 0UL then
                    F2.Zero
                else
                    F2.One

            {
                Parity = finalParity
                ContributionCount = contributionCount
                Confidence = ObservableUtils.computeConfidence environment
            }

        member this.ComputeBatch environment peps locations ops =
            // 並列バッチ処理
            locations
            |> Array.Parallel.map (fun loc ->
                (this :> IObservable).Compute environment peps loc ops)

/// 局所パリティ演算子
type LocalParityOperator() =

    interface IObservable with
        member _.Kind = LocalParity

        member _.Compute environment peps location ops =
            let (x, y) = location.PrimaryPosition
            let chi = environment.Chi

            // 単一点のパリティ
            let idx = x * chi + y
            let cornerValue =
                if idx < environment.UniqueCorner.Length then
                    environment.UniqueCorner.[idx]
                else
                    { Bits = 0UL }

            // エッジテンソルからの寄与
            let edgeContribution =
                let mutable sum = 0UL
                for d in 0 .. peps.BondDimension - 1 do
                    let edgeIdx = x * chi * peps.BondDimension + y * peps.BondDimension + d
                    if edgeIdx < environment.UniqueEdge.Length then
                        sum <- sum ^^^ environment.UniqueEdge.[edgeIdx].Bits
                sum

            // 総パリティ
            let totalParity = cornerValue.Bits ^^^ edgeContribution

            {
                Parity = if totalParity % 2UL = 0UL then F2.Zero else F2.One
                ContributionCount = 1 + peps.BondDimension
                Confidence = ObservableUtils.computeConfidence environment
            }

        member this.ComputeBatch environment peps locations ops =
            // GPUバッチ処理用のデータ準備
            let batchData =
                locations |> Array.map (fun loc ->
                    let (x, y) = loc.PrimaryPosition
                    (x, y, x * environment.Chi + y))

            // 並列計算
            let results =
                batchData
                |> Array.Parallel.map (fun (x, y, idx) ->
                    let loc = {
                        PrimaryPosition = (x, y)
                        SecondaryPositions = []
                        Radius = 0
                    }
                    (this :> IObservable).Compute environment peps loc ops)

            results

/// プラケット演算子
type PlaquetteOperator(size: int) =

    interface IObservable with
        member _.Kind = PlaquetteOperator size

        member _.Compute environment peps location ops =
            let (centerX, centerY) = location.PrimaryPosition
            let chi = environment.Chi

            // プラケット周囲のパリティを計算
            let mutable plaquetteParity = 0UL
            let mutable contributions = 0

            // 正方形プラケットの周囲を巡る
            for i in 0 .. size - 1 do
                // 上辺
                let x = (centerX + i) % chi
                let y = centerY
                let idx = x * chi + y
                if idx < environment.UniqueCorner.Length then
                    plaquetteParity <- plaquetteParity ^^^ environment.UniqueCorner.[idx].Bits
                    contributions <- contributions + 1

                // 右辺
                let x = (centerX + size) % chi
                let y = (centerY + i) % chi
                let idx = x * chi + y
                if idx < environment.UniqueCorner.Length then
                    plaquetteParity <- plaquetteParity ^^^ environment.UniqueCorner.[idx].Bits
                    contributions <- contributions + 1

                // 下辺
                let x = (centerX + size - i) % chi
                let y = (centerY + size) % chi
                let idx = x * chi + y
                if idx < environment.UniqueCorner.Length then
                    plaquetteParity <- plaquetteParity ^^^ environment.UniqueCorner.[idx].Bits
                    contributions <- contributions + 1

                // 左辺
                let x = centerX
                let y = (centerY + size - i) % chi
                let idx = x * chi + y
                if idx < environment.UniqueCorner.Length then
                    plaquetteParity <- plaquetteParity ^^^ environment.UniqueCorner.[idx].Bits
                    contributions <- contributions + 1

            {
                Parity = if plaquetteParity % 2UL = 0UL then F2.Zero else F2.One
                ContributionCount = contributions
                Confidence = ObservableUtils.computeConfidence environment
            }

        member this.ComputeBatch environment peps locations ops =
            locations
            |> Array.Parallel.map (fun loc ->
                (this :> IObservable).Compute environment peps loc ops)

/// ボルテックス密度
type VortexDensity() =

    interface IObservable with
        member _.Kind = VortexDensity

        member _.Compute environment peps location ops =
            let (x, y) = location.PrimaryPosition
            let radius = max 1 location.Radius
            let chi = environment.Chi

            // ボルテックスは4つのプラケットの交点
            let mutable vortexParity = 0UL
            let plaquettes = [
                (x, y);           // 左上
                (x + 1, y);       // 右上
                (x, y + 1);       // 左下
                (x + 1, y + 1)    // 右下
            ]

            let mutable contributions = 0

            for (px, py) in plaquettes do
                let px = (px + chi) % chi
                let py = (py + chi) % chi

                // 各プラケットのフラックス
                let plaquetteFlux =
                    let mutable flux = 0UL

                    // 2×2プラケットのエッジを巡る
                    for edge in [(0,0,1,0); (1,0,1,1); (1,1,0,1); (0,1,0,0)] do
                        let (x1, y1, x2, y2) = edge
                        let idx1 = ((px + x1) % chi) * chi + ((py + y1) % chi)
                        let idx2 = ((px + x2) % chi) * chi + ((py + y2) % chi)

                        if idx1 < environment.UniqueCorner.Length &&
                           idx2 < environment.UniqueCorner.Length then
                            let link = environment.UniqueCorner.[idx1].Bits &&&
                                      environment.UniqueCorner.[idx2].Bits
                            flux <- flux ^^^ link
                            contributions <- contributions + 1

                    flux

                vortexParity <- vortexParity ^^^ plaquetteFlux

            // ボルテックスが存在するかどうか
            let hasVortex = vortexParity % 2UL = 1UL

            {
                Parity = if hasVortex then F2.One else F2.Zero
                ContributionCount = contributions
                Confidence = ObservableUtils.computeConfidence environment
            }

        member this.ComputeBatch environment peps locations ops =
            locations
            |> Array.Parallel.map (fun loc ->
                (this :> IObservable).Compute environment peps loc ops)