// 5-Observable/4-TopologicalOrder.fs
namespace E8.Observable

open System
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.Ace

/// Wilson Loop演算子
type WilsonLoop() =

    interface IObservable with
        member _.Kind = WilsonLoop []

        member _.Compute environment peps location ops =
            let chi = environment.Chi

            // 輪郭を取得（矩形または指定された経路）
            let contour =
                if location.SecondaryPositions.IsEmpty then
                    // デフォルトは2×2の正方形
                    let (x, y) = location.PrimaryPosition
                    [
                        (x, y); (x + 1, y); (x + 2, y);
                        (x + 2, y + 1); (x + 2, y + 2);
                        (x + 1, y + 2); (x, y + 2);
                        (x, y + 1); (x, y)  // 閉曲線
                    ]
                else
                    location.PrimaryPosition :: location.SecondaryPositions

            // Wilson Loopの値を計算（経路順序積）
            let mutable loopProduct = 1UL
            let mutable contributions = 0

            for i in 0 .. contour.Length - 2 do
                let (x1, y1) = contour.[i]
                let (x2, y2) = contour.[i + 1]

                // リンク変数（エッジ上の値）
                let linkValue =
                    // 水平リンク
                    if y1 = y2 && abs(x2 - x1) = 1 then
                        let x = min x1 x2
                        let edgeIdx = x * chi * peps.BondDimension +
                                     y1 * peps.BondDimension
                        if edgeIdx < environment.UniqueEdge.Length then
                            environment.UniqueEdge.[edgeIdx].Bits
                        else
                            1UL
                    // 垂直リンク
                    elif x1 = x2 && abs(y2 - y1) = 1 then
                        let y = min y1 y2
                        let edgeIdx = x1 * chi * peps.BondDimension +
                                     y * peps.BondDimension + 1
                        if edgeIdx < environment.UniqueEdge.Length then
                            environment.UniqueEdge.[edgeIdx].Bits
                        else
                            1UL
                    else
                        // 非隣接点
                        1UL

                loopProduct <- loopProduct &&& linkValue
                contributions <- contributions + 1

            // プラケット項（面の寄与）
            let areaContribution =
                if contour.Length >= 4 then
                    // 輪郭内の面積を計算（Shoelaceの公式）
                    let mutable area = 0
                    for i in 0 .. contour.Length - 2 do
                        let (x1, y1) = contour.[i]
                        let (x2, y2) = contour.[i + 1]
                        area <- area + (x1 * y2 - x2 * y1)

                    area <- abs(area) / 2

                    // 面積に比例したフラックス
                    if area % 2 = 0 then 0UL else 1UL
                else
                    0UL

            let wilsonValue = loopProduct ^^^ areaContribution

            {
                Parity = if wilsonValue % 2UL = 0UL then F2.Zero else F2.One
                ContributionCount = contributions
                Confidence = ObservableUtils.computeConfidence environment
            }

        member this.ComputeBatch environment peps locations ops =
            // サイズ別にグループ化
            let grouped =
                locations
                |> Array.groupBy (fun loc ->
                    loc.SecondaryPositions.Length)

            let results = ResizeArray<ObservableValue>()

            for (size, locs) in grouped do
                let values =
                    locs
                    |> Array.Parallel.map (fun loc ->
                        (this :> IObservable).Compute environment peps loc ops)
                results.AddRange(values)

            results.ToArray()

/// String Order Parameter
type StringOrderParameter() =

    interface IObservable with
        member _.Kind = StringOrder []

        member _.Compute environment peps location ops =
            let chi = environment.Chi

            // String経路を取得
            let stringPath =
                if location.SecondaryPositions.IsEmpty then
                    // デフォルトは水平線
                    let (x, y) = location.PrimaryPosition
                    [ for i in 0 .. min 4 (chi - 1) -> (x + i, y) ]
                else
                    location.PrimaryPosition :: location.SecondaryPositions

            // String演算子の計算
            let mutable stringProduct = 1UL
            let mutable contributions = 0

            // 端点での演算子
            if stringPath.Length >= 2 then
                let (x1, y1) = stringPath.Head
                let (x2, y2) = stringPath.[stringPath.Length - 1]

                // 始点の寄与
                let idx1 = x1 * chi + y1
                if idx1 < environment.UniqueCorner.Length then
                    stringProduct <- stringProduct &&& environment.UniqueCorner.[idx1].Bits
                    contributions <- contributions + 1

                // String部分（経路上のZ演算子の積）
                for i in 1 .. stringPath.Length - 2 do
                    let (x, y) = stringPath.[i]
                    let idx = x * chi + y

                    if idx < environment.UniqueCorner.Length then
                        // Z演算子はパリティを反転
                        let zOperator = environment.UniqueCorner.[idx].Bits ^^^ 1UL
                        stringProduct <- stringProduct &&& zOperator
                        contributions <- contributions + 1

                // 終点の寄与
                let idx2 = x2 * chi + y2
                if idx2 < environment.UniqueCorner.Length then
                    stringProduct <- stringProduct &&& environment.UniqueCorner.[idx2].Bits
                    contributions <- contributions + 1

            // 非局所的な秩序の検出
            let stringOrder =
                if stringPath.Length >= 3 then
                    // 長距離相関の存在をチェック
                    let longRangeOrder = stringProduct <> 0UL
                    if longRangeOrder then F2.One else F2.Zero
                else
                    F2.Zero

            {
                Parity = stringOrder
                ContributionCount = contributions
                Confidence = ObservableUtils.computeConfidence environment *
                           (1.0 / float stringPath.Length)
            }

        member this.ComputeBatch environment peps locations ops =
            locations
            |> Array.Parallel.map (fun loc ->
                (this :> IObservable).Compute environment peps loc ops)

/// トポロジカルエントロピー
type TopologicalEntropyMeasurement() =

    interface IObservable with
        member _.Kind = TopologicalEntropy

        member _.Compute environment peps location ops =
            let chi = environment.Chi
            let (x, y) = location.PrimaryPosition
            let radius = max 1 location.Radius

            // 領域Aを定義（円形領域）
            let regionA = ResizeArray<(int * int)>()
            for dx in -radius .. radius do
                for dy in -radius .. radius do
                    if dx * dx + dy * dy <= radius * radius then
                        regionA.Add(((x + dx + chi) % chi, (y + dy + chi) % chi))

            // 領域Aの縮小密度行列を構築
            let regionSize = regionA.Count
            let densityMatrix = Array2D.zeroCreate<BitBlock64> regionSize regionSize

            for i in 0 .. regionSize - 1 do
                for j in 0 .. regionSize - 1 do
                    let (xi, yi) = regionA.[i]
                    let (xj, yj) = regionA.[j]

                    let idxi = xi * chi + yi
                    let idxj = xj * chi + yj

                    if idxi < environment.UniqueCorner.Length &&
                       idxj < environment.UniqueCorner.Length then
                        // 密度行列要素 ρᵢⱼ
                        densityMatrix.[i, j] <- {
                            Bits = environment.UniqueCorner.[idxi].Bits &&&
                                  environment.UniqueCorner.[idxj].Bits
                        }

            // エンタングルメントスペクトルの計算（F₂版）
            // プロジェクターの固有値から推定
            let flatMatrix =
                densityMatrix
                |> Array2D.toArray
                |> Array.concat

            let (rank, _) = ops.ComputeTopologicalRank flatMatrix regionSize regionSize

            // トポロジカルエントロピー S_topo
            // フォン・ノイマンエントロピーのF₂版
            let stopo =
                if rank > 0 && rank < regionSize then
                    // S = -γ（トポロジカル補正項）
                    // フィボナッチエニオンの場合、γ = log(黄金比)
                    let goldenRatio = (1.0 + Math.Sqrt(5.0)) / 2.0
                    Math.Log(goldenRatio)
                else
                    0.0

            // F₂へのマッピング（閾値処理）
            let topoEntropy =
                if stopo > 0.5 then F2.One else F2.Zero

            {
                Parity = topoEntropy
                ContributionCount = regionSize * regionSize
                Confidence = ObservableUtils.computeConfidence environment *
                           (float rank / float regionSize)
            }

        member this.ComputeBatch environment peps locations ops =
            // 領域サイズでグループ化
            let grouped =
                locations
                |> Array.groupBy (fun loc -> loc.Radius)

            let results = ResizeArray<ObservableValue>()

            for (radius, locs) in grouped do
                let values =
                    locs
                    |> Array.Parallel.map (fun loc ->
                        (this :> IObservable).Compute environment peps loc ops)
                results.AddRange(values)

            results.ToArray()

/// カイラル秩序パラメータ
type ChiralOrderParameter() =

    interface IObservable with
        member _.Kind = ChiralOrder

        member _.Compute environment peps location ops =
            let chi = environment.Chi
            let (x, y) = location.PrimaryPosition

            // 3点でのカイラリティ（時計回り/反時計回り）
            let triangle = [
                (x, y)
                ((x + 1) % chi, y)
                (x, (y + 1) % chi)
                (x, y)  // 閉じる
            ]

            // 向き付けられた面積
            let mutable orientedArea = 0
            for i in 0 .. triangle.Length - 2 do
                let (x1, y1) = triangle.[i]
                let (x2, y2) = triangle.[i + 1]
                orientedArea <- orientedArea + (x1 * y2 - x2 * y1)

            // カイラリティの計算
            let mutable chiralProduct = 1UL
            let mutable contributions = 0

            for (px, py) in triangle do
                let idx = px * chi + py
                if idx < environment.UniqueCorner.Length then
                    chiralProduct <- chiralProduct &&& environment.UniqueCorner.[idx].Bits
                    contributions <- contributions + 1

            // 符号付きカイラリティ
            let chirality =
                if orientedArea > 0 then
                    // 反時計回り
                    chiralProduct
                else
                    // 時計回り（符号反転）
                    chiralProduct ^^^ 1UL

            {
                Parity = if chirality % 2UL = 0UL then F2.Zero else F2.One
                ContributionCount = contributions
                Confidence = ObservableUtils.computeConfidence environment
            }

        member this.ComputeBatch environment peps locations ops =
            locations
            |> Array.Parallel.map (fun loc ->
                (this :> IObservable).Compute environment peps loc ops)
