// 5-Observable/3-CorrelationFunctions.fs
namespace E8.Observable

open System
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.Ace

/// 2点相関関数
type TwoPointCorrelator() =

    interface IObservable with
        member _.Kind =
            // デフォルト距離（実際の距離は location から取得）
            TwoPointCorrelation(1, 0)

        member _.Compute environment peps location ops =
            let (x1, y1) = location.PrimaryPosition
            let chi = environment.Chi

            // 第2点を取得
            let (x2, y2) =
                match location.SecondaryPositions with
                | [] -> ((x1 + 1) % chi, y1)  // デフォルトは隣接点
                | pos :: _ -> pos

            // 距離計算（トーラス上）
            let dx = min (abs(x2 - x1)) (chi - abs(x2 - x1))
            let dy = min (abs(y2 - y1)) (chi - abs(y2 - y1))
            let distance = dx + dy  // マンハッタン距離

            // 転送行列のスペクトル情報を取得
            let spectralInfo = ObservableUtils.getSpectralInfo environment peps ops

            // プロジェクターの場合、相関は指数減衰
            let correlationStrength =
                if spectralInfo.IsProjector then
                    // 相関長1なので急速に減衰
                    if distance = 0 then
                        1.0
                    elif distance = 1 then
                        0.5
                    else
                        Math.Exp(-float distance)
                else
                    // 一般の場合
                    Math.Exp(-float distance / float spectralInfo.Rank)

            // 2点での観測量の積
            let idx1 = x1 * chi + y1
            let idx2 = x2 * chi + y2

            let value1 =
                if idx1 < environment.UniqueCorner.Length then
                    environment.UniqueCorner.[idx1].Bits
                else
                    0UL

            let value2 =
                if idx2 < environment.UniqueCorner.Length then
                    environment.UniqueCorner.[idx2].Bits
                else
                    0UL

            // 相関 = ⟨O₁O₂⟩
            let correlation = value1 &&& value2

            // 環境テンソルを通じた伝播を考慮
            let propagatedCorrelation =
                if distance > 0 then
                    // 経路に沿った積
                    let mutable pathProduct = correlation

                    // 最短経路を辿る
                    let stepX = if x2 > x1 then 1 else -1
                    let stepY = if y2 > y1 then 1 else -1

                    let mutable currentX = x1
                    let mutable currentY = y1
                    let mutable steps = 0

                    while (currentX <> x2 || currentY <> y2) && steps < chi do
                        if currentX <> x2 then
                            currentX <- (currentX + stepX + chi) % chi
                        elif currentY <> y2 then
                            currentY <- (currentY + stepY + chi) % chi

                        let idx = currentX * chi + currentY
                        if idx < environment.UniqueCorner.Length then
                            pathProduct <- pathProduct &&& environment.UniqueCorner.[idx].Bits

                        steps <- steps + 1

                    pathProduct
                else
                    correlation

            {
                Parity = if propagatedCorrelation % 2UL = 0UL then F2.Zero else F2.One
                ContributionCount = distance + 2
                Confidence = ObservableUtils.computeConfidence environment * correlationStrength
            }

        member this.ComputeBatch environment peps locations ops =
            // 距離でグループ化してバッチ処理
            let grouped =
                locations
                |> Array.groupBy (fun loc ->
                    match loc.SecondaryPositions with
                    | [] -> 0
                    | (x2, y2) :: _ ->
                        let (x1, y1) = loc.PrimaryPosition
                        abs(x2 - x1) + abs(y2 - y1))

            let results = ResizeArray<ObservableValue>()

            for (distance, locs) in grouped do
                // 同じ距離の相関を並列計算
                let values =
                    locs
                    |> Array.Parallel.map (fun loc ->
                        (this :> IObservable).Compute environment peps loc ops)

                results.AddRange(values)

            results.ToArray()

/// 連結相関関数
type ConnectedCorrelator() =

    interface IObservable with
        member _.Kind = ConnectedCorrelation(1, 0)

        member _.Compute environment peps location ops =
            let (x1, y1) = location.PrimaryPosition
            let chi = environment.Chi

            let (x2, y2) =
                match location.SecondaryPositions with
                | [] -> ((x1 + 1) % chi, y1)
                | pos :: _ -> pos

            // 通常の相関 ⟨O₁O₂⟩
            let twoPoint = TwoPointCorrelator()
            let correlation = twoPoint.Compute environment peps location ops

            // 1点平均 ⟨O₁⟩
            let loc1 = {
                PrimaryPosition = (x1, y1)
                SecondaryPositions = []
                Radius = 0
            }
            let magnetization1 = LocalMagnetization()
            let avg1 = magnetization1.Compute environment peps loc1 ops

            // 1点平均 ⟨O₂⟩
            let loc2 = {
                PrimaryPosition = (x2, y2)
                SecondaryPositions = []
                Radius = 0
            }
            let avg2 = magnetization1.Compute environment peps loc2 ops

            // 連結相関 = ⟨O₁O₂⟩ - ⟨O₁⟩⟨O₂⟩
            let connected =
                let corr = if correlation.Parity = F2.One then 1 else 0
                let a1 = if avg1.Parity = F2.One then 1 else 0
                let a2 = if avg2.Parity = F2.One then 1 else 0
                let conn = (corr - a1 * a2) % 2
                if conn = 0 then F2.Zero else F2.One

            {
                Parity = connected
                ContributionCount = correlation.ContributionCount +
                                   avg1.ContributionCount +
                                   avg2.ContributionCount
                Confidence = correlation.Confidence * 0.9  // 連結相関は信頼度が下がる
            }

        member this.ComputeBatch environment peps locations ops =
            locations
            |> Array.Parallel.map (fun loc ->
                (this :> IObservable).Compute environment peps loc ops)

/// 多点相関関数
type MultiPointCorrelator(points: (int * int) list) =

    interface IObservable with
        member _.Kind =
            // 多点相関は連鎖的な2点相関として扱う
            TwoPointCorrelation(points.Length, 0)

        member _.Compute environment peps location ops =
            let chi = environment.Chi

            // 全測定点を取得
            let allPoints =
                location.PrimaryPosition :: location.SecondaryPositions
                |> List.truncate (max 2 points.Length)

            if allPoints.Length < 2 then
                // 点が少なすぎる場合
                {
                    Parity = F2.Zero
                    ContributionCount = 0
                    Confidence = 0.0
                }
            else
                // 全点での値の積
                let mutable productParity = 1UL
                let mutable contributions = 0

                for (x, y) in allPoints do
                    let idx = x * chi + y
                    if idx < environment.UniqueCorner.Length then
                        productParity <- productParity &&& environment.UniqueCorner.[idx].Bits
                        contributions <- contributions + 1

                // 経路積分的な補正
                let pathCorrection =
                    let mutable correction = 0UL

                    for i in 0 .. allPoints.Length - 2 do
                        let (x1, y1) = allPoints.[i]
                        let (x2, y2) = allPoints.[i + 1]

                        // 隣接点間の転送
                        let transfer =
                            let dx = abs(x2 - x1)
                            let dy = abs(y2 - y1)
                            if dx + dy = 1 then
                                // 隣接
                                1UL
                            else
                                // 非隣接（減衰）
                                0UL

                        correction <- correction ^^^ transfer

                    correction

                let finalParity = productParity ^^^ pathCorrection

                {
                    Parity = if finalParity % 2UL = 0UL then F2.Zero else F2.One
                    ContributionCount = contributions
                    Confidence = ObservableUtils.computeConfidence environment /
                                float allPoints.Length
                }

        member this.ComputeBatch environment peps locations ops =
            locations
            |> Array.Parallel.map (fun loc ->
                (this :> IObservable).Compute environment peps loc ops)
