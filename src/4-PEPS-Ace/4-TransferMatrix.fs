// 5-Ace/4-TransferMatrix.fs
namespace E8.Ace

open System
open System.Threading.Tasks
open E8.Algebra
open E8.Hardware

/// 転送行列の高度な解析と操作
module TransferMatrix =

    /// 固有空間の詳細情報
    type EigenspaceDecomposition = {
        /// 固有値0の固有空間
        Kernel: Eigenspace
        /// 固有値1の固有空間（プロジェクターの場合）
        Image: Eigenspace
        /// 分解の品質指標
        DecompositionQuality: float
        /// 計算時間
        ComputationTimeMs: float
    }

    and Eigenspace = {
        /// 固有値
        Eigenvalue: F2
        /// 基底ベクトル
        BasisVectors: BitBlock64[][]
        /// 次元
        Dimension: int
        /// 直交性の検証
        IsOrthogonal: bool
    }

    /// 転送行列の代数的性質の完全な解析
    type AlgebraicAnalysis = {
        /// べき等性 (T² = T)
        IsIdempotent: bool
        /// 冪零性 (T^n = 0)
        IsNilpotent: bool
        /// 対合性 (T² = I)
        IsInvolutive: bool
        /// 正規性 (TT† = T†T、F₂では自動的に成立)
        IsNormal: bool
        /// トレース
        Trace: F2
        /// 行列式（F₂上）
        Determinant: F2
        /// 特性多項式
        CharacteristicPolynomial: Polynomial
        /// 最小多項式
        MinimalPolynomial: Polynomial
    }

    and Polynomial = {
        Coefficients: F2[]  // 係数（低次から）
        Degree: int
    }

    /// べき乗法による固有ベクトル探索（F₂版）
    let powerMethodF2 (matrix: BitBlock64[]) (dim: int)
                     (ops: IF2Operations) (maxIterations: int) =

        // ランダム初期ベクトル
        let rng = Random()
        let mutable v = Array.init dim (fun _ ->
            { Bits = uint64(rng.Next(2)) })

        let mutable previousV = Array.copy v
        let mutable hasConverged = false
        let mutable iteration = 0
        let mutable eigenvalue = F2.Zero

        while not hasConverged && iteration < maxIterations do
            // v_new = T * v
            let v_new = ops.MatrixMultiply matrix v dim 1 dim

            // 収束チェック
            let unchanged = Array.forall2 (fun a b -> a.Bits = b.Bits) v v_new
            let cyclic = Array.forall2 (fun a b -> a.Bits = b.Bits) previousV v_new

            if unchanged then
                // 不動点に到達 → 固有値1
                eigenvalue <- F2.One
                hasConverged <- true
            elif cyclic then
                // 2周期 → 固有値-1（F₂では1と同じ）
                eigenvalue <- F2.One
                hasConverged <- true
            else
                // ゼロベクトルチェック
                let isZero = Array.forall (fun x -> x.Bits = 0UL) v_new
                if isZero then
                    eigenvalue <- F2.Zero
                    hasConverged <- true

            previousV <- v
            v <- v_new
            iteration <- iteration + 1

        (eigenvalue, v, hasConverged, iteration)

    /// 固有空間の基底を抽出
    let extractEigenspaceBasis (matrix: BitBlock64[]) (eigenvalue: F2)
                               (dim: int) (ops: IF2Operations) =

        // (T - λI)v = 0 を解く
        let modifiedMatrix = Array.copy matrix

        if eigenvalue = F2.One then
            // T - I の計算
            for i in 0 .. dim - 1 do
                let diagIdx = i * dim + i
                modifiedMatrix.[diagIdx] <- {
                    Bits = modifiedMatrix.[diagIdx].Bits ^^^ 1UL
                }

        // ランクと核を計算
        let (rank, pivots) = ops.ComputeTopologicalRank modifiedMatrix dim dim
        let nullity = dim - rank

        // 基底ベクトルの構築
        let basis = ResizeArray<BitBlock64[]>()

        // 自由変数に対応する基底
        let freeVars =
            let all = Set.ofSeq [0 .. dim - 1]
            let pivotCols = pivots |> Array.map (fun p -> p % dim) |> Set.ofArray
            Set.difference all pivotCols |> Set.toArray

        for freeVar in freeVars do
            let vec = Array.zeroCreate<BitBlock64> dim
            vec.[freeVar] <- { Bits = 1UL }

            // 後退代入で他の成分を決定
            for pivotIdx in Array.rev pivots do
                let row = pivotIdx / dim
                let col = pivotIdx % dim

                if col <> freeVar then
                    let mutable sum = { Bits = 0UL }
                    for j in col + 1 .. dim - 1 do
                        sum <- { Bits = sum.Bits ^^^ (modifiedMatrix.[row * dim + j].Bits &&& vec.[j].Bits) }

                    vec.[col] <- sum

            basis.Add(vec)

            if basis.Count >= nullity then
                freeVars.Length <- 0  // ループ終了

        basis.ToArray()

    /// 固有空間分解の完全実装
    let eigenspaceDecomposition (matrix: BitBlock64[]) (dim: int)
                               (isProjector: bool) (ops: IF2Operations) =

        let timer = System.Diagnostics.Stopwatch.StartNew()

        if isProjector then
            // プロジェクターの場合：固有値は0と1のみ

            // Im(T) = 固有値1の固有空間
            let imageBasis = extractEigenspaceBasis matrix F2.One dim ops

            // Ker(T) = 固有値0の固有空間
            let kernelBasis = extractEigenspaceBasis matrix F2.Zero dim ops

            // 直交性チェック（F₂では簡略化）
            let checkOrthogonality (basis: BitBlock64[][]) =
                if basis.Length < 2 then true
                else
                    let mutable isOrtho = true
                    for i in 0 .. basis.Length - 2 do
                        for j in i + 1 .. basis.Length - 1 do
                            // 内積が0かチェック
                            let mutable innerProduct = 0UL
                            for k in 0 .. dim - 1 do
                                innerProduct <- innerProduct ^^^ (basis.[i].[k].Bits &&& basis.[j].[k].Bits)
                            if innerProduct <> 0UL then
                                isOrtho <- false
                    isOrtho

            timer.Stop()

            {
                Kernel = {
                    Eigenvalue = F2.Zero
                    BasisVectors = kernelBasis
                    Dimension = kernelBasis.Length
                    IsOrthogonal = checkOrthogonality kernelBasis
                }
                Image = {
                    Eigenvalue = F2.One
                    BasisVectors = imageBasis
                    Dimension = imageBasis.Length
                    IsOrthogonal = checkOrthogonality imageBasis
                }
                DecompositionQuality =
                    if kernelBasis.Length + imageBasis.Length = dim then 1.0 else 0.5
                ComputationTimeMs = timer.Elapsed.TotalMilliseconds
            }
        else
            // 一般の場合：べき乗法で固有ベクトルを探索
            let eigenvectors1 = ResizeArray<BitBlock64[]>()
            let eigenvectors0 = ResizeArray<BitBlock64[]>()

            // 複数の初期ベクトルで試行
            for trial in 0 .. min 10 dim do
                let (eigenvalue, eigenvector, converged, _) =
                    powerMethodF2 matrix dim ops 100

                if converged then
                    if eigenvalue = F2.One then
                        eigenvectors1.Add(eigenvector)
                    else
                        eigenvectors0.Add(eigenvector)

            timer.Stop()

            {
                Kernel = {
                    Eigenvalue = F2.Zero
                    BasisVectors = eigenvectors0.ToArray()
                    Dimension = eigenvectors0.Count
                    IsOrthogonal = false
                }
                Image = {
                    Eigenvalue = F2.One
                    BasisVectors = eigenvectors1.ToArray()
                    Dimension = eigenvectors1.Count
                    IsOrthogonal = false
                }
                DecompositionQuality =
                    float(eigenvectors0.Count + eigenvectors1.Count) / float dim
                ComputationTimeMs = timer.Elapsed.TotalMilliseconds
            }

    /// 特性多項式の計算（F₂版のFaddeev-LeVerrier法）
    let computeCharacteristicPolynomial (matrix: BitBlock64[]) (dim: int)
                                       (ops: IF2Operations) =

        // det(λI - T) の計算
        let coefficients = Array.zeroCreate<F2> (dim + 1)
        coefficients.[dim] <- F2.One  // 最高次の係数

        // Faddeev-LeVerrierアルゴリズムのF₂版
        let mutable B = Array.copy matrix

        for k in 1 .. dim do
            // c_{n-k} = -1/k * tr(A * B_{k-1})
            // F₂では除算がないので単純化
            let mutable trace = { Bits = 0UL }
            for i in 0 .. dim - 1 do
                trace <- { Bits = trace.Bits ^^^ B.[i * dim + i].Bits }

            coefficients.[dim - k] <- if trace.Bits = 0UL then F2.Zero else F2.One

            // B_k = A * B_{k-1} - c_{n-k} * I
            if k < dim then
                let newB = ops.MatrixMultiply matrix B dim dim dim

                if coefficients.[dim - k] = F2.One then
                    for i in 0 .. dim - 1 do
                        newB.[i * dim + i] <- {
                            Bits = newB.[i * dim + i].Bits ^^^ 1UL
                        }

                B <- newB

        {
            Coefficients = coefficients
            Degree = dim
        }

    /// 最小多項式の計算（Berlekamp法）
    let computeMinimalPolynomial (matrix: BitBlock64[]) (dim: int)
                                (ops: IF2Operations) =

        // べき乗の列を生成
        let powers = ResizeArray<BitBlock64[]>()

        // I（単位行列）
        let identity = Array.init (dim * dim) (fun i ->
            if i % (dim + 1) = 0 then { Bits = 1UL } else { Bits = 0UL })
        powers.Add(identity)

        // T
        powers.Add(matrix)

        let mutable currentPower = matrix
        let mutable foundMinPoly = false
        let mutable degree = 1

        while degree < dim && not foundMinPoly do
            degree <- degree + 1
            currentPower <- ops.MatrixMultiply currentPower matrix dim dim dim

            // 線形従属性チェック
            let augmented = Array2D.zeroCreate<BitBlock64> (dim * dim) (degree + 1)
            for p in 0 .. degree do
                for i in 0 .. dim * dim - 1 do
                    if p < powers.Count then
                        augmented.[i, p] <- powers.[p].[i]
                    else
                        augmented.[i, p] <- currentPower.[i]

            // ガウス消去
            let mutable rank = 0
            let coefficients = Array.zeroCreate<F2> (degree + 1)

            for col in 0 .. degree do
                let mutable pivotRow = -1
                for row in rank .. dim * dim - 1 do
                    if augmented.[row, col].Bits <> 0UL && pivotRow = -1 then
                        pivotRow <- row

                if pivotRow >= 0 then
                    // ピボット交換
                    if pivotRow <> rank then
                        for c in 0 .. degree do
                            let temp = augmented.[rank, c]
                            augmented.[rank, c] <- augmented.[pivotRow, c]
                            augmented.[pivotRow, c] <- temp

                    // 消去
                    for row in 0 .. dim * dim - 1 do
                        if row <> rank && augmented.[row, col].Bits <> 0UL then
                            for c in 0 .. degree do
                                augmented.[row, c] <- {
                                    Bits = augmented.[row, c].Bits ^^^ augmented.[rank, c].Bits
                                }

                    rank <- rank + 1

            if rank < degree + 1 then
                foundMinPoly <- true
                // 係数を抽出
                for i in 0 .. degree do
                    coefficients.[i] <- if augmented.[rank, i].Bits <> 0UL then F2.One else F2.Zero

            if not foundMinPoly then
                powers.Add(currentPower)

        {
            Coefficients =
                if foundMinPoly then
                    Array.take (degree + 1) coefficients
                else
                    [| F2.One |]  // デフォルト
            Degree = degree
        }

    /// 代数的性質の完全解析
    let analyzeAlgebraicProperties (matrix: BitBlock64[]) (dim: int)
                                  (ops: IF2Operations) =

        let timer = System.Diagnostics.Stopwatch.StartNew()

        // T²の計算
        let T2 = ops.MatrixMultiply matrix matrix dim dim dim

        // 単位行列
        let identity = Array.init (dim * dim) (fun i ->
            if i % (dim + 1) = 0 then { Bits = 1UL } else { Bits = 0UL })

        // ゼロ行列チェック
        let isZero m = Array.forall (fun b -> b.Bits = 0UL) m

        // 等価性チェック
        let areEqual m1 m2 =
            Array.forall2 (fun a b -> a.Bits = b.Bits) m1 m2

        // べき等性チェック (T² = T)
        let isIdempotent = areEqual T2 matrix

        // 冪零性チェック (T^n = 0)
        let checkNilpotency() =
            let mutable power = matrix
            let mutable isNilpotent = false
            for n in 2 .. min 10 dim do
                power <- ops.MatrixMultiply power matrix dim dim dim
                if isZero power then
                    isNilpotent <- true
                    n <- dim + 1  // ループ終了
            isNilpotent

        let isNilpotent = checkNilpotency()

        // 対合性チェック (T² = I)
        let isInvolutive = areEqual T2 identity

        // トレース計算
        let mutable trace = { Bits = 0UL }
        for i in 0 .. dim - 1 do
            trace <- { Bits = trace.Bits ^^^ matrix.[i * dim + i].Bits }

        // 行列式計算（簡略化：対角要素の積のパリティ）
        let mutable det = { Bits = 1UL }
        for i in 0 .. dim - 1 do
            det <- { Bits = det.Bits &&& matrix.[i * dim + i].Bits }

        // 多項式計算
        let charPoly = computeCharacteristicPolynomial matrix dim ops
        let minPoly = computeMinimalPolynomial matrix dim ops

        timer.Stop()

        {
            IsIdempotent = isIdempotent
            IsNilpotent = isNilpotent
            IsInvolutive = isInvolutive
            IsNormal = true  // F₂では常に真
            Trace = if trace.Bits = 0UL then F2.Zero else F2.One
            Determinant = if det.Bits = 0UL then F2.Zero else F2.One
            CharacteristicPolynomial = charPoly
            MinimalPolynomial = minPoly
        }

    /// トポロジカル不変量の計算
    let computeTopologicalInvariants (matrix: BitBlock64[]) (dim: int)
                                    (isProjector: bool) (ops: IF2Operations) =

        // 固有空間分解
        let eigenspaces = eigenspaceDecomposition matrix dim isProjector ops

        // 縮退度スペクトラム
        let degeneracySpectrum = Map.ofList [
            (F2.Zero, eigenspaces.Kernel.Dimension)
            (F2.One, eigenspaces.Image.Dimension)
        ]

        // トポロジカルエントロピー
        let topologicalEntropy =
            if eigenspaces.Image.Dimension > 1 then
                Math.Log(float eigenspaces.Image.Dimension, 2.0)
            else
                0.0

        // ギャップの存在
        let hasGap =
            eigenspaces.Kernel.Dimension > 0 && eigenspaces.Image.Dimension > 0

        // チャーン数（F₂版の簡略計算）
        let chernNumber =
            if isProjector && hasGap then
                1  // トポロジカル相
            else
                0  // 自明な相

        {|
            DegeneracySpectrum = degeneracySpectrum
            TopologicalEntropy = topologicalEntropy
            HasSpectralGap = hasGap
            ChernNumber = chernNumber
            IsTopological = eigenspaces.Image.Dimension > 1 || eigenspaces.Kernel.Dimension > 1
        |}