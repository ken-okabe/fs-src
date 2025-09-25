// src/4-PEPS-Ace/4-TransferMatrix.fs
namespace E8.Ace

open System
open System.Collections.Generic
open E8.Algebra
open E8.Tensors
open E8.Hardware

/// 転送行列の高度な解析と操作
module TransferMatrix =

    /// 転送行列の構造
    type TransferMatrixStructure = {
        /// 行列データ
        Data: BitBlock64[]
        /// サイズ（正方行列と仮定）
        Size: int
        /// ブロック構造（もしあれば）
        BlockStructure: BlockDecomposition option
        /// 疎性パターン
        SparsityPattern: SparsityInfo
        /// 対称性
        Symmetries: SymmetryInfo
    }

    and BlockDecomposition = {
        /// ブロック数
        NumBlocks: int
        /// 各ブロックのサイズ
        BlockSizes: int[]
        /// ブロック間の結合
        Couplings: (int * int)[]
    }

    and SparsityInfo = {
        /// 非零要素数
        NonZeroCount: int
        /// 疎性率（0-1）
        SparsityRatio: float
        /// 行ごとの非零要素数
        RowNonZeros: int[]
        /// 列ごとの非零要素数
        ColNonZeros: int[]
    }

    and SymmetryInfo = {
        /// 転置対称性
        IsSymmetric: bool
        /// 反対称性
        IsAntiSymmetric: bool
        /// ブロック対角性
        IsBlockDiagonal: bool
        /// 巡回対称性
        IsCirculant: bool
    }

    /// 転送行列を解析
    let analyzeStructure (matrix: BitBlock64[]) (size: int) (ops: IF2Operations) =

        let wordsPerRow = (size + 63) / 64

        // 疎性情報を計算
        let mutable nonZeroCount = 0
        let rowNonZeros = Array.zeroCreate size
        let colNonZeros = Array.zeroCreate size

        for i in 0 .. size - 1 do
            for j in 0 .. size - 1 do
                let wordIdx = i * wordsPerRow + j / 64
                let bitIdx = j % 64
                if wordIdx < matrix.Length then
                    if (matrix.[wordIdx].Bits >>> bitIdx) &&& 1UL = 1UL then
                        nonZeroCount <- nonZeroCount + 1
                        rowNonZeros.[i] <- rowNonZeros.[i] + 1
                        colNonZeros.[j] <- colNonZeros.[j] + 1

        let sparsity = {
            NonZeroCount = nonZeroCount
            SparsityRatio = 1.0 - (float nonZeroCount / float (size * size))
            RowNonZeros = rowNonZeros
            ColNonZeros = colNonZeros
        }

        // 対称性をチェック
        let checkSymmetric() =
            let mutable isSymmetric = true
            for i in 0 .. size - 1 do
                for j in i + 1 .. size - 1 do
                    let ij_wordIdx = i * wordsPerRow + j / 64
                    let ij_bitIdx = j % 64
                    let ji_wordIdx = j * wordsPerRow + i / 64
                    let ji_bitIdx = i % 64

                    if ij_wordIdx < matrix.Length && ji_wordIdx < matrix.Length then
                        let ij_bit = (matrix.[ij_wordIdx].Bits >>> ij_bitIdx) &&& 1UL
                        let ji_bit = (matrix.[ji_wordIdx].Bits >>> ji_bitIdx) &&& 1UL
                        if ij_bit <> ji_bit then
                            isSymmetric <- false
            isSymmetric

        let symmetry = {
            IsSymmetric = checkSymmetric()
            IsAntiSymmetric = false  // 簡略化
            IsBlockDiagonal = false  // 簡略化
            IsCirculant = false      // 簡略化
        }

        {
            Data = matrix
            Size = size
            BlockStructure = None  // 簡略化
            SparsityPattern = sparsity
            Symmetries = symmetry
        }

    /// 転送行列のべき乗を計算
    let matrixPower (matrix: BitBlock64[]) (size: int) (power: int) (ops: IF2Operations) =

        if power = 0 then
            // 単位行列を返す
            let wordsPerRow = (size + 63) / 64
            let identity = Array.zeroCreate (size * wordsPerRow)
            for i in 0 .. size - 1 do
                let wordIdx = i * wordsPerRow + i / 64
                let bitIdx = i % 64
                identity.[wordIdx] <- { Bits = identity.[wordIdx].Bits ||| (1UL <<< bitIdx) }
            identity

        elif power = 1 then
            Array.copy matrix

        else
            // 繰り返し二乗法
            let rec powerRec (m: BitBlock64[]) (p: int) =
                if p = 1 then
                    m
                elif p % 2 = 0 then
                    let half = powerRec m (p / 2)
                    ops.MatrixMultiply half half size size size
                else
                    let m' = powerRec m (p - 1)
                    ops.MatrixMultiply m m' size size size

            powerRec matrix power

    /// 転送行列の固有空間を近似（簡略化）
    type EigenspaceInfo = {
        /// 固有値1の固有空間の次元
        NullityOfTMinusI: int
        /// 固有値0の固有空間の次元
        NullityOfT: int
        /// その他の情報
        HasUniqueFixedPoint: bool
    }

    let approximateEigenspace (matrix: BitBlock64[]) (size: int) (ops: IF2Operations) =

        // T - Iを計算
        let wordsPerRow = (size + 63) / 64
        let tMinusI = Array.copy matrix

        for i in 0 .. size - 1 do
            let wordIdx = i * wordsPerRow + i / 64
            let bitIdx = i % 64
            if wordIdx < tMinusI.Length then
                tMinusI.[wordIdx] <-
                    { Bits = tMinusI.[wordIdx].Bits ^^^ (1UL <<< bitIdx) }

        // ランクを計算
        let (rankT, _) = ops.ComputeTopologicalRank matrix size size
        let (rankTMinusI, _) = ops.ComputeTopologicalRank tMinusI size size

        {
            NullityOfTMinusI = size - rankTMinusI
            NullityOfT = size - rankT
            HasUniqueFixedPoint = (size - rankTMinusI) = 1
        }

    /// 転送行列の相関長を計算（簡略化）
    let computeCorrelationLength (matrix: BitBlock64[]) (size: int) (ops: IF2Operations) =

        // 第2固有値を推定（簡略化：プロジェクターテストを使用）
        let t2 = ops.MatrixMultiply matrix matrix size size size
        let (isProjector, _) = ops.MatrixEquals matrix t2 size

        if isProjector then
            // プロジェクターの場合、相関長は無限大（F₂では表現不可）
            None
        else
            // 簡略化：有限の相関長を仮定
            Some 10  // ダミー値

    /// 転送行列から物理量を抽出
    type PhysicalQuantities = {
        /// 磁化密度
        MagnetizationDensity: F2
        /// エネルギー密度
        EnergyDensity: F2
        /// エンタングルメントエントロピー（パリティ）
        EntanglementParity: F2
    }

    let extractPhysicalQuantities (matrix: BitBlock64[]) (size: int) (ops: IF2Operations) =

        // トレースを計算
        let wordsPerRow = (size + 63) / 64
        let mutable trace = 0UL

        for i in 0 .. size - 1 do
            let wordIdx = i * wordsPerRow + i / 64
            let bitIdx = i % 64
            if wordIdx < matrix.Length then
                if (matrix.[wordIdx].Bits >>> bitIdx) &&& 1UL = 1UL then
                    trace <- trace ^^^ 1UL

        // 非対角要素の和（簡略化）
        let mutable offDiagonal = 0UL

        for i in 0 .. min 10 (size - 1) do
            for j in 0 .. min 10 (size - 1) do
                if i <> j then
                    let wordIdx = i * wordsPerRow + j / 64
                    let bitIdx = j % 64
                    if wordIdx < matrix.Length then
                        if (matrix.[wordIdx].Bits >>> bitIdx) &&& 1UL = 1UL then
                            offDiagonal <- offDiagonal ^^^ 1UL

        {
            MagnetizationDensity = if trace = 1UL then F2.One else F2.Zero
            EnergyDensity = if offDiagonal = 1UL then F2.One else F2.Zero
            EntanglementParity = if (trace ^^^ offDiagonal) = 1UL then F2.One else F2.Zero
        }

    /// 転送行列の完全な解析
    type CompleteAnalysis = {
        Structure: TransferMatrixStructure
        Eigenspace: EigenspaceInfo
        CorrelationLength: int option
        PhysicalQuantities: PhysicalQuantities
        IsProjector: bool
        MinimalPolynomialDegree: int
    }

    let performCompleteAnalysis (matrix: BitBlock64[]) (size: int) (ops: IF2Operations) =

        // 構造解析
        let structure = analyzeStructure matrix size ops

        // 固有空間
        let eigenspace = approximateEigenspace matrix size ops

        // 相関長
        let correlationLength = computeCorrelationLength matrix size ops

        // 物理量
        let quantities = extractPhysicalQuantities matrix size ops

        // プロジェクターチェック
        let t2 = ops.MatrixMultiply matrix matrix size size size
        let (isProjector, _) = ops.MatrixEquals matrix t2 size

        // 最小多項式の次数（簡略化）
        let minPolyDegree = if isProjector then 2 else min size 10

        {
            Structure = structure
            Eigenspace = eigenspace
            CorrelationLength = correlationLength
            PhysicalQuantities = quantities
            IsProjector = isProjector
            MinimalPolynomialDegree = minPolyDegree
        }