// 4-Tensors.fs
// 具体的なテンソル実装

namespace E8.Tensors

open System
open System.Runtime.CompilerServices
open E8.Algebra
open E8.BitPacking
open E8.TensorAlgebra

/// 1階テンソル（ベクトル）
[<Sealed>]
type VectorF2(size: int) =
    inherit TensorF2Base(Shape1D size)

    let data = BitPackedArray(size)

    member _.Size = size

    member x.Item
        with get(i: int) =
            let idx = IndexComputation.compute1D i size
            if data.[idx] then F2.One else F2.Zero
        and set(i: int) (value: F2) =
            let idx = IndexComputation.compute1D i size
            data.[idx] <- value.Value = 1uy

    override _.GetValue(flatIndex: int) =
        if data.[flatIndex] then F2.One else F2.Zero

    override _.SetValue flatIndex value =
        data.[flatIndex] <- value.Value = 1uy

    override _.Clone() =
        let copy = VectorF2(size)
        Array.Copy(data.Data, copy.GetDataUnsafe().Data, data.WordCount)
        copy :> TensorF2Base

    override _.Clear() =
        data.Clear()

    override _.CountNonZero() =
        data.CountOnes()

    member private x.GetDataUnsafe() = data

    member x.Dot(other: VectorF2) =
        if x.Size <> other.Size then
            raise (ArgumentException("Vectors must have same size for dot product"))

        let mutable result = F2.Zero
        for i in 0 .. size - 1 do
            result <- result + (x.[i] * other.[i])
        result

/// 2階テンソル（行列）
[<Sealed>]
type MatrixF2(rows: int, cols: int) =
    inherit TensorF2Base(Shape2D(rows, cols))

    let wordsPerRow = BitOperations.computeWordCount cols
    let data = Array.zeroCreate<uint64>(rows * wordsPerRow)

    member _.Rows = rows
    member _.Cols = cols
    member _.WordsPerRow = wordsPerRow

    member private _.Data = data

    member x.Item
        with get(i: int, j: int) =
            let _ = IndexComputation.compute2D i j rows cols  // Bounds check
            let wordIdx = i * wordsPerRow + j / 64
            let bitIdx = j % 64
            if (data.[wordIdx] >>> bitIdx) &&& 1UL <> 0UL then F2.One else F2.Zero
        and set(i: int, j: int) (value: F2) =
            let _ = IndexComputation.compute2D i j rows cols  // Bounds check
            let wordIdx = i * wordsPerRow + j / 64
            let bitIdx = j % 64
            let mask = 1UL <<< bitIdx
            if value.Value = 1uy then
                data.[wordIdx] <- data.[wordIdx] ||| mask
            else
                data.[wordIdx] <- data.[wordIdx] &&& ~~~mask

    override x.GetValue(flatIndex: int) =
        let i = flatIndex / cols
        let j = flatIndex % cols
        x.[i, j]

    override x.SetValue flatIndex value =
        let i = flatIndex / cols
        let j = flatIndex % cols
        x.[i, j] <- value

    override x.Clone() =
        let copy = MatrixF2(rows, cols)
        Array.Copy(data, copy.Data, data.Length)
        copy :> TensorF2Base

    override _.Clear() =
        Array.Clear(data, 0, data.Length)

    override _.CountNonZero() =
        let mutable count = 0
        for word in data do
            count <- count + BitOperations.PopCount(word)
        count

    static member Identity(size: int) =
        let m = MatrixF2(size, size)
        for i in 0 .. size - 1 do
            m.[i, i] <- F2.One
        m

    member x.Transpose() =
        let t = MatrixF2(cols, rows)
        for i in 0 .. rows - 1 do
            for j in 0 .. cols - 1 do
                t.[j, i] <- x.[i, j]
        t

    static member (*) (A: MatrixF2, B: MatrixF2) =
        if A.Cols <> B.Rows then
            raise (ArgumentException("Matrix dimensions incompatible for multiplication"))

        let C = MatrixF2(A.Rows, B.Cols)
        for i in 0 .. A.Rows - 1 do
            for j in 0 .. B.Cols - 1 do
                let mutable sum = F2.Zero
                for k in 0 .. A.Cols - 1 do
                    sum <- sum + (A.[i, k] * B.[k, j])
                C.[i, j] <- sum
        C

/// 3階テンソル
[<Sealed>]
type Tensor3F2(d1: int, d2: int, d3: int) =
    inherit TensorF2Base(Shape3D(d1, d2, d3))

    let totalBits = d1 * d2 * d3
    let data = BitPackedArray(totalBits)

    member _.D1 = d1
    member _.D2 = d2
    member _.D3 = d3

    member x.Item
        with get(i: int, j: int, k: int) =
            let idx = IndexComputation.compute3D i j k d1 d2 d3
            if data.[idx] then F2.One else F2.Zero
        and set(i: int, j: int, k: int) (value: F2) =
            let idx = IndexComputation.compute3D i j k d1 d2 d3
            data.[idx] <- value.Value = 1uy

    override _.GetValue(flatIndex: int) =
        if data.[flatIndex] then F2.One else F2.Zero

    override _.SetValue flatIndex value =
        data.[flatIndex] <- value.Value = 1uy

    override _.Clone() =
        let copy = Tensor3F2(d1, d2, d3)
        Array.Copy(data.Data, copy.GetDataUnsafe().Data, data.WordCount)
        copy :> TensorF2Base

    override _.Clear() =
        data.Clear()

    override _.CountNonZero() =
        data.CountOnes()

    member private x.GetDataUnsafe() = data

/// 4階テンソル
[<Sealed>]
type Tensor4F2(d1: int, d2: int, d3: int, d4: int) =
    inherit TensorF2Base(Shape4D(d1, d2, d3, d4))

    let totalBits = d1 * d2 * d3 * d4
    let data = BitPackedArray(totalBits)

    member _.D1 = d1
    member _.D2 = d2
    member _.D3 = d3
    member _.D4 = d4

    member x.Item
        with get(i: int, j: int, k: int, l: int) =
            let idx = IndexComputation.compute4D i j k l d1 d2 d3 d4
            if data.[idx] then F2.One else F2.Zero
        and set(i: int, j: int, k: int, l: int) (value: F2) =
            let idx = IndexComputation.compute4D i j k l d1 d2 d3 d4
            data.[idx] <- value.Value = 1uy

    override _.GetValue(flatIndex: int) =
        if data.[flatIndex] then F2.One else F2.Zero

    override _.SetValue flatIndex value =
        data.[flatIndex] <- value.Value = 1uy

    override _.Clone() =
        let copy = Tensor4F2(d1, d2, d3, d4)
        Array.Copy(data.Data, copy.GetDataUnsafe().Data, data.WordCount)
        copy :> TensorF2Base

    override _.Clear() =
        data.Clear()

    override _.CountNonZero() =
        data.CountOnes()

    member private x.GetDataUnsafe() = data

/// 5階テンソル（PEPS用）
[<Sealed>]
type Tensor5F2(d1: int, d2: int, d3: int, d4: int, d5: int) =
    inherit TensorF2Base(Shape5D(d1, d2, d3, d4, d5))

    let totalBits = d1 * d2 * d3 * d4 * d5
    let data = BitPackedArray(totalBits)

    member _.D1 = d1  // Physical
    member _.D2 = d2  // Left
    member _.D3 = d3  // Right
    member _.D4 = d4  // Up
    member _.D5 = d5  // Down

    member x.Item
        with get(i: int, j: int, k: int, l: int, m: int) =
            let idx = IndexComputation.compute5D i j k l m d1 d2 d3 d4 d5
            if data.[idx] then F2.One else F2.Zero
        and set(i: int, j: int, k: int, l: int, m: int) (value: F2) =
            let idx = IndexComputation.compute5D i j k l m d1 d2 d3 d4 d5
            data.[idx] <- value.Value = 1uy

    override _.GetValue(flatIndex: int) =
        if data.[flatIndex] then F2.One else F2.Zero

    override _.SetValue flatIndex value =
        data.[flatIndex] <- value.Value = 1uy

    override _.Clone() =
        let copy = Tensor5F2(d1, d2, d3, d4, d5)
        Array.Copy(data.Data, copy.GetDataUnsafe().Data, data.WordCount)
        copy :> TensorF2Base

    override _.Clear() =
        data.Clear()

    override _.CountNonZero() =
        data.CountOnes()

    member private x.GetDataUnsafe() = data