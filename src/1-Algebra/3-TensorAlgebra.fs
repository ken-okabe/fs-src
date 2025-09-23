// 3-TensorAlgebra.fs
// テンソル代数の抽象層

namespace E8.TensorAlgebra

open E8.Algebra
open E8.BitPacking

/// テンソルの形状
type TensorShape =
    | Shape1D of int
    | Shape2D of int * int
    | Shape3D of int * int * int
    | Shape4D of int * int * int * int
    | Shape5D of int * int * int * int * int
    | ShapeND of int[]

    member x.TotalElements =
        match x with
        | Shape1D d -> d
        | Shape2D (d1, d2) -> d1 * d2
        | Shape3D (d1, d2, d3) -> d1 * d2 * d3
        | Shape4D (d1, d2, d3, d4) -> d1 * d2 * d3 * d4
        | Shape5D (d1, d2, d3, d4, d5) -> d1 * d2 * d3 * d4 * d5
        | ShapeND dims -> Array.fold (*) 1 dims

    member x.Rank =
        match x with
        | Shape1D _ -> 1
        | Shape2D _ -> 2
        | Shape3D _ -> 3
        | Shape4D _ -> 4
        | Shape5D _ -> 5
        | ShapeND dims -> dims.Length

    member x.Dimensions =
        match x with
        | Shape1D d -> [| d |]
        | Shape2D (d1, d2) -> [| d1; d2 |]
        | Shape3D (d1, d2, d3) -> [| d1; d2; d3 |]
        | Shape4D (d1, d2, d3, d4) -> [| d1; d2; d3; d4 |]
        | Shape5D (d1, d2, d3, d4, d5) -> [| d1; d2; d3; d4; d5 |]
        | ShapeND dims -> dims

/// テンソルインデックス演算
module IndexComputation =

    let inline compute1D (i: int) (d1: int) =
        if i < 0 || i >= d1 then
            raise (IndexOutOfRangeException($"Index ({i}) out of bounds for shape ({d1})"))
        i

    let inline compute2D (i: int) (j: int) (d1: int) (d2: int) =
        if i < 0 || i >= d1 || j < 0 || j >= d2 then
            raise (IndexOutOfRangeException($"Index ({i},{j}) out of bounds for shape ({d1},{d2})"))
        i * d2 + j

    let inline compute3D (i: int) (j: int) (k: int) (d1: int) (d2: int) (d3: int) =
        if i < 0 || i >= d1 || j < 0 || j >= d2 || k < 0 || k >= d3 then
            raise (IndexOutOfRangeException($"Index ({i},{j},{k}) out of bounds for shape ({d1},{d2},{d3})"))
        (i * d2 + j) * d3 + k

    let inline compute4D (i: int) (j: int) (k: int) (l: int)
                         (d1: int) (d2: int) (d3: int) (d4: int) =
        if i < 0 || i >= d1 || j < 0 || j >= d2 ||
           k < 0 || k >= d3 || l < 0 || l >= d4 then
            raise (IndexOutOfRangeException($"Index ({i},{j},{k},{l}) out of bounds"))
        ((i * d2 + j) * d3 + k) * d4 + l

    let inline compute5D (i: int) (j: int) (k: int) (l: int) (m: int)
                         (d1: int) (d2: int) (d3: int) (d4: int) (d5: int) =
        if i < 0 || i >= d1 || j < 0 || j >= d2 || k < 0 || k >= d3 ||
           l < 0 || l >= d4 || m < 0 || m >= d5 then
            raise (IndexOutOfRangeException($"Index ({i},{j},{k},{l},{m}) out of bounds"))
        (((i * d2 + j) * d3 + k) * d4 + l) * d5 + m

    let computeND (indices: int[]) (dimensions: int[]) =
        if indices.Length <> dimensions.Length then
            raise (ArgumentException("Index and dimension arrays must have same length"))

        let mutable flatIndex = 0
        for i in 0 .. indices.Length - 1 do
            if indices.[i] < 0 || indices.[i] >= dimensions.[i] then
                raise (IndexOutOfRangeException($"Index {i} = {indices.[i]} out of bounds"))
            flatIndex <- flatIndex * dimensions.[i] + indices.[i]
        flatIndex

/// 抽象テンソル基底クラス
[<AbstractClass>]
type TensorF2Base(shape: TensorShape) =
    member _.Shape = shape
    member _.TotalElements = shape.TotalElements
    member _.Rank = shape.Rank
    abstract member GetValue: int -> F2
    abstract member SetValue: int -> F2 -> unit
    abstract member Clone: unit -> TensorF2Base
    abstract member Clear: unit -> unit
    abstract member CountNonZero: unit -> int
