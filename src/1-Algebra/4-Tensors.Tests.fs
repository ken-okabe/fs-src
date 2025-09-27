// 4-Tensors.Tests.fs

namespace E8.Tests.Tensors

open Xunit
open E8.Algebra
open E8.Tensors
open E8.TensorAlgebra

module VectorF2Tests =

    [<Fact>]
    let ``VectorF2 construction and basic operations`` () =
        let v = VectorF2(10)
        Assert.Equal(10, v.Size)
        Assert.Equal(Shape1D 10, v.Shape)

        Assert.Equal(F2.Zero, v.[0])
        Assert.Equal(F2.Zero, v.[9])

        v.[0] <- F2.One
        v.[9] <- F2.One

        Assert.Equal(F2.One, v.[0])
        Assert.Equal(F2.One, v.[9])
        Assert.Equal(F2.Zero, v.[5])

    [<Fact>]
    let ``VectorF2 bounds checking`` () =
        let v = VectorF2(5)

        Assert.Throws<System.IndexOutOfRangeException>(fun () -> v.[-1] |> ignore) |> ignore
        Assert.Throws<System.IndexOutOfRangeException>(fun () -> v.[5] |> ignore) |> ignore
        Assert.Throws<System.IndexOutOfRangeException>(fun () -> v.[-1] <- F2.One) |> ignore
        Assert.Throws<System.IndexOutOfRangeException>(fun () -> v.[5] <- F2.One) |> ignore

    [<Fact>]
    let ``VectorF2 dot product`` () =
        let v1 = VectorF2(4)
        let v2 = VectorF2(4)

        v1.[0] <- F2.One
        v1.[2] <- F2.One

        v2.[0] <- F2.One
        v2.[1] <- F2.One

        let dot = v1.Dot(v2)
        Assert.Equal(F2.One, dot)  // 1*1 + 0*1 + 1*0 + 0*0 = 1

        v2.[2] <- F2.One
        let dot2 = v1.Dot(v2)
        Assert.Equal(F2.Zero, dot2)  // 1*1 + 0*1 + 1*1 + 0*0 = 1+1 = 0 in F2

    [<Fact>]
    let ``VectorF2 dot product size mismatch throws`` () =
        let v1 = VectorF2(3)
        let v2 = VectorF2(4)

        Assert.Throws<System.ArgumentException>(fun () -> v1.Dot(v2) |> ignore) |> ignore

    [<Fact>]
    let ``VectorF2 clone is independent`` () =
        let v1 = VectorF2(5)
        v1.[1] <- F2.One
        v1.[3] <- F2.One

        let v2 = v1.Clone() :?> VectorF2

        Assert.Equal(v1.[1], v2.[1])
        Assert.Equal(v1.[3], v2.[3])

        v2.[1] <- F2.Zero
        v2.[4] <- F2.One

        Assert.Equal(F2.One, v1.[1])   // Original unchanged
        Assert.Equal(F2.Zero, v1.[4])  // Original unchanged
        Assert.Equal(F2.Zero, v2.[1])  // Copy changed
        Assert.Equal(F2.One, v2.[4])   // Copy changed

    [<Fact>]
    let ``VectorF2 clear and count non-zero`` () =
        let v = VectorF2(100)

        v.[10] <- F2.One
        v.[20] <- F2.One
        v.[30] <- F2.One

        Assert.Equal(3, v.CountNonZero())

        v.Clear()
        Assert.Equal(0, v.CountNonZero())

        for i in 0 .. 99 do
            Assert.Equal(F2.Zero, v.[i])

module MatrixF2Tests =

    [<Fact>]
    let ``MatrixF2 construction and basic operations`` () =
        let m = MatrixF2(3, 4)
        Assert.Equal(3, m.Rows)
        Assert.Equal(4, m.Cols)
        Assert.Equal(Shape2D(3, 4), m.Shape)

        Assert.Equal(F2.Zero, m.[0, 0])
        Assert.Equal(F2.Zero, m.[2, 3])

        m.[0, 0] <- F2.One
        m.[2, 3] <- F2.One

        Assert.Equal(F2.One, m.[0, 0])
        Assert.Equal(F2.One, m.[2, 3])
        Assert.Equal(F2.Zero, m.[1, 1])

    [<Fact>]
    let ``MatrixF2 identity matrix`` () =
        let m = MatrixF2.Identity(4)

        for i in 0 .. 3 do
            for j in 0 .. 3 do
                if i = j then
                    Assert.Equal(F2.One, m.[i, j])
                else
                    Assert.Equal(F2.Zero, m.[i, j])

    [<Fact>]
    let ``MatrixF2 transpose`` () =
        let m = MatrixF2(2, 3)
        m.[0, 1] <- F2.One
        m.[1, 2] <- F2.One

        let t = m.Transpose()

        Assert.Equal(3, t.Rows)
        Assert.Equal(2, t.Cols)
        Assert.Equal(F2.One, t.[1, 0])
        Assert.Equal(F2.One, t.[2, 1])
        Assert.Equal(F2.Zero, t.[0, 0])

    [<Fact>]
    let ``MatrixF2 multiplication`` () =
        let a = MatrixF2(2, 3)
        a.[0, 0] <- F2.One
        a.[0, 2] <- F2.One
        a.[1, 1] <- F2.One

        let b = MatrixF2(3, 2)
        b.[0, 0] <- F2.One
        b.[1, 1] <- F2.One
        b.[2, 0] <- F2.One

        let c = a * b

        Assert.Equal(2, c.Rows)
        Assert.Equal(2, c.Cols)
        Assert.Equal(F2.Zero, c.[0, 0])  // 1*1 + 0*0 + 1*1 = 0 in F2
        Assert.Equal(F2.Zero, c.[0, 1])  // 1*0 + 0*1 + 1*0 = 0
        Assert.Equal(F2.Zero, c.[1, 0])  // 0*1 + 1*0 + 0*1 = 0
        Assert.Equal(F2.One, c.[1, 1])   // 0*0 + 1*1 + 0*0 = 1

    [<Fact>]
    let ``MatrixF2 multiplication dimension mismatch throws`` () =
        let a = MatrixF2(2, 3)
        let b = MatrixF2(2, 3)

        Assert.Throws<System.ArgumentException>(fun () -> a * b |> ignore) |> ignore

    [<Fact>]
    let ``MatrixF2 count non-zero`` () =
        let m = MatrixF2(10, 10)

        Assert.Equal(0, m.CountNonZero())

        m.[0, 0] <- F2.One
        m.[5, 5] <- F2.One
        m.[9, 9] <- F2.One

        Assert.Equal(3, m.CountNonZero())

        m.Clear()
        Assert.Equal(0, m.CountNonZero())

module Tensor3F2Tests =

    [<Fact>]
    let ``Tensor3F2 construction and basic operations`` () =
        let t = Tensor3F2(2, 3, 4)

        Assert.Equal(2, t.D1)
        Assert.Equal(3, t.D2)
        Assert.Equal(4, t.D3)
        Assert.Equal(Shape3D(2, 3, 4), t.Shape)
        Assert.Equal(24, t.TotalElements)

        Assert.Equal(F2.Zero, t.[0, 0, 0])
        Assert.Equal(F2.Zero, t.[1, 2, 3])

        t.[0, 0, 0] <- F2.One
        t.[1, 2, 3] <- F2.One

        Assert.Equal(F2.One, t.[0, 0, 0])
        Assert.Equal(F2.One, t.[1, 2, 3])
        Assert.Equal(F2.Zero, t.[1, 1, 1])

    [<Fact>]
    let ``Tensor3F2 bounds checking`` () =
        let t = Tensor3F2(2, 3, 4)

        Assert.Throws<System.IndexOutOfRangeException>(fun () -> t.[-1, 0, 0] |> ignore) |> ignore
        Assert.Throws<System.IndexOutOfRangeException>(fun () -> t.[2, 0, 0] |> ignore) |> ignore
        Assert.Throws<System.IndexOutOfRangeException>(fun () -> t.[0, 3, 0] |> ignore) |> ignore
        Assert.Throws<System.IndexOutOfRangeException>(fun () -> t.[0, 0, 4] |> ignore) |> ignore

    [<Fact>]
    let ``Tensor3F2 clone is independent`` () =
        let t1 = Tensor3F2(2, 2, 2)
        t1.[0, 0, 0] <- F2.One
        t1.[1, 1, 1] <- F2.One

        let t2 = t1.Clone() :?> Tensor3F2

        Assert.Equal(t1.[0, 0, 0], t2.[0, 0, 0])
        Assert.Equal(t1.[1, 1, 1], t2.[1, 1, 1])

        t2.[0, 0, 0] <- F2.Zero
        t2.[0, 1, 0] <- F2.One

        Assert.Equal(F2.One, t1.[0, 0, 0])  // Original unchanged
        Assert.Equal(F2.Zero, t1.[0, 1, 0]) // Original unchanged

module Tensor4F2Tests =

    [<Fact>]
    let ``Tensor4F2 can function as a hexagonal PEPS tensor`` () =
        let t = Tensor4F2(2, 2, 2, 2) // Physical, Virtual1, Virtual2, Virtual3

        Assert.Equal(2, t.D1) // Physical dimension
        Assert.Equal(2, t.D2) // Virtual dimension 1
        Assert.Equal(2, t.D3) // Virtual dimension 2
        Assert.Equal(2, t.D4) // Virtual dimension 3
        Assert.Equal(16, t.TotalElements)

        t.[1, 0, 1, 0] <- F2.One
        Assert.Equal(F2.One, t.[1, 0, 1, 0])
        Assert.Equal(F2.Zero, t.[0, 0, 0, 0])

module Tensor5F2Tests =

    [<Fact>]
    let ``Tensor5F2 PEPS tensor construction`` () =
        let t = Tensor5F2(2, 2, 2, 2, 2)  // Physical, Left, Right, Up, Down

        Assert.Equal(2, t.D1)
        Assert.Equal(2, t.D2)
        Assert.Equal(2, t.D3)
        Assert.Equal(2, t.D4)
        Assert.Equal(2, t.D5)
        Assert.Equal(32, t.TotalElements)

    [<Fact>]
    let ``Tensor5F2 element access`` () =
        let t = Tensor5F2(2, 2, 2, 2, 2)

        t.[0, 0, 0, 0, 0] <- F2.One
        t.[1, 1, 1, 1, 1] <- F2.One

        Assert.Equal(F2.One, t.[0, 0, 0, 0, 0])
        Assert.Equal(F2.One, t.[1, 1, 1, 1, 1])
        Assert.Equal(F2.Zero, t.[0, 1, 0, 1, 0])

        Assert.Equal(2, t.CountNonZero())

    [<Fact>]
    let ``Tensor5F2 memory efficiency`` () =
        let dims = 4
        let t = Tensor5F2(dims, dims, dims, dims, dims)
        let totalElements = dims * dims * dims * dims * dims  // 1024

        Assert.Equal(totalElements, t.TotalElements)

        // Set some elements
        for i in 0 .. 10 do
            t.[i % dims, (i*2) % dims, (i*3) % dims, (i*5) % dims, (i*7) % dims] <- F2.One

        let nonZeroCount = t.CountNonZero()
        Assert.True(nonZeroCount > 0 && nonZeroCount <= 11)