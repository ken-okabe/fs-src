// 3-TensorAlgebra.Tests.fs

namespace E8.Tests.TensorAlgebra

open Xunit
open E8.TensorAlgebra

module TensorShapeTests =

    [<Fact>]
    let ``TensorShape correctly computes total elements`` () =
        Assert.Equal(10, (Shape1D 10).TotalElements)
        Assert.Equal(20, (Shape2D(4, 5)).TotalElements)
        Assert.Equal(60, (Shape3D(3, 4, 5)).TotalElements)
        Assert.Equal(120, (Shape4D(2, 3, 4, 5)).TotalElements)
        Assert.Equal(720, (Shape5D(2, 3, 4, 5, 6)).TotalElements)
        Assert.Equal(24, (ShapeND [| 2; 3; 4 |]).TotalElements)

    [<Fact>]
    let ``TensorShape correctly reports rank`` () =
        Assert.Equal(1, (Shape1D 10).Rank)
        Assert.Equal(2, (Shape2D(4, 5)).Rank)
        Assert.Equal(3, (Shape3D(3, 4, 5)).Rank)
        Assert.Equal(4, (Shape4D(2, 3, 4, 5)).Rank)
        Assert.Equal(5, (Shape5D(2, 3, 4, 5, 6)).Rank)
        Assert.Equal(3, (ShapeND [| 2; 3; 4 |]).Rank)

    [<Fact>]
    let ``TensorShape correctly returns dimensions`` () =
        Assert.Equal([| 10 |], (Shape1D 10).Dimensions)
        Assert.Equal([| 4; 5 |], (Shape2D(4, 5)).Dimensions)
        Assert.Equal([| 3; 4; 5 |], (Shape3D(3, 4, 5)).Dimensions)
        Assert.Equal([| 2; 3; 4; 5 |], (Shape4D(2, 3, 4, 5)).Dimensions)
        Assert.Equal([| 2; 3; 4; 5; 6 |], (Shape5D(2, 3, 4, 5, 6)).Dimensions)
        Assert.Equal([| 2; 3; 4 |], (ShapeND [| 2; 3; 4 |]).Dimensions)

module IndexComputationTests =

    [<Fact>]
    let ``1D index computation with bounds checking`` () =
        Assert.Equal(0, IndexComputation.compute1D 0 10)
        Assert.Equal(5, IndexComputation.compute1D 5 10)
        Assert.Equal(9, IndexComputation.compute1D 9 10)

        Assert.Throws<IndexOutOfRangeException>(fun () ->
            IndexComputation.compute1D -1 10 |> ignore) |> ignore
        Assert.Throws<IndexOutOfRangeException>(fun () ->
            IndexComputation.compute1D 10 10 |> ignore) |> ignore

    [<Fact>]
    let ``2D index computation with row-major ordering`` () =
        Assert.Equal(0, IndexComputation.compute2D 0 0 3 4)
        Assert.Equal(1, IndexComputation.compute2D 0 1 3 4)
        Assert.Equal(4, IndexComputation.compute2D 1 0 3 4)
        Assert.Equal(5, IndexComputation.compute2D 1 1 3 4)
        Assert.Equal(11, IndexComputation.compute2D 2 3 3 4)

        Assert.Throws<IndexOutOfRangeException>(fun () ->
            IndexComputation.compute2D 3 0 3 4 |> ignore) |> ignore
        Assert.Throws<IndexOutOfRangeException>(fun () ->
            IndexComputation.compute2D 0 4 3 4 |> ignore) |> ignore

    [<Fact>]
    let ``3D index computation`` () =
        Assert.Equal(0, IndexComputation.compute3D 0 0 0 2 3 4)
        Assert.Equal(1, IndexComputation.compute3D 0 0 1 2 3 4)
        Assert.Equal(4, IndexComputation.compute3D 0 1 0 2 3 4)
        Assert.Equal(12, IndexComputation.compute3D 1 0 0 2 3 4)
        Assert.Equal(23, IndexComputation.compute3D 1 2 3 2 3 4)

        Assert.Throws<IndexOutOfRangeException>(fun () ->
            IndexComputation.compute3D 2 0 0 2 3 4 |> ignore) |> ignore

    [<Fact>]
    let ``4D index computation`` () =
        Assert.Equal(0, IndexComputation.compute4D 0 0 0 0 2 2 2 2)
        Assert.Equal(1, IndexComputation.compute4D 0 0 0 1 2 2 2 2)
        Assert.Equal(2, IndexComputation.compute4D 0 0 1 0 2 2 2 2)
        Assert.Equal(15, IndexComputation.compute4D 1 1 1 1 2 2 2 2)

        Assert.Throws<IndexOutOfRangeException>(fun () ->
            IndexComputation.compute4D 0 0 0 2 2 2 2 2 |> ignore) |> ignore

    [<Fact>]
    let ``5D index computation`` () =
        Assert.Equal(0, IndexComputation.compute5D 0 0 0 0 0 2 2 2 2 2)
        Assert.Equal(1, IndexComputation.compute5D 0 0 0 0 1 2 2 2 2 2)
        Assert.Equal(31, IndexComputation.compute5D 1 1 1 1 1 2 2 2 2 2)

        Assert.Throws<IndexOutOfRangeException>(fun () ->
            IndexComputation.compute5D 2 0 0 0 0 2 2 2 2 2 |> ignore) |> ignore

    [<Fact>]
    let ``ND index computation`` () =
        Assert.Equal(0, IndexComputation.computeND [| 0; 0; 0 |] [| 2; 3; 4 |])
        Assert.Equal(1, IndexComputation.computeND [| 0; 0; 1 |] [| 2; 3; 4 |])
        Assert.Equal(23, IndexComputation.computeND [| 1; 2; 3 |] [| 2; 3; 4 |])

        Assert.Throws<ArgumentException>(fun () ->
            IndexComputation.computeND [| 0; 0 |] [| 2; 3; 4 |] |> ignore) |> ignore
        Assert.Throws<IndexOutOfRangeException>(fun () ->
            IndexComputation.computeND [| 2; 0; 0 |] [| 2; 3; 4 |] |> ignore) |> ignore