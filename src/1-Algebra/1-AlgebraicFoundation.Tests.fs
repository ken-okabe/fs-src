// 1-AlgebraicFoundation.Tests.fs

namespace E8.Tests.Algebra

open Xunit
open E8.Algebra

module F2Tests =

    [<Fact>]
    let ``F2 constructor accepts only 0 and 1`` () =
        let zero = F2(0uy)
        let one = F2(1uy)
        Assert.Equal(0uy, zero.Value)
        Assert.Equal(1uy, one.Value)

        Assert.Throws<ArgumentException>(fun () -> F2(2uy) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun () -> F2(255uy) |> ignore) |> ignore

    [<Fact>]
    let ``F2 addition follows XOR semantics`` () =
        let zero = F2.Zero
        let one = F2.One

        Assert.Equal(zero, zero + zero)
        Assert.Equal(one, zero + one)
        Assert.Equal(one, one + zero)
        Assert.Equal(zero, one + one)  // Critical: 1 + 1 = 0 in F2

    [<Fact>]
    let ``F2 multiplication follows AND semantics`` () =
        let zero = F2.Zero
        let one = F2.One

        Assert.Equal(zero, zero * zero)
        Assert.Equal(zero, zero * one)
        Assert.Equal(zero, one * zero)
        Assert.Equal(one, one * one)

    [<Fact>]
    let ``F2 division is defined only for denominator 1`` () =
        let zero = F2.Zero
        let one = F2.One

        Assert.Equal(zero, zero / one)
        Assert.Equal(one, one / one)

        Assert.Throws<DivideByZeroException>(fun () -> zero / zero |> ignore) |> ignore
        Assert.Throws<DivideByZeroException>(fun () -> one / zero |> ignore) |> ignore

    [<Fact>]
    let ``F2 negation is identity`` () =
        let zero = F2.Zero
        let one = F2.One

        Assert.Equal(zero, -zero)
        Assert.Equal(one, -one)

    [<Fact>]
    let ``F2 square is identity`` () =
        let zero = F2.Zero
        let one = F2.One

        Assert.Equal(zero, zero.Square())
        Assert.Equal(one, one.Square())

    [<Fact>]
    let ``F2 multiplicative inverse`` () =
        let one = F2.One
        let zero = F2.Zero

        Assert.Equal(one, one.Inverse())
        Assert.Throws<InvalidOperationException>(fun () -> zero.Inverse() |> ignore) |> ignore

    [<Fact>]
    let ``F2 conversions work correctly`` () =
        Assert.Equal(F2.Zero, F2.op_Explicit false)
        Assert.Equal(F2.One, F2.op_Explicit true)
        Assert.True(F2.op_Explicit F2.One)
        Assert.False(F2.op_Explicit F2.Zero)

        Assert.Equal(F2.Zero, F2.op_Explicit 0)
        Assert.Equal(F2.One, F2.op_Explicit 1)
        Assert.Equal(F2.Zero, F2.op_Explicit 2)  // 2 mod 2 = 0
        Assert.Equal(F2.One, F2.op_Explicit 3)   // 3 mod 2 = 1

        Assert.Equal(0, F2.op_Explicit F2.Zero : int)
        Assert.Equal(1, F2.op_Explicit F2.One : int)

    [<Fact>]
    let ``F2 equality and comparison`` () =
        let zero1 = F2.Zero
        let zero2 = F2(0uy)
        let one1 = F2.One
        let one2 = F2(1uy)

        Assert.Equal(zero1, zero2)
        Assert.Equal(one1, one2)
        Assert.NotEqual(zero1, one1)

        Assert.True(zero1.Equals(box zero2))
        Assert.False(zero1.Equals(box one1))
        Assert.False(zero1.Equals(box "0"))

        Assert.True((zero1 :> IComparable<F2>).CompareTo(zero2) = 0)
        Assert.True((zero1 :> IComparable<F2>).CompareTo(one1) < 0)
        Assert.True((one1 :> IComparable<F2>).CompareTo(zero1) > 0)

    [<Fact>]
    let ``F2 forms a field`` () =
        Assert.True(F2Properties.isField())