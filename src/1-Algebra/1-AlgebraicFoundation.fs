// 1-AlgebraicFoundation.fs
// 代数的基盤層：F₂体とその基本演算

namespace E8.Algebra

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

/// F₂体の要素
[<Struct; StructLayout(LayoutKind.Sequential, Pack=1)>]
type F2 =
    val Value: byte

    new(v: byte) =
        if v > 1uy then
            raise (ArgumentException($"F2 policy violation: {v} ∉ {{0,1}}"))
        { Value = v }

    static member Zero = F2(0uy)
    static member One = F2(1uy)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member inline (+) (a: F2, b: F2) = F2(a.Value ^^^ b.Value)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member inline (*) (a: F2, b: F2) = F2(a.Value &&& b.Value)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member inline (/) (a: F2, b: F2) =
        if b.Value = 0uy then
            raise (DivideByZeroException("Division by zero in F2"))
        a  // In F2, a/1 = a, and division by 0 is undefined

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    static member inline (~-) (a: F2) = a  // Additive inverse in F2

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member inline x.Square() = x

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member inline x.Inverse() =
        if x.Value = 0uy then
            raise (InvalidOperationException("Zero has no multiplicative inverse"))
        x  // 1^(-1) = 1 in F2

    override x.ToString() = if x.Value = 1uy then "1" else "0"

    override x.Equals(obj) =
        match obj with
        | :? F2 as other -> x.Value = other.Value
        | _ -> false

    override x.GetHashCode() = x.Value.GetHashCode()

    interface IEquatable<F2> with
        member x.Equals(other) = x.Value = other.Value

    interface IComparable<F2> with
        member x.CompareTo(other) = x.Value.CompareTo(other.Value)

    static member op_Explicit(b: bool) : F2 = if b then F2.One else F2.Zero
    static member op_Explicit(f: F2) : bool = f.Value = 1uy
    static member op_Explicit(i: int) : F2 = F2(byte(i &&& 1))
    static member op_Explicit(f: F2) : int = int f.Value

/// F₂体の演算プロパティ
module F2Properties =

    let isField() =
        // Verify F2 forms a field
        let zero = F2.Zero
        let one = F2.One

        // Additive identity
        let additiveIdentity =
            (zero + zero = zero) &&
            (zero + one = one) &&
            (one + zero = one)

        // Multiplicative identity
        let multiplicativeIdentity =
            (one * one = one) &&
            (one * zero = zero) &&
            (zero * one = zero)

        // Additive inverse
        let additiveInverse =
            (zero + zero = zero) &&
            (one + one = zero)  // Important: 1 + 1 = 0 in F2

        // Multiplicative inverse (for non-zero)
        let multiplicativeInverse =
            (one * one = one)

        // Distributivity
        let distributivity =
            let check a b c =
                a * (b + c) = (a * b) + (a * c)
            check zero zero zero &&
            check zero zero one &&
            check zero one zero &&
            check zero one one &&
            check one zero zero &&
            check one zero one &&
            check one one zero &&
            check one one one

        additiveIdentity && multiplicativeIdentity &&
        additiveInverse && multiplicativeInverse && distributivity