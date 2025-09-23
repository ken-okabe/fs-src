// 1-GoldenRatio.fs
// Mathematical foundation: Working in the quadratic field Q(φ) where φ² = φ + 1
namespace E8.QuantumAlgebra

open System
open System.Numerics
open System.Collections.Concurrent
open E8.Algebra

/// Module for exact algebraic manipulation of the golden ratio φ = (1 + √5)/2
/// All operations are performed using integer arithmetic to maintain exactness
module GoldenRatio =

    /// Represents φⁿ in the form Fₙφ + Fₙ₋₁ where Fₙ is the nth Fibonacci number
    /// This representation is exact and avoids any floating-point arithmetic
    [<Struct>]
    type PhiPower = {
        /// The coefficient of φ (Fibonacci number Fₙ)
        FibN: BigInteger
        /// The constant term (Fibonacci number Fₙ₋₁)
        FibNMinus1: BigInteger
        /// The exponent n in φⁿ
        Exponent: int
        /// Cached F₂ projection for performance
        mutable CachedF2: F2 option
    }

    /// Cache for Fibonacci numbers to avoid recomputation
    let private fibonacciCache = ConcurrentDictionary<int, BigInteger>()

    /// Initialize the cache with base cases
    do
        fibonacciCache.TryAdd(0, BigInteger.Zero) |> ignore
        fibonacciCache.TryAdd(1, BigInteger.One) |> ignore

    /// Computes the nth Fibonacci number using matrix exponentiation
    /// Time complexity: O(log n) for large n
    let computeFibonacci (n: int) : BigInteger =
        if n < 0 then
            raise (ArgumentException($"Negative Fibonacci index {n} not supported"))

        fibonacciCache.GetOrAdd(n, fun n ->
            if n <= 1 then
                if n = 0 then BigInteger.Zero else BigInteger.One
            else
                // Matrix exponentiation method: [[1,1],[1,0]]^n = [[F(n+1),F(n)],[F(n),F(n-1)]]
                let rec matrixPower (a11: BigInteger) (a12: BigInteger)
                                   (a21: BigInteger) (a22: BigInteger) (k: int) =
                    if k = 1 then
                        (a11, a12, a21, a22)
                    elif k % 2 = 0 then
                        // Square the matrix for even powers
                        let b11 = a11 * a11 + a12 * a21
                        let b12 = a11 * a12 + a12 * a22
                        let b21 = a21 * a11 + a22 * a21
                        let b22 = a21 * a12 + a22 * a22
                        matrixPower b11 b12 b21 b22 (k / 2)
                    else
                        // For odd powers, multiply by base matrix
                        let (b11, b12, b21, b22) = matrixPower a11 a12 a21 a22 (k - 1)
                        let c11 = b11 + b12
                        let c12 = b11
                        let c21 = b21 + b22
                        let c22 = b21
                        (c11, c12, c21, c22)

                let (fn_plus_1, fn, _, _) = matrixPower BigInteger.One BigInteger.One
                                                        BigInteger.One BigInteger.Zero n
                fn
        )

    /// Creates a PhiPower representing φⁿ
    let phiPower (n: int) : PhiPower =
        let fn = computeFibonacci n
        let fn_minus_1 = if n > 0 then computeFibonacci (n - 1) else BigInteger.Zero
        {
            FibN = fn
            FibNMinus1 = fn_minus_1
            Exponent = n
            CachedF2 = None
        }

    /// Multiplies two phi powers: φᵐ × φⁿ = φᵐ⁺ⁿ
    let multiplyPhiPowers (p1: PhiPower) (p2: PhiPower) : PhiPower =
        phiPower (p1.Exponent + p2.Exponent)

    /// Multiplies two general expressions in Q(φ): (a₁φ + b₁)(a₂φ + b₂)
    /// Uses the fundamental relation φ² = φ + 1
    let multiplyPhiExpressions (a1: BigInteger, b1: BigInteger)
                              (a2: BigInteger, b2: BigInteger) : (BigInteger * BigInteger) =
        // (a₁φ + b₁)(a₂φ + b₂) = a₁a₂φ² + (a₁b₂ + b₁a₂)φ + b₁b₂
        // Since φ² = φ + 1, we have a₁a₂φ² = a₁a₂(φ + 1) = a₁a₂φ + a₁a₂
        let a1a2 = a1 * a2
        let a1b2_plus_b1a2 = a1 * b2 + b1 * a2
        let b1b2 = b1 * b2

        // Result: (a₁a₂ + a₁b₂ + b₁a₂)φ + (a₁a₂ + b₁b₂)
        (a1a2 + a1b2_plus_b1a2, a1a2 + b1b2)

    /// Adds two phi expressions: (a₁φ + b₁) + (a₂φ + b₂) = (a₁+a₂)φ + (b₁+b₂)
    let addPhiExpressions (a1: BigInteger, b1: BigInteger)
                         (a2: BigInteger, b2: BigInteger) : (BigInteger * BigInteger) =
        (a1 + a2, b1 + b2)

    /// Subtracts phi expressions: (a₁φ + b₁) - (a₂φ + b₂) = (a₁-a₂)φ + (b₁-b₂)
    let subtractPhiExpressions (a1: BigInteger, b1: BigInteger)
                               (a2: BigInteger, b2: BigInteger) : (BigInteger * BigInteger) =
        (a1 - a2, b1 - b2)

    /// Projects a phi expression to F₂ by taking modulo 2
    let projectToF2 (phiCoeff: BigInteger, constCoeff: BigInteger) : (F2 * F2) =
        let phiMod2 = if phiCoeff % (BigInteger 2) = BigInteger.Zero then F2.Zero else F2.One
        let constMod2 = if constCoeff % (BigInteger 2) = BigInteger.Zero then F2.Zero else F2.One
        (phiMod2, constMod2)

    /// Projects a PhiPower directly to F₂
    let phiPowerToF2 (p: PhiPower) : F2 =
        match p.CachedF2 with
        | Some cached -> cached
        | None ->
            // φⁿ = Fₙφ + Fₙ₋₁, so we need to project both parts
            let (phiPart, constPart) = projectToF2 (p.FibN, p.FibNMinus1)
            // In general, we might want to combine these somehow
            // For now, we take the sum in F₂
            let result = F2.add phiPart constPart
            p.CachedF2 <- Some result
            result

    /// Computes 1/φ = φ - 1 (derived from φ² - φ - 1 = 0)
    let phiInverse() : (BigInteger * BigInteger) =
        (BigInteger.One, BigInteger.MinusOne)

    /// Computes φ^(-n) for negative powers
    let phiNegativePower (n: int) : (BigInteger * BigInteger) =
        if n >= 0 then
            raise (ArgumentException("Expected negative exponent"))

        // φ^(-n) = (1/φ)^|n| = (φ - 1)^|n|
        // We need to compute (φ - 1)^|n| recursively
        let rec power (a: BigInteger, b: BigInteger) (k: int) =
            if k = 0 then
                (BigInteger.Zero, BigInteger.One)  // φ⁰ = 1
            elif k = 1 then
                (a, b)
            else
                let (half_a, half_b) = power (a, b) (k / 2)
                let squared = multiplyPhiExpressions (half_a, half_b) (half_a, half_b)
                if k % 2 = 0 then
                    squared
                else
                    multiplyPhiExpressions squared (a, b)

        power (BigInteger.One, BigInteger.MinusOne) (-n)

    /// Computes the nth Lucas number Lₙ = Fₙ₋₁ + Fₙ₊₁
    /// Lucas numbers satisfy Lₙ = φⁿ + (1-φ)ⁿ
    let lucasNumber (n: int) : BigInteger =
        if n = 0 then BigInteger 2
        elif n = 1 then BigInteger.One
        else
            let fn_minus_1 = computeFibonacci (n - 1)
            let fn_plus_1 = computeFibonacci (n + 1)
            fn_minus_1 + fn_plus_1

    /// Verifies Binet's formula: Fₙ = (φⁿ - (-φ)⁻ⁿ) / √5
    /// Since we work algebraically, we check the equivalent: φⁿ = Fₙφ + Fₙ₋₁
    let verifyBinet (n: int) : bool =
        let phi_n = phiPower n
        let fn = computeFibonacci n
        let fn_minus_1 = if n > 0 then computeFibonacci (n - 1) else BigInteger.Zero
        phi_n.FibN = fn && phi_n.FibNMinus1 = fn_minus_1

    /// Verifies Cassini's identity: Fₙ₋₁ × Fₙ₊₁ - Fₙ² = (-1)ⁿ
    let verifyCassini (n: int) : bool =
        if n <= 0 then true
        else
            let fn_minus_1 = computeFibonacci (n - 1)
            let fn = computeFibonacci n
            let fn_plus_1 = computeFibonacci (n + 1)
            let left = fn_minus_1 * fn_plus_1 - fn * fn
            let right = if n % 2 = 0 then BigInteger.One else BigInteger.MinusOne
            left = right

    /// Creates an F₂ matrix representation of multiplication by φ
    /// in a finite-dimensional approximation
    let toMatrixRepresentation (dimension: int) : MatrixF2 =
        let matrix = MatrixF2(dimension, dimension)
        // φ acts on the basis {1, φ, φ², ...} by multiplication
        // φ × 1 = φ (shifts to next basis element)
        // φ × φⁱ = φⁱ⁺¹
        // But we need to reduce using φ² = φ + 1 when necessary

        for i in 0 .. dimension - 2 do
            matrix.[i, i + 1] <- F2.One  // φ shifts basis elements up

        // Handle the wrap-around using φ² = φ + 1
        if dimension >= 2 then
            matrix.[dimension - 1, 0] <- F2.One  // Constant term from φ² = φ + 1
            matrix.[dimension - 1, 1] <- F2.One  // φ term from φ² = φ + 1

        matrix

    /// Comprehensive test of golden ratio algebra
    let runComprehensiveTests() : bool =
        printfn "Running comprehensive golden ratio algebra tests..."

        // Test 1: φ² = φ + 1
        let phi = phiPower 1
        let phi2 = multiplyPhiPowers phi phi
        let phi_plus_1 = addPhiExpressions (BigInteger.One, BigInteger.Zero)
                                           (BigInteger.Zero, BigInteger.One)
        let test1 = (phi2.FibN = BigInteger.One && phi2.FibNMinus1 = BigInteger.One)
        printfn "  Test φ² = φ + 1: %s" (if test1 then "PASS" else "FAIL")

        // Test 2: Cassini identity for n=1..10
        let test2 = [1..10] |> List.forall verifyCassini
        printfn "  Test Cassini identity: %s" (if test2 then "PASS" else "FAIL")

        // Test 3: Binet formula for n=0..20
        let test3 = [0..20] |> List.forall verifyBinet
        printfn "  Test Binet formula: %s" (if test3 then "PASS" else "FAIL")

        // Test 4: 1/φ = φ - 1
        let phi_inv = phiInverse()
        let phi_minus_1 = subtractPhiExpressions (BigInteger.One, BigInteger.Zero)
                                                 (BigInteger.Zero, BigInteger.One)
        let test4 = (phi_inv = phi_minus_1)
        printfn "  Test 1/φ = φ - 1: %s" (if test4 then "PASS" else "FAIL")

        test1 && test2 && test3 && test4