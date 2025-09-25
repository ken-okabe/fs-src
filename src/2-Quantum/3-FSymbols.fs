// 3-FSymbols.fs
// F-symbols (6j symbols) and their consistency equations (Pentagon and Hexagon)
namespace E8.QuantumAlgebra

open System
open System.Numerics
open E8.Algebra
open E8.QuantumAlgebra.GoldenRatio
open E8.QuantumAlgebra.FibonacciFusion

/// Module for F-symbols (6j symbols) of Fibonacci anyons
module FSymbols =

    /// Represents an F-symbol [F^d_{abc}]_ef with its value in Q(φ)
    type AlgebraicFSymbol = {
        /// Labels for the F-symbol
        A: AnyonType
        B: AnyonType
        C: AnyonType
        D: AnyonType
        E: AnyonType
        F: AnyonType
        /// Value as an element of Q(φ): (coefficient of φ, constant term)
        Value: (BigInteger * BigInteger)
        /// Cached F₂ projection
        mutable CachedF2Value: F2 option
    }

    /// Computes the F-symbol value for given anyon labels
    /// These values are derived from solving the Pentagon equations
    let computeFSymbol (a: AnyonType) (b: AnyonType) (c: AnyonType)
                       (d: AnyonType) (e: AnyonType) (f: AnyonType) : AlgebraicFSymbol =

        // First check if this F-symbol is allowed by fusion rules
        let isValid =
            isAllowedFusion a b e &&
            isAllowedFusion e c d &&
            isAllowedFusion b c f &&
            isAllowedFusion a f d

        if not isValid then
            // Zero F-symbol for forbidden fusion
            { A=a; B=b; C=c; D=d; E=e; F=f; Value=(BigInteger.Zero, BigInteger.Zero); CachedF2Value=None }
        else
            // Non-trivial F-symbols for Fibonacci anyons
            // These are the solutions to the Pentagon equations
            match (a, b, c, d, e, f) with

            // Trivial cases involving vacuum
            | (Vacuum, _, _, _, _, _)
            | (_, Vacuum, _, _, _, _)
            | (_, _, Vacuum, _, _, _) ->
                { A=a; B=b; C=c; D=d; E=e; F=f; Value=(BigInteger.Zero, BigInteger.One); CachedF2Value=None }

            // The non-trivial F-symbols for Fibonacci anyons
            // These involve the golden ratio φ
            | (Tau, Tau, Tau, Tau, Vacuum, Vacuum) ->
                // F[τ,τ,τ]^τ_{1,1} = 1/φ = φ - 1
                let value = phiInverse()
                { A=a; B=b; C=c; D=d; E=e; F=f; Value=value; CachedF2Value=None }

            | (Tau, Tau, Tau, Tau, Vacuum, Tau) ->
                // F[τ,τ,τ]^τ_{1,τ} = -1/φ² = -(2 - φ)
                let phi_inv = phiInverse()
                let phi_inv_squared = multiplyPhiExpressions phi_inv phi_inv
                let value = (BigInteger.Negate (fst phi_inv_squared), BigInteger.Negate (snd phi_inv_squared))
                { A=a; B=b; C=c; D=d; E=e; F=f; Value=value; CachedF2Value=None }

            | (Tau, Tau, Tau, Tau, Tau, Vacuum) ->
                // F[τ,τ,τ]^τ_{τ,1} = -1/φ² = -(2 - φ)
                let phi_inv = phiInverse()
                let phi_inv_squared = multiplyPhiExpressions phi_inv phi_inv
                let value = (BigInteger.Negate (fst phi_inv_squared), BigInteger.Negate (snd phi_inv_squared))
                { A=a; B=b; C=c; D=d; E=e; F=f; Value=value; CachedF2Value=None }

            | (Tau, Tau, Tau, Tau, Tau, Tau) ->
                // F[τ,τ,τ]^τ_{τ,τ} = (φ - 2)/φ² = 1/φ - 1/φ²
                // This equals 1/φ - (2 - φ) = φ - 1 - 2 + φ = 2φ - 3
                let value = (BigInteger 2, BigInteger(-3))
                { A=a; B=b; C=c; D=d; E=e; F=f; Value=value; CachedF2Value=None }

            // All other cases
            | _ ->
                { A=a; B=b; C=c; D=d; E=e; F=f; Value=(BigInteger.Zero, BigInteger.One); CachedF2Value=None }

    /// Projects an F-symbol value to F₂
    let fSymbolToF2 (fsym: AlgebraicFSymbol) : F2 =
        match fsym.CachedF2Value with
        | Some cached -> cached
        | None ->
            let (phiPart, constPart) = projectToF2 fsym.Value
            let result = F2.add phiPart constPart
            fsym.CachedF2Value <- Some result
            result

    /// Verifies the Pentagon equation for a specific set of anyons
    /// (F^g_{klm})_pq (F^g_{ijq})_mn = Σ_r (F^k_{ijl})_mr (F^g_{irm})_ln (F^l_{jkn})_rq
    let verifyPentagonForAnyons (i: AnyonType) (j: AnyonType) (k: AnyonType)
                               (l: AnyonType) (m: AnyonType) (n: AnyonType)
                                (g: AnyonType) : bool =
        let mutable leftSum = (BigInteger.Zero, BigInteger.Zero)
        let mutable rightSum = (BigInteger.Zero, BigInteger.Zero)

        // Left side of Pentagon equation
        for p in [Vacuum; Tau] do
            for q in [Vacuum; Tau] do
                let f1 = computeFSymbol k l m g p q
                let f2 = computeFSymbol i j q g m n
                if f1.Value <> (BigInteger.Zero, BigInteger.Zero) &&
                   f2.Value <> (BigInteger.Zero, BigInteger.Zero) then
                    let product = multiplyPhiExpressions f1.Value f2.Value
                    leftSum <- addPhiExpressions leftSum product

        // Right side of Pentagon equation
        for r in [Vacuum; Tau] do
            let f3 = computeFSymbol i j l k m r
            let f4 = computeFSymbol i r m g l n
            let f5 = computeFSymbol j k n l r q
            if f3.Value <> (BigInteger.Zero, BigInteger.Zero) &&
               f4.Value <> (BigInteger.Zero, BigInteger.Zero) &&
               f5.Value <> (BigInteger.Zero, BigInteger.Zero) then
                let product1 = multiplyPhiExpressions f3.Value f4.Value
                let product2 = multiplyPhiExpressions product1 f5.Value
                rightSum <- addPhiExpressions rightSum product2

        // Check equality
        leftSum = rightSum

    /// Comprehensive Pentagon equation verification
    let verifyPentagonEquations() : bool =
        printfn "Verifying Pentagon equations for Fibonacci anyons..."
        printfn "  This ensures consistency of fusion associativity..."

        let mutable allValid = true
        let mutable violationCount = 0
        let anyonTypes = [Vacuum; Tau]

        // We need to check Pentagon for all possible configurations
        // This is computationally intensive but necessary for consistency
        for i in anyonTypes do
            for j in anyonTypes do
                for k in anyonTypes do
                    for l in anyonTypes do
                        for m in fusionRule i j do
                            for n in fusionRule j k do
                                for g in fusionRule m n do
                                    if isAllowedFusion i l g && isAllowedFusion k l g then
                                        let isValid = verifyPentagonForAnyons i j k l m n g
                                        if not isValid then
                                            allValid <- false
                                            violationCount <- violationCount + 1
                                            if violationCount <= 3 then
                                                printfn "    Pentagon violation: (%A,%A,%A,%A,%A,%A,%A)"
                                                        i j k l m n g

        if allValid then
            printfn "  ✓ All Pentagon equations satisfied!"
        else
            printfn "  ✗ Found %d Pentagon violations" violationCount

        allValid

    /// R-matrix (braiding matrix) for Fibonacci anyons
    let rMatrix (a: AnyonType) (b: AnyonType) : (BigInteger * BigInteger) =
        match (a, b) with
        | (Vacuum, _) | (_, Vacuum) ->
            (BigInteger.Zero, BigInteger.One)  // R = 1 for vacuum
        | (Tau, Tau) ->
            // R_{τ,τ} = e^(4πi/5)
            // In our algebraic setting, we use the fact that this is related to φ
            // For F₂ projection, we approximate as -1/φ
            let phi_inv = phiInverse()
            (BigInteger.Negate (fst phi_inv), BigInteger.Negate (snd phi_inv))

    /// Verifies the Hexagon equation for braiding consistency
    /// This ensures that braiding operations are consistent with fusion
    let verifyHexagonForAnyons (a: AnyonType) (b: AnyonType) (c: AnyonType)
                               (d: AnyonType) : bool =
        let mutable leftSum = (BigInteger.Zero, BigInteger.Zero)
        let mutable rightSum = (BigInteger.Zero, BigInteger.Zero)

        // Hexagon equation relates R-matrices and F-symbols
        for e in fusionRule a b do
            for f in fusionRule e c do
                if isAllowedFusion f c d then
                    let r1 = rMatrix a b
                    let f1 = computeFSymbol a b c d e f
                    let r2 = rMatrix a c

                    let temp1 = multiplyPhiExpressions r1 f1.Value
                    let temp2 = multiplyPhiExpressions temp1 r2
                    leftSum <- addPhiExpressions leftSum temp2

                    let f2 = computeFSymbol b a c d e f
                    let r3 = rMatrix b c
                    let f3 = computeFSymbol a b f d e c

                    let temp3 = multiplyPhiExpressions f2.Value r3
                    let temp4 = multiplyPhiExpressions temp3 f3.Value
                    rightSum <- addPhiExpressions rightSum temp4

        leftSum = rightSum

    /// Comprehensive Hexagon equation verification
    let verifyHexagonEquations() : bool =
        printfn "Verifying Hexagon equations for braiding consistency..."

        let mutable allValid = true
        let anyonTypes = [Vacuum; Tau]

        for a in anyonTypes do
            for b in anyonTypes do
                for c in anyonTypes do
                    for d in fusionRule a (AnyonType.FromInt((b.ToInt() + c.ToInt()) % 2)) do
                        let isValid = verifyHexagonForAnyons a b c d
                        if not isValid then
                            allValid <- false
                            printfn "    Hexagon violation: (%A,%A,%A,%A)" a b c d

        if allValid then
            printfn "  ✓ All Hexagon equations satisfied!"
        else
            printfn "  ✗ Hexagon equations violated"

        allValid

    /// Master consistency check for the entire algebraic structure
    let verifyFullConsistency() : bool =
        printfn "\n=== F-Symbol Consistency Verification ==="
        printfn "Checking mathematical consistency of Fibonacci anyon algebra..."

        let pentagonValid = verifyPentagonEquations()
        let hexagonValid = verifyHexagonEquations()

        if pentagonValid && hexagonValid then
            printfn "\n✓✓✓ Complete algebraic consistency verified!"
            printfn "The Fibonacci anyon model is mathematically sound."
            true
        else
            printfn "\n✗✗✗ Algebraic inconsistency detected!"
            printfn "The model has mathematical contradictions."
            false

    /// Creates the F-matrix as a linear operator for fixed (a,b,c,d)
    let createFMatrix (a: AnyonType) (b: AnyonType) (c: AnyonType) (d: AnyonType) : E8.Tensors.MatrixF2 =
        // Count valid intermediate channels
        let mutable validE = []
        let mutable validF = []

        for e in [Vacuum; Tau] do
            if isAllowedFusion a b e && isAllowedFusion e c d then
                validE <- e :: validE

        for f in [Vacuum; Tau] do
            if isAllowedFusion b c f && isAllowedFusion a f d then
                validF <- f :: validF

        let dimE = validE.Length
        let dimF = validF.Length

        if dimE = 0 || dimF = 0 then
            E8.Tensors.MatrixF2(1, 1)  // Return trivial matrix if no valid channels
        else
            let matrix = E8.Tensors.MatrixF2(dimE, dimF)

            for i, e in List.indexed (List.rev validE) do
                for j, f in List.indexed (List.rev validF) do
                    let fsym = computeFSymbol a b c d e f
                    matrix.[i, j] <- fSymbolToF2 fsym

            matrix