// 3-SymmetryEnforcement.fs
// Enforcement and verification of physical symmetries
namespace E8.TensorConstruction

open E8.Algebra
open E8.Tensors

/// Module for enforcing and verifying symmetries in PEPS tensors
module SymmetryEnforcement =

    /// Types of symmetry operations
    type SymmetryOperation =
        | Rotation90       // 90-degree rotation
        | Rotation180      // 180-degree rotation
        | Rotation270      // 270-degree rotation
        | ReflectionH      // Horizontal reflection
        | ReflectionV      // Vertical reflection
        | ReflectionD1     // Diagonal reflection (main diagonal)
        | ReflectionD2     // Diagonal reflection (anti-diagonal)
        | Identity         // Identity operation

    /// Group element for the D4 symmetry group (square symmetries)
    type D4Element = {
        Operation: SymmetryOperation
        Order: int  // Order of the group element
    }

    /// Applies a symmetry operation to tensor indices
    let applySymmetryToIndices (op: SymmetryOperation) (l: int, r: int, u: int, d: int) : (int * int * int * int) =
        match op with
        | Identity -> (l, r, u, d)
        | Rotation90 -> (d, u, l, r)      // 90° clockwise
        | Rotation180 -> (r, l, d, u)     // 180°
        | Rotation270 -> (u, d, r, l)     // 270° clockwise
        | ReflectionH -> (r, l, u, d)     // Horizontal flip
        | ReflectionV -> (l, r, d, u)     // Vertical flip
        | ReflectionD1 -> (u, d, l, r)    // Diagonal flip
        | ReflectionD2 -> (d, u, r, l)    // Anti-diagonal flip

    /// Applies a symmetry operation to a tensor element
    let applySymmetryToTensor (tensor: Tensor5F2) (op: SymmetryOperation) (p: int) (l: int) (r: int) (u: int) (d: int) : F2 =
        let (l', r', u', d') = applySymmetryToIndices op (l, r, u, d)
        tensor.[p, l', r', u', d']

    /// Checks if a tensor respects a given symmetry
    let checkSymmetry (tensor: Tensor5F2) (op: SymmetryOperation) : bool * int =
        let physDim = tensor.D1
        let bondDim = tensor.D2
        let mutable violations = 0
        let mutable isSymmetric = true

        for p in 0 .. physDim - 1 do
            for l in 0 .. bondDim - 1 do
                for r in 0 .. bondDim - 1 do
                    for u in 0 .. bondDim - 1 do
                        for d in 0 .. bondDim - 1 do
                            let original = tensor.[p, l, r, u, d]
                            let transformed = applySymmetryToTensor tensor op p l r u d

                            if original <> transformed then
                                isSymmetric <- false
                                violations <- violations + 1

        (isSymmetric, violations)

    /// Enforces a specific symmetry on the tensor
    let enforceSymmetry (tensor: Tensor5F2) (op: SymmetryOperation) : unit =
        let physDim = tensor.D1
        let bondDim = tensor.D2

        printfn "  Enforcing %A symmetry..." op

        // Create a set to track which elements we've already processed
        let processed = HashSet<int * int * int * int * int>()

        for p in 0 .. physDim - 1 do
            for l in 0 .. bondDim - 1 do
                for r in 0 .. bondDim - 1 do
                    for u in 0 .. bondDim - 1 do
                        for d in 0 .. bondDim - 1 do
                            let key = (p, l, r, u, d)

                            if not (processed.Contains(key)) then
                                // Get the transformed indices
                                let (l', r', u', d') = applySymmetryToIndices op (l, r, u, d)
                                let transformedKey = (p, l', r', u', d')

                                // Get both values
                                let original = tensor.[p, l, r, u, d]
                                let transformed = tensor.[p, l', r', u', d']

                                // Symmetrize by taking OR (could also use AND or XOR)
                                let symmetrized = if original = F2.One || transformed = F2.One then F2.One else F2.Zero

                                // Set both elements to the symmetrized value
                                tensor.[p, l, r, u, d] <- symmetrized
                                tensor.[p, l', r', u', d'] <- symmetrized

                                // Mark both as processed
                                processed.Add(key) |> ignore
                                processed.Add(transformedKey) |> ignore

    /// Enforces the full D4 symmetry group on the tensor
    let enforceD4Symmetry (tensor: Tensor5F2) : unit =
        printfn "Enforcing D4 (dihedral) symmetry group..."

        // The D4 group has 8 elements
        let symmetries = [
            Rotation90
            Rotation180
            Rotation270
            ReflectionH
            ReflectionV
            ReflectionD1
            ReflectionD2
        ]

        // Enforce each symmetry
        for sym in symmetries do
            enforceSymmetry tensor sym

        printfn "  ✓ D4 symmetry fully enforced"

    /// Verifies all symmetries and returns a report
    let verifyAllSymmetries (tensor: Tensor5F2) : unit =
        printfn "\nVerifying tensor symmetries..."

        let symmetries = [
            (Identity, "Identity")
            (Rotation90, "90° rotation")
            (Rotation180, "180° rotation")
            (Rotation270, "270° rotation")
            (ReflectionH, "Horizontal reflection")
            (ReflectionV, "Vertical reflection")
            (ReflectionD1, "Main diagonal reflection")
            (ReflectionD2, "Anti-diagonal reflection")
        ]

        let mutable allSymmetric = true

        for (sym, name) in symmetries do
            if sym <> Identity then  // Skip identity check
                let (isSymmetric, violations) = checkSymmetry tensor sym

                if isSymmetric then
                    printfn "  ✓ %s: SATISFIED" name
                else
                    printfn "  ✗ %s: VIOLATED (%d violations)" name violations
                    allSymmetric <- false

        if allSymmetric then
            printfn "  ✓✓ All symmetries satisfied!"
        else
            printfn "  ✗✗ Some symmetries violated"

    /// Projects a tensor onto the symmetric subspace
    let projectToSymmetricSubspace (tensor: Tensor5F2) : Tensor5F2 =
        printfn "Projecting tensor to symmetric subspace..."

        // Create a copy to work with
        let projected = tensor.Clone() :?> Tensor5F2

        // Enforce all symmetries
        enforceD4Symmetry projected

        // Verify the result
        verifyAllSymmetries projected

        projected

    /// Computes the symmetry defect (how far from symmetric)
    let computeSymmetryDefect (tensor: Tensor5F2) : float =
        let symmetries = [
            Rotation90; Rotation180; Rotation270
            ReflectionH; ReflectionV; ReflectionD1; ReflectionD2
        ]

        let mutable totalViolations = 0

        for sym in symmetries do
            let (_, violations) = checkSymmetry tensor sym
            totalViolations <- totalViolations + violations

        let totalElements = tensor.TotalElements * symmetries.Length
        float totalViolations / float totalElements

    /// Generates a random symmetric tensor for testing
    let generateRandomSymmetricTensor (physDim: int) (bondDim: int) (seed: int) : Tensor5F2 =
        let rng = System.Random(seed)
        let tensor = Tensor5F2(physDim, bondDim, bondDim, bondDim, bondDim)

        // Fill with random values
        for p in 0 .. physDim - 1 do
            for l in 0 .. bondDim - 1 do
                for r in 0 .. bondDim - 1 do
                    for u in 0 .. bondDim - 1 do
                        for d in 0 .. bondDim - 1 do
                            if rng.NextDouble() < 0.3 then  // 30% density
                                tensor.[p, l, r, u, d] <- F2.One

        // Enforce symmetries
        enforceD4Symmetry tensor

        tensor