// src/4-PEPS-Ace/3-ProjectorSpectral.Tests.fs
// This file tests the algebraic verification of the projector hypothesis for transfer matrices.
namespace E8.Tests.Ace

open System
open Xunit
open FsUnit.Xunit
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.Hardware.CPU
open E8.Ace
open E8.Ace.ProjectorSpectral

module ProjectorSpectralTests =

    // Helper function to create a projector matrix (P^2 = P).
    let createProjectorMatrix (size: int) (rank: int) =
        if rank > size then raise (System.ArgumentException("Rank cannot exceed size."))
        let wordsPerRow = (size + 63) / 64
        let matrix = Array.zeroCreate (size * wordsPerRow)
        // A simple projector: the first 'rank' diagonal elements are 1, others are 0.
        for i in 0 .. rank - 1 do
            let wordIdx = i * wordsPerRow + i / 64
            let bitIdx = i % 64
            matrix.[wordIdx] <- { Bits = matrix.[wordIdx].Bits ||| (1UL <<< bitIdx) }
        matrix

    // Helper function to create a non-projector matrix (e.g., a cyclic shift operator).
    let createNonProjectorMatrix (size: int) =
        let wordsPerRow = (size + 63) / 64
        let matrix = Array.zeroCreate (size * wordsPerRow)
        // A cyclic shift matrix where T^n = I, but T^2 <> T for n > 2.
        for i in 0 .. size - 1 do
            let j = (i + 1) % size
            let wordIdx = i * wordsPerRow + j / 64
            let bitIdx = j % 64
            matrix.[wordIdx] <- { Bits = matrix.[wordIdx].Bits ||| (1UL <<< bitIdx) }
        matrix

    [<Fact>]
    let ``Projector hypothesis correctly identifies projectors`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations
        let size = 8

        // Test Case 1: Non-trivial projector
        let rank = 4
        let projector = createProjectorMatrix size rank
        let result = verifyProjectorHypothesis projector size ops
        Assert.True(result.IsProjector, "Should identify a non-trivial projector.")
        Assert.Equal(0, result.DifferenceCount)
        Assert.Equal(rank, result.Rank)
        Assert.Equal(rank, result.Trace) // For a projector, Rank = Trace

        // Test Case 2: Identity matrix (a full-rank projector)
        let identity = createProjectorMatrix size size
        let resultId = verifyProjectorHypothesis identity size ops
        Assert.True(resultId.IsProjector, "Should identify the identity matrix as a projector.")
        Assert.Equal(size, resultId.Rank)

        // Test Case 3: Zero matrix (a rank-0 projector)
        let zero = createProjectorMatrix size 0
        let resultZero = verifyProjectorHypothesis zero size ops
        Assert.True(resultZero.IsProjector, "Should identify the zero matrix as a projector.")
        Assert.Equal(0, resultZero.Rank)

    [<Fact>]
    let ``Projector hypothesis correctly identifies non-projectors`` () =
        use ops = new OptimizedCPUOperations() :> IF2Operations
        let size = 8
        let nonProjector = createNonProjectorMatrix size

        let result = verifyProjectorHypothesis nonProjector size ops

        Assert.False(result.IsProjector, "Should correctly identify a non-projector matrix.")
        Assert.True(result.DifferenceCount > 0, "Difference count must be greater than zero for non-projectors.")

    [<Fact>]
    let ``Spectral info is correctly deduced for projectors`` () =
        let size = 8
        let rank = 4

        // Create a verification result for a valid projector
        let verificationResult = {
            IsProjector = true
            DifferenceCount = 0
            Rank = rank
            Trace = rank
        }

        let info = getSpectralInfo verificationResult
        Assert.Equal(F2.One, info.LeadingEigenvalue)
        Assert.Equal(F2.One, info.SpectralGap)
        Assert.Equal(rank, info.Degeneracy)
        Assert.Equal(2, info.MinimalPolynomialDegree)

    [<Fact>]
    let ``Spectral info throws for non-projectors, enforcing theoretical model`` () =
        // This test is critical. It ensures that a deviation from the theoretical model
        // results in a fatal error, preventing the system from producing physically meaningless results.
        let size = 8

        // Create a verification result for an invalid, non-projector matrix
        let verificationResult = {
            IsProjector = false
            DifferenceCount = 10 // Some non-zero value
            Rank = size // Rank might be anything
            Trace = 0   // Trace might be anything
        }

        // Verify that calling getSpectralInfo with this result throws the expected exception.
        let ex = Assert.Throws<System.InvalidOperationException>(fun () -> getSpectralInfo verificationResult |> ignore)
        Assert.Contains("FATAL: Transfer matrix is not a projector", ex.Message)