// src/4-PEPS-Ace/3-ProjectorSpectral.fs
// Spectral analysis based on the rigorous algebraic properties of the transfer matrix.
// For the Fibonacci anyon model on a hexagonal lattice, the transfer matrix
// is expected to be a projector (T^2 = T), which dramatically simplifies its spectrum.
namespace E8.Ace

open System
open E8.Algebra
open E8.Tensors
open E8.Hardware

/// Module for analyzing the spectral properties of transfer matrices via algebraic verification.
module ProjectorSpectral =

    /// Represents the definitive spectral information for a projector matrix.
    /// The spectrum is strictly limited to {0, 1}.
    type SpectralInfo = {
        /// The leading eigenvalue is strictly 1 for any non-trivial projector.
        LeadingEigenvalue: F2
        /// The spectral gap between eigenvalues {0, 1} is strictly 1.
        SpectralGap: F2
        /// The degeneracy of the leading eigenvalue, equivalent to the rank of the projector.
        Degeneracy: int
        /// The minimal polynomial for a projector is x^2 - x = 0 (or x^2 + x = 0 in F2), which has degree 2.
        MinimalPolynomialDegree: int
    }

    /// Represents the result of verifying the projector property (T^2 = T).
    type ProjectorVerificationResult = {
        /// True if T^2 = T holds exactly.
        IsProjector: bool
        /// The number of differing bits between T^2 and T. Should be 0 for a projector.
        DifferenceCount: int
        /// The rank of the matrix T, computed via Gaussian elimination.
        Rank: int
        /// The trace of the matrix T. For a projector, Tr(T) = Rank(T).
        Trace: int
    }

    /// Verifies the fundamental projector hypothesis (T² = T) for a given transfer matrix.
    /// This is the core function of the module, replacing iterative eigenvalue solvers.
    let verifyProjectorHypothesis (transfer: BitBlock64[]) (size: int) (ops: IF2Operations) : ProjectorVerificationResult =

        // 1. Compute T^2
        let tSquared = ops.MatrixMultiply transfer transfer size size size

        // 2. Verify T^2 = T by exact bitwise comparison.
        let (isEqual, differences) = ops.MatrixEquals transfer tSquared size

        // 3. Compute the rank and trace, which are essential properties of a projector.
        let (rank, _) = ops.ComputeTopologicalRank transfer size size

        let wordsPerRow = (size + 63) / 64
        let mutable trace = 0
        for i in 0 .. size - 1 do
            let wordIdx = i * wordsPerRow + i / 64
            let bitIdx = i % 64
            if wordIdx < transfer.Length then
                if (transfer.[wordIdx].Bits >>> bitIdx) &&& 1UL = 1UL then
                    trace <- trace + 1

        {
            IsProjector = isEqual
            DifferenceCount = differences
            Rank = rank
            Trace = trace
        }

    /// Determines the spectral information of the transfer matrix based on the projector verification.
    /// This function does not "compute" a spectrum, but rather "deduces" it from the algebraic properties.
    let getSpectralInfo (verificationResult: ProjectorVerificationResult) : SpectralInfo =
        // The core principle: If the matrix is a projector, its spectral properties are
        // uniquely and exactly determined. If not, it violates the physical model's premises.
        if verificationResult.IsProjector then
            // For a projector, eigenvalues are strictly {0, 1}.
            // The rank equals the number of '1' eigenvalues (degeneracy of the fixed point).
            {
                LeadingEigenvalue = if verificationResult.Rank > 0 then F2.One else F2.Zero
                SpectralGap = if verificationResult.Rank > 0 && verificationResult.Rank < verificationResult.Trace then F2.One else F2.Zero
                Degeneracy = verificationResult.Rank
                MinimalPolynomialDegree = if verificationResult.Rank = 0 || verificationResult.Rank = verificationResult.Trace then 1 else 2 // x=0 or x=1 or x(x+1)=0
            }
        else
            // If T^2 <> T, the matrix violates the fundamental property expected of the system's
            // ground state environment. This is not a numerical error, but a sign of a
            // fundamental theoretical inconsistency or a non-converged state.
            // Therefore, we raise a fatal exception instead of attempting to compute further.
            raise (System.InvalidOperationException(
                sprintf "FATAL: Transfer matrix is not a projector (DifferenceCount: %d). The system is not in a valid physical fixed point state consistent with the theoretical model."
                verificationResult.DifferenceCount))