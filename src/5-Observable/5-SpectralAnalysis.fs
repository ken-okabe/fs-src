// 5-SpectralAnalysis.fs
// Implements observables related to the spectral properties of the transfer matrix,
// such as spectral gap and the algebraic confidence metric.
namespace E8.Observable

open System
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.Ace

/// A unified observable for quantities derived from spectral analysis.
type SpectralObservable(measureType: string) =

    let observableKind =
        match measureType.ToLower() with
        | "spectralgap" -> SpectralGap
        | "confidence" -> AlgebraicConfidenceValue
        | _ -> failwithf "Unknown spectral measure type: %s" measureType

    interface IObservable with
        member _.Kind = observableKind

        member _.Compute environment peps location ops =
            // All spectral observables are derived from the same core analysis
            // performed rigorously in Layer 4.

            // 1. Build the transfer matrix.
            let (transferMatrixData, size) = TransferMatrix.constructTransferMatrix environment peps ops

            // 2. Verify the projector hypothesis to get spectral info.
            let verification = ProjectorSpectral.verifyProjectorHypothesis transferMatrixData size ops
            let spectralInfo = ProjectorSpectral.getSpectralInfo verification

            // 3. Extract the requested value based on the measureType.
            match measureType.ToLower() with
            | "spectralgap" ->
                {
                    Value = Parity spectralInfo.SpectralGap
                    AlgebraicConfidence = spectralInfo.MinimalPolynomialDegree
                    ContributionCount = size
                }
            | "confidence" ->
                // The confidence is the minimal polynomial degree itself.
                // We return it as a GSD-like integer value for consistency in the type system.
                {
                    Value = GroundStateDegeneracy spectralInfo.MinimalPolynomialDegree
                    AlgebraicConfidence = spectralInfo.MinimalPolynomialDegree
                    ContributionCount = size * size
                }
            | _ -> failwithf "Unknown spectral measure type: %s" measureType

        member this.ComputeBatch environment peps locations ops =
            // Since the result is independent of location for a converged iPEPS,
            // we compute it once and broadcast the result.
            let singleResult = (this :> IObservable).Compute environment peps locations.[0] ops
            Array.create locations.Length singleResult