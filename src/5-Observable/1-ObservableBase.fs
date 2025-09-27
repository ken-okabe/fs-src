// 1-ObservableBase.fs
// Defines the foundational, purely algebraic types for physical observables.
// All floating-point representations are strictly prohibited.
namespace E8.Observable

open System
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.Ace

/// Represents the strictly algebraic value of a physical observable.
/// This type replaces simple F2 parity with a more descriptive, strongly-typed value.
type AlgebraicValue =
    /// A simple Z2 parity value (e.g., for magnetization).
    | Parity of F2
    /// An integer value representing the Ground State Degeneracy (for TEE).
    | GroundStateDegeneracy of int
    /// An F2 value representing the phase of a Wilson Loop (0 for trivial, 1 for non-trivial).
    | WilsonLoopPhase of F2
    /// Represents a correlation value, which is fundamentally a parity in F2.
    | CorrelationParity of F2

/// Represents the result of an observable measurement, defined in purely algebraic terms.
type ObservableValue = {
    /// The algebraic value of the observable.
    Value: AlgebraicValue
    /// A measure of the structural stability of the fixed point from which the observable was calculated.
    /// This is the degree of the minimal polynomial of the transfer matrix. A value of 2 indicates
    /// a perfectly stable projector, which is the highest level of confidence.
    AlgebraicConfidence: int
    /// The number of tensor elements that contributed to this calculation.
    ContributionCount: int
}

/// Discriminates between different kinds of physical observables that can be measured.
type ObservableKind =
    | LocalMagnetization
    | LocalParity
    | TwoPointCorrelation of distance: int * int
    | ConnectedCorrelation of distance: int * int
    | WilsonLoop of contour: (int * int) list
    | StringOrder of path: (int * int) list
    | TopologicalEntropy
    | PlaquetteOperator of size: int
    | VortexDensity
    | ChiralOrder
    | SpectralGap
    | AlgebraicConfidenceValue

/// Defines the geometric location(s) for a measurement.
type MeasurementLocation = {
    /// The primary position for the measurement (e.g., center of a plaquette).
    PrimaryPosition: int * int
    /// Auxiliary positions (e.g., the second point in a correlation function).
    SecondaryPositions: (int * int) list
    /// The radius of the measurement region (for observables like TEE).
    Radius: int
}

/// Encapsulates the complete result of a single observable measurement.
type ObservableResult = {
    /// The kind of observable that was measured.
    Kind: ObservableKind
    /// The strictly algebraic result of the measurement.
    Value: ObservableValue
    /// The location where the measurement was performed.
    Location: MeasurementLocation
    /// The wall-clock time taken for the computation.
    ComputationTimeMs: float
    /// A string identifying the hardware device used for the computation.
    DeviceUsed: string
    /// The timestamp of when the computation was completed.
    Timestamp: DateTime
}

/// A common interface for all observable computation modules.
/// Ensures that all observables can be computed in a standardized way.
type IObservable =
    /// Computes a single observable at a specific location.
    abstract member Compute:
        environment: ACE_CTMRG.SymmetricEnvironment ->
        peps: Tensor4F2 ->
        location: MeasurementLocation ->
        ops: IF2Operations ->
        ObservableValue

    /// Computes a batch of observables at multiple locations.
    abstract member ComputeBatch:
        environment: ACE_CTMRG.SymmetricEnvironment ->
        peps: Tensor4F2 ->
        locations: MeasurementLocation[] ->
        ops: IF2Operations ->
        ObservableValue[]

    /// Returns the kind of the observable.
    abstract member Kind: ObservableKind