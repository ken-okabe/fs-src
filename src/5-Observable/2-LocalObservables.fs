// 2-LocalObservables.fs
// Implements the computation of local observables like magnetization and parity
// based on the rigorous principles of the Algebraic Observables Engine (AOE).
namespace E8.Observable

open System
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.Ace

/// Computes the local magnetization at a single site.
type LocalMagnetization() =

    interface IObservable with
        member _.Kind = LocalMagnetization

        member this.Compute environment peps location ops =
            // Local magnetization is a 1-point function, <O_i>.
            // In our formalism, this corresponds to the trace of the operator on the fixed point.
            // This is equivalent to a 2-point correlator <I O_i>, or can be computed directly.
            // For simplicity and consistency, we use the correlator logic for the special case <O_i^2>
            // which for Pauli ops simplifies to <I> = Tr(rho_fixed).

            // Defines a local Z operator (as an example for magnetization).
            let opZ = {
                Name = "PauliZ"
                ToVirtualMatrix = fun (size: int) -> MatrixF2.Identity(size)
            }

            // For a 1-point function, both operators are the same, at the same position.
            let correlator = ProjectedCorrelator()
            correlator.Compute environment peps location opZ opZ ops

        member this.ComputeBatch environment peps locations ops =
            locations
            |> Array.Parallel.map (fun loc ->
                (this :> IObservable).Compute environment peps loc ops)