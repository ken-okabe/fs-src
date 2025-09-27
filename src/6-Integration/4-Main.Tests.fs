// 4-Main.Tests.fs
// This file contains top-level integration tests for the main application entry point.
namespace E8.Tests.Integration

open Xunit
open E8.Integration

module MainTests =

    [<Fact>]
    let ``Main execution function runs without crashing for a small valid configuration`` () =
        // This is the final end-to-end sanity test. It doesn't check the numerical
        // correctness (which is done in lower-level tests), but it ensures that the
        // entire application stack can be initialized and a simple simulation can be
        // run to completion without throwing exceptions.

        // 1. Define a minimal, valid configuration for a quick run.
        let config = {
            ConfigurationLoader.defaultConfig with
                Lattice = { Size = (2, 2); BoundaryCondition = Periodic }
                ACE = { ConfigurationLoader.defaultConfig.ACE with MaxIterations = 2; Chi = 2 }
                Observable = {
                    Measurements = [ { Type = LocalMagnetization; Parameters = Map.empty; OutputName = "test_mag" } ]
                    MeasurementLocations = AllSites
                    MeasurementInterval = 1
                }
                Output = { ConfigurationLoader.defaultConfig.Output with Verbosity = Silent }
        }

        // 2. Define the execution mode.
        let mode = FullLattice

        // 3. Execute the main function.
        let exitCode = Main.execute mode config

        // 4. Assert that the execution completed successfully.
        Assert.Equal(0, exitCode)