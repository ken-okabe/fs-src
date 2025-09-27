// 1-Configuration.Tests.fs
// This file contains unit tests for the simplified and rigorous SystemConfiguration.
namespace E8.Tests.Integration

open Xunit
open FsUnit.Xunit
open E8.Integration

module ConfigurationTests =

    [<Fact>]
    let ``Default configuration is created without errors and is valid`` () =
        let config = ConfigurationLoader.defaultConfig
        Assert.NotNull(config)

        // Validate the default configuration
        let validationResult = ConfigurationLoader.validate config
        Assert.True(validationResult.IsOk, "Default configuration should be valid.")

    [<Fact>]
    let ``Default configuration reflects hexagonal lattice theory`` () =
        let config = ConfigurationLoader.defaultConfig

        // PEPS initialization should be physically meaningful
        match config.PEPS.Initialization with
        | InitializationMethod.FibonacciBasis -> () // This is a valid default
        | InitializationMethod.GoldenChain -> ()    // This is also valid
        | _ -> Assert.Fail("Default initialization should be physically meaningful for the Fibonacci model.")

        // Convergence should be based on strict, algebraic methods
        Assert.Equal(ConvergenceMethod.StateHash, config.ACE.ConvergenceMethod)

    [<Fact>]
    let ``Configuration validation catches invalid settings`` () =
        let config = ConfigurationLoader.defaultConfig

        // Test invalid lattice size
        let invalidLattice = { config with Lattice = { config.Lattice with Size = (-10, 10) } }
        let result1 = ConfigurationLoader.validate invalidLattice
        Assert.True(result1.IsError)
        match result1 with
        | Error e -> Assert.Contains("Lattice size must be positive", e.[0])
        | Ok _ -> Assert.Fail("Validation should have failed.")

        // Test invalid chi configuration
        let invalidChi = { config with ACE = { config.ACE with MaxChi = config.ACE.InitialChi - 1 } }
        let result2 = ConfigurationLoader.validate invalidChi
        Assert.True(result2.IsError)
        match result2 with
        | Error e -> Assert.Contains("Max chi must be >= initial chi", e.[0])
        | Ok _ -> Assert.Fail("Validation should have failed.")

    [<Fact>]
    let ``Configuration types do not contain obsolete fields`` () =
        // This test uses reflection to ensure that fields removed during the refactoring
        // (like EnforceSymmetry) no longer exist, preventing accidental reintroduction.

        let latticeConfigType = typeof<LatticeConfiguration>
        let enforceSymmetryProp = latticeConfigType.GetProperty("EnforceSymmetry")
        Assert.Null(enforceSymmetryProp) // This property should be completely gone.

        let pepsConfigType = typeof<PEPSConfiguration>
        let ensureMPOProp = pepsConfigType.GetProperty("EnsureMPOInjectivity")
        Assert.Null(ensureMPOProp) // This property should be completely gone.

        let observableConfigType = typeof<ObservableConfiguration>
        let symmetryAverageProp = observableConfigType.GetProperty("SymmetryAverage")
        Assert.Null(symmetryAverageProp) // This property should be completely gone.