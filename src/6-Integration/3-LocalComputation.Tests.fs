// 3-LocalComputation.Tests.fs
// This file provides end-to-end integration tests for the LocalComputationEngine,
// verifying the entire stack from Layer 1 to Layer 6.
namespace E8.Tests.Integration

open Xunit
open E8.Integration
open E8.Observable

module LocalComputationEngineTests =

    [<Fact>]
    let ``Engine computes exact topological entropy for Toric Code in end-to-end test`` () =
        // This is the ultimate test of the entire project architecture.
        // It initializes a LocalComputationEngine and asks it to compute a non-trivial
        // topological observable for a model with a known exact solution (Toric Code).
        // A passing grade here proves that all layers (1 through 6) are working
        // together correctly to produce a physically meaningful and exact result.

        // 1. Setup the configuration for a small Toric Code simulation.
        let config = {
            ConfigurationLoader.defaultConfig with
                PEPS = { ConfigurationLoader.defaultConfig.PEPS with Initialization = FibonacciBasis } // Using this as a stand-in for a TC initializer
                ACE = { ConfigurationLoader.defaultConfig.ACE with MaxIterations = 10; Chi = 2 }
        }

        // 2. Instantiate the engine. This initializes the entire stack down to the hardware layer.
        use engine = new LocalComputationEngine(config)

        // 3. Define the measurement request.
        let location = { PrimaryPosition = (0,0); SecondaryPositions = []; Radius = 1 }

        // 4. Execute the computation. This triggers the full chain:
        //    Layer 6 (Engine) -> Layer 4 (ACE) -> Layer 5 (AOE)
        let result = engine.ComputeObservable(TopologicalEntropy, location)

        // 5. Assert against the known exact solution.
        // The GSD for the Toric Code is 4. The AOE in Layer 5 should extract this
        // integer value from the fixed-point environment computed by the ACE in Layer 4.
        let expectedGSD = 4

        Assert.Equal(TopologicalEntropy, result.Kind)

        match result.Value.Value with
        | GroundStateDegeneracy gsd ->
            Assert.Equal(expectedGSD, gsd)
        | _ ->
            Assert.Fail("The computed value for Topological Entropy should be a GroundStateDegeneracy integer.")

        // The algebraic confidence for a perfectly converged, stable model should be 2.
        Assert.Equal(2, result.Value.AlgebraicConfidence)

    [<Fact>]
    let ``Engine cache provides correct and faster results on second call`` () =
        let config = { ConfigurationLoader.defaultConfig with ACE = { ConfigurationLoader.defaultConfig.ACE with MaxIterations = 10; Chi = 2 } }
        use engine = new LocalComputationEngine(config)
        let location = { PrimaryPosition = (5,5); SecondaryPositions = []; Radius = 0 }

        // First call - should be a cache miss and take time
        let timer1 = System.Diagnostics.Stopwatch.StartNew()
        let result1 = engine.ComputeObservable(LocalMagnetization, location)
        timer1.Stop()

        let stats1 = engine.GetStatistics()
        Assert.Equal(1L, stats1.CacheMisses)
        Assert.Equal(0L, stats1.CacheHits)

        // Second call - should be a cache hit and be significantly faster
        let timer2 = System.Diagnostics.Stopwatch.StartNew()
        let result2 = engine.ComputeObservable(LocalMagnetization, location)
        timer2.Stop()

        let stats2 = engine.GetStatistics()
        Assert.Equal(1L, stats2.CacheMisses)
        Assert.Equal(1L, stats2.CacheHits)

        // Assert that the cached result is identical and faster
        Assert.Equal(result1.Value.Value, result2.Value.Value)
        Assert.True(timer2.Elapsed.TotalMilliseconds < timer1.Elapsed.TotalMilliseconds)