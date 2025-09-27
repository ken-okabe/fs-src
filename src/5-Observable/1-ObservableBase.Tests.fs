// 1-ObservableBase.Tests.fs
// This file contains unit tests for the foundational, purely algebraic types
// for physical observables defined in 1-ObservableBase.fs.
namespace E8.Tests.Observable

open Xunit
open E8.Algebra
open E8.Observable

module ObservableBaseTests =

    [<Fact>]
    let ``AlgebraicValue can represent different physical quantities`` () =
        // Test Parity case
        let magValue = Parity F2.One
        match magValue with
        | Parity p -> Assert.Equal(F2.One, p)
        | _ -> Assert.Fail("Should have been a Parity value.")

        // Test GroundStateDegeneracy case
        let teeValue = GroundStateDegeneracy 4
        match teeValue with
        | GroundStateDegeneracy gsd -> Assert.Equal(4, gsd)
        | _ -> Assert.Fail("Should have been a GroundStateDegeneracy value.")

        // Test WilsonLoopPhase case
        let loopValue = WilsonLoopPhase F2.Zero
        match loopValue with
        | WilsonLoopPhase phase -> Assert.Equal(F2.Zero, phase)
        | _ -> Assert.Fail("Should have been a WilsonLoopPhase value.")

    [<Fact>]
    let ``ObservableValue correctly stores AlgebraicConfidence as an integer`` () =
        // The core requirement is that Confidence is no longer a float.
        let highestConfidence = 2 // Represents a perfect projector (T^2=T)
        let lowerConfidence = 5   // Represents a less stable state

        let obsVal1 = {
            Value = Parity F2.One
            AlgebraicConfidence = highestConfidence
            ContributionCount = 100
        }

        let obsVal2 = {
            Value = GroundStateDegeneracy 4
            AlgebraicConfidence = lowerConfidence
            ContributionCount = 50
        }

        Assert.Equal(2, obsVal1.AlgebraicConfidence)
        Assert.Equal(5, obsVal2.AlgebraicConfidence)
        Assert.IsType<int>(obsVal1.AlgebraicConfidence)

    [<Fact>]
    let ``ObservableResult encapsulates the full algebraic result correctly`` () =
        let location = {
            PrimaryPosition = (10, 20)
            SecondaryPositions = []
            Radius = 5
        }

        let value = {
            Value = TopologicalEntropy
            ContributionCount = 1
        }

        // This is a dummy value for the test
        let TEE_GSD = 4

        let result = {
            Kind = TopologicalEntropy
            Value = {
                Value = GroundStateDegeneracy TEE_GSD
                AlgebraicConfidence = 2
                ContributionCount = 78 // Example value for a radius 5 circle
            }
            Location = location
            ComputationTimeMs = 123.456
            DeviceUsed = "CPU (16 cores)"
            Timestamp = System.DateTime.UtcNow
        }

        Assert.Equal(TopologicalEntropy, result.Kind)
        Assert.Equal((10, 20), result.Location.PrimaryPosition)

        match result.Value.Value with
        | GroundStateDegeneracy gsd -> Assert.Equal(TEE_GSD, gsd)
        | _ -> Assert.Fail("Value should have been GroundStateDegeneracy.")

        Assert.Equal(2, result.Value.AlgebraicConfidence)