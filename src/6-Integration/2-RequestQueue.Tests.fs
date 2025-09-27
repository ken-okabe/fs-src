// 2-RequestQueue.Tests.fs
// This file contains unit tests for the asynchronous request queue and scheduler.
namespace E8.Tests.Integration

open System
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open E8.Integration
open E8.Observable

module RequestQueueTests =

    [<Fact>]
    let ``PriorityRequestQueue correctly prioritizes items`` () =
        let queue = PriorityRequestQueue<string>()

        queue.Enqueue(5, "Normal")
        queue.Enqueue(1, "Low")
        queue.Enqueue(9, "High")

        Assert.Equal(3, queue.Count)

        let high = queue.TryDequeue()
        Assert.True(high.IsSome)
        Assert.Equal("High", high.Value)

        let normal = queue.TryDequeue()
        Assert.True(normal.IsSome)
        Assert.Equal("Normal", normal.Value)

        let low = queue.TryDequeue()
        Assert.True(low.IsSome)
        Assert.Equal("Low", low.Value)

    [<Fact>]
    let ``ComputationRequest and ComputationResult use strict algebraic types`` () =
        // This test verifies that the core data structures are aligned with the AOE theory.
        let location = {
            PrimaryPosition = (1,1)
            SecondaryPositions = []
            Radius = 0
        }

        // 1. Create a request with a strict type from Layer 5.
        let request = {
            Id = Guid.NewGuid()
            Type = ObservableMeasurement (LocalMagnetization, location)
            Priority = 5
            SubmittedAt = DateTime.UtcNow
            Deadline = None
            Callback = fun _ -> ()
        }

        Assert.Equal(LocalMagnetization, match request.Type with | ObservableMeasurement (k,_) -> k | _ -> failwith "wrong type")

        // 2. Create a result with a strict algebraic value from Layer 5.
        let result = {
            Kind = LocalMagnetization
            Value = {
                Value = Parity F2.One
                AlgebraicConfidence = 2
                ContributionCount = 10
            }
            Location = location
            ComputationTimeMs = 1.0
            DeviceUsed = "Test"
            Timestamp = DateTime.UtcNow
        }

        let compResult = ObservableResult result

        match compResult with
        | ObservableResult r ->
            Assert.Equal(2, r.Value.AlgebraicConfidence)
            match r.Value.Value with
            | Parity p -> Assert.Equal(F2.One, p)
            | _ -> Assert.Fail("Incorrect algebraic value type")
        | _ -> Assert.Fail("Incorrect result type")

    [<Fact>]
    let ``RequestScheduler can be started and stopped`` () =
        // This is a simple lifecycle test.
        let config = ConfigurationLoader.defaultConfig
        use scheduler = new RequestScheduler(config)

        // The engine is a dependency, so we create a mock or placeholder.
        use engine = new LocalComputationEngine(config)

        scheduler.Start(engine)
        // Give the background task a moment to start
        Task.Delay(50).Wait()

        scheduler.Stop()
        // No exceptions should be thrown.
        Assert.True(true)