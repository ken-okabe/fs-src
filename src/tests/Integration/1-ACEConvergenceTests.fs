// tests/Integration/ACEConvergenceTests.fs
namespace E8.Tests.Integration

open System
open Xunit
open E8.Algebra
open E8.Tensors
open E8.Hardware
open E8.Ace
open E8.Integration

/// ACE収束テスト
module ACEConvergenceTests =

    let createTestConfig() = {
        ConfigurationLoader.defaultConfig with
            Lattice = {
                ConfigurationLoader.defaultConfig.Lattice with
                    Size = (8, 8)
            }
            ACE = {
                ConfigurationLoader.defaultConfig.ACE with
                    InitialChi = 4
                    MaxChi = 16
                    MaxIterations = 20
                    ConvergenceThreshold = 1e-6
            }
    }

    /// ACEは収束する
    [<Fact>]
    let ``ACE converges for small system`` () =
        let config = createTestConfig()
        use engine = new LocalComputationEngine(config)

        let (env, converged, iterations) = engine.ConvergeEnvironment((0, 0), 20)

        Assert.True(converged, sprintf "Did not converge in %d iterations" iterations)
        Assert.True(iterations <= 20, sprintf "Too many iterations: %d" iterations)

    /// 収束後の環境は安定
    [<Fact>]
    let ``Converged environment is stable`` () =
        let config = createTestConfig()
        use engine = new LocalComputationEngine(config)

        // 初回収束
        let (env1, converged1, _) = engine.ConvergeEnvironment((0, 0), 20)
        Assert.True(converged1)

        // 同じ位置で再計算
        let (env2, converged2, iterations2) = engine.ConvergeEnvironment((0, 0), 20)
        Assert.True(converged2)
        Assert.Equal(1, iterations2)  // 既に収束しているので1反復で終了

    /// 異なる初期条件でも同じ固定点に収束
    [<Fact>]
    let ``Different initializations converge to same fixed point`` () =
        let config1 = {
            createTestConfig() with
                PEPS = {
                    createTestConfig().PEPS with
                        Initialization = Random 42
                }
        }

        let config2 = {
            createTestConfig() with
                PEPS = {
                    createTestConfig().PEPS with
                        Initialization = Random 123
                }
        }

        use engine1 = new LocalComputationEngine(config1)
        use engine2 = new LocalComputationEngine(config2)

        let (env1, conv1, _) = engine1.ConvergeEnvironment((0, 0), 50)
        let (env2, conv2, _) = engine2.ConvergeEnvironment((0, 0), 50)

        Assert.True(conv1 && conv2, "Both should converge")

        // 収束値の比較（ハッシュで）
        use sha = System.Security.Cryptography.SHA256.Create()
        let hash1 = sha.ComputeHash(
                        env1.UniqueCorner
                        |> Array.collect (fun b -> BitConverter.GetBytes(b.Bits)))
        let hash2 = sha.ComputeHash(
                        env2.UniqueCorner
                        |> Array.collect (fun b -> BitConverter.GetBytes(b.Bits)))

        // ハッシュが近いことを確認（完全一致は期待しない）
        let difference =
            Array.zip hash1 hash2
            |> Array.sumBy (fun (a, b) -> abs(int a - int b))

        Assert.True(difference < 1000, sprintf "Hashes differ too much: %d" difference)
