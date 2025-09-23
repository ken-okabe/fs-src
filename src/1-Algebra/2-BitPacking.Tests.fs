// 2-BitPacking.Tests.fs

namespace E8.Tests.BitPacking

open Xunit
open E8.BitPacking

module BitOperationsTests =

    [<Fact>]
    let ``Word index computation is correct`` () =
        Assert.Equal(0, BitOperations.getWordIndex 0)
        Assert.Equal(0, BitOperations.getWordIndex 63)
        Assert.Equal(1, BitOperations.getWordIndex 64)
        Assert.Equal(1, BitOperations.getWordIndex 127)
        Assert.Equal(2, BitOperations.getWordIndex 128)

    [<Fact>]
    let ``Bit offset computation is correct`` () =
        Assert.Equal(0, BitOperations.getBitOffset 0)
        Assert.Equal(63, BitOperations.getBitOffset 63)
        Assert.Equal(0, BitOperations.getBitOffset 64)
        Assert.Equal(1, BitOperations.getBitOffset 65)
        Assert.Equal(0, BitOperations.getBitOffset 128)

    [<Fact>]
    let ``Word count computation handles edge cases`` () =
        Assert.Equal(1, BitOperations.computeWordCount 1)
        Assert.Equal(1, BitOperations.computeWordCount 63)
        Assert.Equal(1, BitOperations.computeWordCount 64)
        Assert.Equal(2, BitOperations.computeWordCount 65)
        Assert.Equal(2, BitOperations.computeWordCount 128)
        Assert.Equal(3, BitOperations.computeWordCount 129)

    [<Fact>]
    let ``Set and get bit operations work correctly`` () =
        let words = Array.zeroCreate<uint64> 3

        BitOperations.setBit words 0 true
        Assert.True(BitOperations.getBit words 0)

        BitOperations.setBit words 63 true
        Assert.True(BitOperations.getBit words 63)

        BitOperations.setBit words 64 true
        Assert.True(BitOperations.getBit words 64)

        BitOperations.setBit words 127 true
        Assert.True(BitOperations.getBit words 127)

        BitOperations.setBit words 128 true
        Assert.True(BitOperations.getBit words 128)

        BitOperations.setBit words 63 false
        Assert.False(BitOperations.getBit words 63)

        Assert.True(BitOperations.getBit words 0)
        Assert.False(BitOperations.getBit words 1)
        Assert.True(BitOperations.getBit words 64)
        Assert.True(BitOperations.getBit words 127)
        Assert.True(BitOperations.getBit words 128)

    [<Fact>]
    let ``Toggle bit works correctly`` () =
        let words = Array.zeroCreate<uint64> 2

        BitOperations.toggleBit words 10
        Assert.True(BitOperations.getBit words 10)

        BitOperations.toggleBit words 10
        Assert.False(BitOperations.getBit words 10)

        BitOperations.toggleBit words 100
        Assert.True(BitOperations.getBit words 100)

    [<Fact>]
    let ``Clear bit works correctly`` () =
        let words = [| UInt64.MaxValue; UInt64.MaxValue |]

        BitOperations.clearBit words 10
        Assert.False(BitOperations.getBit words 10)
        Assert.True(BitOperations.getBit words 9)
        Assert.True(BitOperations.getBit words 11)

        BitOperations.clearBit words 100
        Assert.False(BitOperations.getBit words 100)

    [<Fact>]
    let ``Count set bits works correctly`` () =
        let words = [| 0UL; 0UL |]
        Assert.Equal(0, BitOperations.countSetBits words)

        let words = [| UInt64.MaxValue; UInt64.MaxValue |]
        Assert.Equal(128, BitOperations.countSetBits words)

        let words = [| 0b1010101010101010UL; 0b1111000011110000UL |]
        Assert.Equal(8 + 8, BitOperations.countSetBits words)

    [<Fact>]
    let ``Find first and last set bit`` () =
        let words = [| 0UL; 0UL; 0UL |]
        Assert.Equal(None, BitOperations.findFirstSetBit words)
        Assert.Equal(None, BitOperations.findLastSetBit words)

        let words = [| 0UL; 1UL <<< 10; 0UL |]
        Assert.Equal(Some (64 + 10), BitOperations.findFirstSetBit words)
        Assert.Equal(Some (64 + 10), BitOperations.findLastSetBit words)

        let words = [| 1UL; 0UL; 1UL <<< 63 |]
        Assert.Equal(Some 0, BitOperations.findFirstSetBit words)
        Assert.Equal(Some (128 + 63), BitOperations.findLastSetBit words)

module BitPackedArrayTests =

    [<Fact>]
    let ``BitPackedArray construction and basic operations`` () =
        let arr = BitPackedArray(100)

        Assert.Equal(100, arr.Length)
        Assert.Equal(2, arr.WordCount)  // ceiling(100/64) = 2

        Assert.False(arr.[0])
        Assert.False(arr.[99])

        arr.[0] <- true
        arr.[99] <- true

        Assert.True(arr.[0])
        Assert.True(arr.[99])
        Assert.False(arr.[50])

    [<Fact>]
    let ``BitPackedArray bounds checking`` () =
        let arr = BitPackedArray(10)

        Assert.Throws<IndexOutOfRangeException>(fun () -> arr.[-1] |> ignore) |> ignore
        Assert.Throws<IndexOutOfRangeException>(fun () -> arr.[10] |> ignore) |> ignore
        Assert.Throws<IndexOutOfRangeException>(fun () -> arr.[-1] <- true) |> ignore
        Assert.Throws<IndexOutOfRangeException>(fun () -> arr.[10] <- true) |> ignore

    [<Fact>]
    let ``BitPackedArray clear and set all`` () =
        let arr = BitPackedArray(150)

        arr.SetAll(true)
        Assert.Equal(150, arr.CountOnes())
        Assert.Equal(0, arr.CountZeros())

        arr.Clear()
        Assert.Equal(0, arr.CountOnes())
        Assert.Equal(150, arr.CountZeros())

        arr.SetAll(false)
        Assert.Equal(0, arr.CountOnes())
        Assert.Equal(150, arr.CountZeros())

    [<Fact>]
    let ``BitPackedArray handles partial last word correctly`` () =
        let arr = BitPackedArray(70)  // More than 64 but not multiple of 64

        arr.SetAll(true)
        Assert.Equal(70, arr.CountOnes())

        for i in 0 .. 69 do
            Assert.True(arr.[i])

    [<Fact>]
    let ``BitPackedArray clone is independent`` () =
        let arr1 = BitPackedArray(100)
        arr1.[10] <- true
        arr1.[20] <- true

        let arr2 = arr1.Clone()

        Assert.True(arr2.[10])
        Assert.True(arr2.[20])

        arr2.[10] <- false
        arr2.[30] <- true

        Assert.True(arr1.[10])   // Original unchanged
        Assert.False(arr1.[30])  // Original unchanged
        Assert.False(arr2.[10])  // Copy changed
        Assert.True(arr2.[30])   // Copy changed