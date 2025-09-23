// 2-BitPacking.fs
// ビット操作基盤層

namespace E8.BitPacking

open System
open System.Runtime.CompilerServices
open System.Numerics

/// ビット操作の基本演算
module BitOperations =

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline getWordIndex (bitIndex: int) =
        bitIndex >>> 6  // bitIndex / 64

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline getBitOffset (bitIndex: int) =
        bitIndex &&& 63  // bitIndex % 64

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline computeWordCount (totalBits: int) =
        (totalBits + 63) >>> 6  // ceiling(totalBits / 64)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline setBit (words: uint64[]) (bitIndex: int) (value: bool) =
        let wordIdx = getWordIndex bitIndex
        let bitOffset = getBitOffset bitIndex
        let mask = 1UL <<< bitOffset
        if value then
            words.[wordIdx] <- words.[wordIdx] ||| mask
        else
            words.[wordIdx] <- words.[wordIdx] &&& ~~~mask

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline getBit (words: uint64[]) (bitIndex: int) =
        let wordIdx = getWordIndex bitIndex
        let bitOffset = getBitOffset bitIndex
        (words.[wordIdx] >>> bitOffset) &&& 1UL <> 0UL

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline toggleBit (words: uint64[]) (bitIndex: int) =
        let wordIdx = getWordIndex bitIndex
        let bitOffset = getBitOffset bitIndex
        let mask = 1UL <<< bitOffset
        words.[wordIdx] <- words.[wordIdx] ^^^ mask

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline clearBit (words: uint64[]) (bitIndex: int) =
        let wordIdx = getWordIndex bitIndex
        let bitOffset = getBitOffset bitIndex
        let mask = ~~~(1UL <<< bitOffset)
        words.[wordIdx] <- words.[wordIdx] &&& mask

    /// Count number of set bits (population count)
    let countSetBits (words: uint64[]) =
        let mutable count = 0
        for word in words do
            count <- count + BitOperations.PopCount(word)
        count

    /// Find first set bit
    let findFirstSetBit (words: uint64[]) =
        for i in 0 .. words.Length - 1 do
            if words.[i] <> 0UL then
                let trailing = BitOperations.TrailingZeroCount(words.[i])
                return Some (i * 64 + trailing)
        None

    /// Find last set bit
    let findLastSetBit (words: uint64[]) =
        for i in words.Length - 1 downto 0 do
            if words.[i] <> 0UL then
                let leading = BitOperations.LeadingZeroCount(words.[i])
                return Some (i * 64 + (63 - leading))
        None

/// ビットパック配列
[<Struct>]
type BitPackedArray =
    val Data: uint64[]
    val Length: int
    val WordCount: int

    new(length: int) =
        let wordCount = BitOperations.computeWordCount length
        {
            Data = Array.zeroCreate<uint64> wordCount
            Length = length
            WordCount = wordCount
        }

    member x.Item
        with get(index: int) =
            if index < 0 || index >= x.Length then
                raise (IndexOutOfRangeException($"Index {index} out of range [0, {x.Length})"))
            BitOperations.getBit x.Data index
        and set(index: int) (value: bool) =
            if index < 0 || index >= x.Length then
                raise (IndexOutOfRangeException($"Index {index} out of range [0, {x.Length})"))
            BitOperations.setBit x.Data index value

    member x.Clear() =
        Array.Clear(x.Data, 0, x.WordCount)

    member x.SetAll(value: bool) =
        let fill = if value then UInt64.MaxValue else 0UL
        for i in 0 .. x.WordCount - 1 do
            x.Data.[i] <- fill
        // Handle partial last word
        if x.Length % 64 <> 0 then
            let lastBits = x.Length % 64
            let mask = (1UL <<< lastBits) - 1UL
            if value then
                x.Data.[x.WordCount - 1] <- mask
            else
                x.Data.[x.WordCount - 1] <- 0UL

    member x.CountOnes() =
        BitOperations.countSetBits x.Data

    member x.CountZeros() =
        x.Length - x.CountOnes()

    member x.Clone() =
        let copy = BitPackedArray(x.Length)
        Array.Copy(x.Data, copy.Data, x.WordCount)
        copy