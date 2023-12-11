open System
open System.IO
open Microsoft.FSharp.Collections

// part 1

type Range =
    { From: int64
      To: int64 }

type ConversionRange =
    { Source: Range
      Destination: Range }

type ConversionMap =
    { Ranges: ConversionRange array }

let defaultRange =
    { Source = { From = 0; To = Int32.MaxValue }
      Destination = { From = 0; To = Int32.MaxValue } }

let isInRange (value: int64) (range: Range) =
    value >= range.From && value <= range.To

let mapValue (value: int64) (range: ConversionRange) =
    (value - range.Source.From) + range.Destination.From

let findConversionAndMapValue (map: ConversionMap) (value: int64) =
    map.Ranges
    |> Seq.tryFind (fun range -> isInRange value range.Source)
    |> Option.defaultValue defaultRange
    |> mapValue value

let readInputLines (streamReader: StreamReader) =
    seq {
        while not streamReader.EndOfStream do
            yield streamReader.ReadLine()
    }

let parseSeeds (inputLine: string) =
    (inputLine.Substring 7).Split ' ' |> Seq.map Int64.Parse

let chunkBy (predicate: 'a -> bool) (source: 'a seq) : 'a list list =
    let rec chunkAcc acc currentChunk = function
        | [] -> acc @ [currentChunk]
        | x::xs when predicate x && List.isEmpty currentChunk ->
            chunkAcc acc [x] xs
        | x::xs when predicate x ->
            chunkAcc (acc @ [currentChunk]) [x] xs
        | x::xs ->
            chunkAcc acc (currentChunk @ [x]) xs

    chunkAcc [] [] (Seq.toList source)

let parseMaps (inputLines: string seq) =
    let isNotEmpty (line: string) = line |> String.length > 0
    let parseRange (line: string) =
        let numbers = line.Split ' ' |> Seq.map Int64.Parse |> List.ofSeq
        match numbers with
        | destinationStart :: sourceFrom :: rangeLength :: _ ->
                  { Source = { From = sourceFrom; To = sourceFrom + rangeLength }
                    Destination = { From = destinationStart; To = destinationStart + rangeLength } }
        | _ -> failwith "Invalid input"

    inputLines
    |> Seq.skip 1
    |> Seq.filter isNotEmpty
    |> chunkBy (fun line -> not (Char.IsNumber line[0]))
    |> Seq.map (fun lines -> lines |> Seq.skip 1 |> Seq.map parseRange |> Seq.toArray)
    |> Seq.map (fun ranges -> { Ranges = ranges })

let parsedSeeds =
    use fileStream = File.OpenRead("input2.txt")
    use streamReader = new StreamReader(fileStream)

    readInputLines streamReader
    |> Seq.head
    |> parseSeeds
    |> Seq.toArray

let parsedMaps =
    use fileStream = File.OpenRead("input2.txt")
    use streamReader = new StreamReader(fileStream)

    readInputLines streamReader
    |> parseMaps
    |> Seq.toArray

let applyMappings mappings x =
    Seq.fold (fun acc mapping -> findConversionAndMapValue mapping acc) x mappings

let resultPartOne =
    parsedSeeds
    |> Seq.map (applyMappings parsedMaps)
    |> Seq.min

printfn $"Part 1 result: %d{resultPartOne}"

// part 2

let intoPairs (sequence: 'a seq) : ('a * 'a) seq =
    let rec createPairs pairs remainingSequence =
        match (remainingSequence |> Seq.tryHead, remainingSequence |> Seq.tryItem 1) with
        | None, None -> pairs
        | Some(p1), Some(p2) -> createPairs (pairs |> Seq.append (seq [(p1, p2)])) (remainingSequence |> Seq.skip 2)
        | _ -> failwith "Invalid input"

    sequence
    |> (createPairs Seq.empty)
    |> Seq.rev

let seedRanges =
    parsedSeeds
    |> intoPairs
    |> Seq.map (fun (from, length) -> { From = from; To = from + length })
    |> Seq.toArray

let mapRange (range: Range) (conversionRange: ConversionRange) =
    { From = mapValue range.From conversionRange
      To = mapValue range.To conversionRange }

let mapValuesRange (values: Range seq) (map: ConversionMap) =

    let mapValuesSingleRange (values: Range) (range: ConversionRange) =
        let isLeftValueInside = isInRange values.From range.Source
        let isRightValueInside = isInRange values.To range.Source
        let isLeftRangeInside = isInRange range.Source.From values
        let isRightRangeInside = isInRange range.Source.To values

        match (isLeftValueInside, isRightValueInside, isLeftRangeInside, isRightRangeInside) with
        // value ---==========---
        // range -----======-----
        // in    -----======-----
        // out   ---==------==---
        | false, false, true, true ->
            let inRange = range.Source
            let outRange1 = { values with To = range.Source.From }
            let outRange2 = { values with From = range.Source.To }

            (seq [mapRange inRange range], seq [outRange1; outRange2])
        // value -------=======--
        // range --===-----------
        // in    ----------------
        // out   -------=======--
        | false, false, _, _ ->
            (Seq.empty, seq [values])
        // value -----======-----
        // range ---==========---
        // in    -----======-----
        // out   ----------------

        // value ---==========---
        // range ---==========---
        // in    ---==========---
        // out   ----------------

        // value ---=======------
        // range ---==========---
        // in    ---=======------
        // out   ----------------

        // value ------======----
        // range ---=========----
        // in    ------======----
        // out   ----------------
        | true, true, _, _ ->
            (seq [mapRange values range], Seq.empty)
        // value ---==========---
        // range ------=======---
        // in    ------=======---
        // out   ---===----------

        // value ---=========----
        // range --------======--
        // in    --------====----
        // out   ---=====--------
        | false, true, _, _ ->
            let inRange = { values with From = range.Source.From }
            let outRange = { values with To = range.Source.From }

            (seq [mapRange inRange range], seq [outRange])
        // value ---==========---
        // range ---=======------
        // in    ---=======------
        // out   ----------===---

        // value ----==========--
        // range --======--------
        // in    ----====--------
        // out   --------======--
        | true, false, _, _ ->
            let inRange = { values with To = range.Source.To }
            let outRange = { values with From = range.Source.To }

            (seq [mapRange inRange range], seq [outRange])

    let rec pipeValuesThroughAllRanges (remainingRanges: ConversionRange list) (values: Range) =
        match remainingRanges with
        | [] -> seq [values]
        | x::xs ->
            let inner, outer = mapValuesSingleRange values x
            Seq.append inner (outer |> Seq.collect (pipeValuesThroughAllRanges xs))

    let mapValuesRange (valuesToMap: Range seq) (conversionRanges: ConversionRange list) =
        valuesToMap
        |> Seq.map (pipeValuesThroughAllRanges conversionRanges)
        |> Seq.collect id

    mapValuesRange values (Array.toList map.Ranges)

let resultPartTwoRange =
    parsedMaps
    |> Seq.fold mapValuesRange seedRanges
    |> Seq.toArray
    |> Seq.minBy (fun values -> values.From)

let resultPartTwo = resultPartTwoRange.From

printfn $"Part 2 result: %d{resultPartTwo}"
