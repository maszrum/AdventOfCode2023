open System
open System.IO
open Microsoft.FSharp.Collections

// part 1

type LocatedNumber =
    { Number: int
      LineNumber: int
      ColumnFrom: int
      ColumnTo: int }

type LocatedSymbol =
    { Symbol: char
      LineNumber: int
      ColumnNumber: int }

let readInputLines (streamReader: StreamReader) =
    seq {
        let mutable i = 0

        while not streamReader.EndOfStream do
            yield (streamReader.ReadLine(), i)
            i <- i + 1
    }

let parseSymbols (line: string, lineNumber: int) =
    let getSymbolIndexes (line: string) =
        seq {
            for i in 0 .. line.Length - 1 do
                if Char.IsDigit(line[i]) = false && line[i] <> '.' then
                    yield
                        { Symbol = line[i]
                          LineNumber = lineNumber
                          ColumnNumber = i }
        }

    (lineNumber, getSymbolIndexes line |> Seq.toArray)

let parseNumbers (line: string, lineNumber: int) =
    let mutable value = 0
    let mutable fromIndex = Option<int>.None

    seq {
        for i in 0 .. line.Length - 1 do
            let index = line.Length - 1 - i

            if Char.IsDigit(line[index]) then
                if fromIndex.IsNone then
                    fromIndex <- Some(index)

                let digit = int (line[index] - '0')
                value <- value + (digit * pown 10 (fromIndex.Value - index))
            else if fromIndex.IsSome then
                yield
                    { Number = value
                      LineNumber = lineNumber
                      ColumnFrom = index + 1
                      ColumnTo = fromIndex.Value }

                value <- 0
                fromIndex <- None

        if fromIndex.IsSome then
            yield
                { Number = value
                  LineNumber = lineNumber
                  ColumnFrom = 0
                  ColumnTo = fromIndex.Value }
    }

let isAdjacentToSymbol (number: LocatedNumber, symbols: LocatedSymbol seq) =
    symbols
    |> Seq.exists (fun symbol ->
        symbol.ColumnNumber >= (number.ColumnFrom - 1)
        && symbol.ColumnNumber <= (number.ColumnTo + 1))

let symbolLocations =
    use fileStream = File.OpenRead("input.txt")
    use streamReader = new StreamReader(fileStream)
    readInputLines streamReader |> Seq.map parseSymbols |> Map

let getSymbolsNearLine (lineNumber: int) =
    let top = symbolLocations.TryFind(lineNumber - 1) |> Option.defaultValue Array.empty
    let middle = symbolLocations.TryFind(lineNumber) |> Option.defaultValue Array.empty
    let bottom = symbolLocations.TryFind(lineNumber + 1) |> Option.defaultValue Array.empty

    Seq.append top (Seq.append middle bottom)

let parsedNumbers =
    use fileStream = File.OpenRead("input.txt")
    use streamReader = new StreamReader(fileStream)

    readInputLines streamReader
    |> Seq.collect parseNumbers
    |> Seq.toArray

let resultPartOne =
    parsedNumbers
    |> Seq.map (fun number -> (number, getSymbolsNearLine(number.LineNumber)))
    |> Seq.filter isAdjacentToSymbol
    |> Seq.map fst
    |> Seq.toArray
    |> Seq.sumBy (fun number -> number.Number)

printfn $"Part 1 result: %d{resultPartOne}"

// part 2

let isGearCandidate (number: LocatedNumber, symbol: LocatedSymbol) =
    symbol.Symbol = '*'
    && symbol.ColumnNumber >= (number.ColumnFrom - 1)
    && symbol.ColumnNumber <= (number.ColumnTo + 1)

let resultPartTwo =
    parsedNumbers
    |> Seq.map (fun number -> (number, getSymbolsNearLine(number.LineNumber)))
    |> Seq.collect (fun (number, symbol) -> Seq.map (fun b -> (number, b)) symbol)
    |> Seq.filter isGearCandidate
    |> Seq.groupBy snd
    |> Seq.map snd
    |> Seq.map(fun pairs -> pairs |> Seq.toArray)
    |> Seq.filter(fun pairs -> pairs |> Array.length = 2)
    |> Seq.map(fun a -> (a[0], a[1]))
    |> Seq.map(fun ((l1, _), (l2, _)) -> l1.Number * l2.Number)
    |> Seq.sum

printfn $"Part 2 result: %d{resultPartTwo}"
