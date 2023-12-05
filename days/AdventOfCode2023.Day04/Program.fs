open System
open System.IO

// part 1

type Card =
    { Number: int
      WinningNumbers: Set<int>
      OwningNumbers: Set<int> }

let readInputLines (streamReader: StreamReader) =
    seq {
        let mutable i = 0

        while not streamReader.EndOfStream do
            yield (streamReader.ReadLine(), i)
            i <- i + 1
    }

let parseCard (line: string, lineNumber: int) =
    let line = line.Substring(10)
    let indexOfSeparator = line.IndexOf('|')

    let winningNumbers =
        line.Substring(0, indexOfSeparator).Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map Int32.Parse
        |> Set.ofSeq

    let owningNumbers =
        line.Substring(indexOfSeparator + 1).Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map Int32.Parse
        |> Set.ofSeq

    { Number = lineNumber
      WinningNumbers = winningNumbers
      OwningNumbers = owningNumbers }

let findWinningNumbersCount (card: Card) =
    Set.intersect card.WinningNumbers card.OwningNumbers |> Set.count

let getPointsFromWinningNumbersCount (winCardsCount: int) =
    if winCardsCount = 0 then
        0
    else
        pown 2 (winCardsCount - 1)

let parsedCards =
    use fileStream = File.OpenRead("input.txt")
    use streamReader = new StreamReader(fileStream)

    readInputLines streamReader
    |> Seq.map parseCard
    |> Seq.toArray

let resultPartOne =
    parsedCards
    |> Seq.map findWinningNumbersCount
    |> Seq.map getPointsFromWinningNumbersCount
    |> Seq.sum

printfn $"Part 1 result: %d{resultPartOne}"

// part 2

type CardsCount = int64 array

let createAccumulator (firstWinningNumbersCount: int) : CardsCount =
    Array.create firstWinningNumbersCount 1L

let updateCardsCountAndGetNextCardCount (cardsCount: CardsCount) (winningNumbersCount: int) =
    let mutable incrementNumbers = winningNumbersCount
    let popValue = ((Array.tryHead cardsCount) |> Option.defaultValue 0L) + 1L

    let newAccumulatorPart1 =
        cardsCount
        |> Seq.skip (min cardsCount.Length 1)
        |> Seq.map(fun x ->
            incrementNumbers <- incrementNumbers - 1
            if incrementNumbers >= 0 then x + popValue else x)
        |> Seq.toArray

    let newAccumulatorPart2 = Array.create (max 0 incrementNumbers) popValue

    let newAccumulator: CardsCount = Seq.append newAccumulatorPart1 newAccumulatorPart2 |> Seq.toArray

    (newAccumulator, popValue)

let scanCards (acc: CardsCount * int64) (cardWinningNumbersCount: int) =
    let accumulator, totalCards = acc
    let newAccumulator, popValue = updateCardsCountAndGetNextCardCount accumulator cardWinningNumbersCount
    let newTotalCards = totalCards + popValue

    (newAccumulator, newTotalCards)

let resultPartTwo =
    parsedCards
    |> Seq.skip 1
    |> Seq.map findWinningNumbersCount
    |> Seq.scan scanCards (createAccumulator(findWinningNumbersCount(parsedCards[0])), 1L)
    |> Seq.last
    |> snd

printfn $"Part 2 result: %d{resultPartTwo}"
