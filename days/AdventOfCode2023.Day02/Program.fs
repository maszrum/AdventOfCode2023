open System
open System.IO

// part 1

type ShownCubes =
    { Red: Option<int>
      Green: Option<int>
      Blue: Option<int> }

type Game =
    { Number: int
      ShownCubes: ShownCubes[] }

let readInputLines (streamReader: StreamReader) =
    seq {
        while not streamReader.EndOfStream do
            yield streamReader.ReadLine()
    }

let parseInputLine (line: string) =
    let indexOfColon = line.IndexOf(':')
    let gameNumber = Int32.Parse(line.Substring(5, indexOfColon - 5))
    let shownCubesSerialized = line.Substring(indexOfColon + 2)

    let shownCubes =
        shownCubesSerialized.Split(';', StringSplitOptions.RemoveEmptyEntries + StringSplitOptions.TrimEntries)
        |> Seq.map (fun set -> set.Split(',', StringSplitOptions.RemoveEmptyEntries + StringSplitOptions.TrimEntries))
        |> Seq.map (fun set ->
            let parseElement (acc: ShownCubes) (value: string) =
                let indexOfSpace = value.IndexOf(' ')
                let number = Int32.Parse(value.Substring(0, indexOfSpace))

                if value.EndsWith("red") then
                    { acc with Red = Some(number) }
                else if value.EndsWith("green") then
                    { acc with Green = Some(number) }
                else if value.EndsWith("blue") then
                    { acc with Blue = Some(number) }
                else
                    failwith "Invalid input"

            let initialAccumulator =
                { Red = None
                  Green = None
                  Blue = None }

            Seq.scan parseElement initialAccumulator set |> Seq.last)
        |> Seq.toArray

    { Number = gameNumber
      ShownCubes = shownCubes }

let isGamePossible (game: Game) =
    let isNoneOrLEThan (value: Option<int>, leThan: int) =
        match value with
        | None -> true
        | Some value -> value <= leThan

    game.ShownCubes
    |> Seq.forall (fun cubes ->
        isNoneOrLEThan (cubes.Red, 12)
        && isNoneOrLEThan (cubes.Green, 13)
        && isNoneOrLEThan (cubes.Blue, 14))

let parsedGames =
    use fileStream = File.OpenRead("input.txt")
    use streamReader = new StreamReader(fileStream)
    readInputLines streamReader |> Seq.map parseInputLine |> Seq.toArray

let resultPartOne =
    parsedGames |> Seq.filter isGamePossible |> Seq.sumBy (fun game -> game.Number)

printfn $"Part 1 result: %d{resultPartOne}"

// part 2

let createBiggestPossibleGame (game: Game) =
    let initialAccumulator =
        { Red = Some(0)
          Green = Some(0)
          Blue = Some(0) }

    let createBiggest (acc: ShownCubes) (value: ShownCubes) =
        let acc =
            if (defaultArg value.Red 0) > acc.Red.Value then
                { acc with Red = value.Red }
            else
                acc

        let acc =
            if (defaultArg value.Green 0) > acc.Green.Value then
                { acc with Green = value.Green }
            else
                acc

        let acc =
            if (defaultArg value.Blue 0) > acc.Blue.Value then
                { acc with Blue = value.Blue }
            else
                acc

        acc

    Seq.scan createBiggest initialAccumulator game.ShownCubes |> Seq.last

let resultPartTwo =
    parsedGames
    |> Seq.map createBiggestPossibleGame
    |> Seq.map (fun game -> (defaultArg game.Red 0) * (defaultArg game.Green 0) * (defaultArg game.Blue 0))
    |> Seq.sum

printfn $"Part 2 result: %d{resultPartTwo}"
