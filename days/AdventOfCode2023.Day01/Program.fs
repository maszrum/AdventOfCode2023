open System
open System.IO

// part 1

let readInputLines (streamReader: StreamReader) =
    seq {
        while not streamReader.EndOfStream do
            yield streamReader.ReadLine()
    }

let findFirstDigit input =
    input
    |> Seq.tryFind Char.IsDigit
    |> Option.map (fun character -> int(character - '0'))
    |> Option.defaultValue 0

let findLastDigit input =
    input
    |> Seq.rev
    |> findFirstDigit

let createNumberFromTwoDigits (d1, d2) =
    (d1 * 10) + d2

let resultPartOne =
    use fileStream = File.OpenRead("input.txt")
    use streamReader = new StreamReader(fileStream)

    readInputLines streamReader
    |> Seq.map(fun line -> (findFirstDigit line, findLastDigit line))
    |> Seq.map createNumberFromTwoDigits
    |> Seq.sum

printfn $"Part 1 result: %d{resultPartOne}"

// part 2

let textualDigitToNumberLength3 (input: string, fromEnd: bool) =
    if input.Length < 3 then
        None
    else
        let substring = if fromEnd then input.Substring(input.Length - 3) else input.Substring(0, 3);
        match substring with
        | "one" -> Some 1
        | "two" -> Some 2
        | "six" -> Some 6
        | _ -> None

let textualDigitToNumberLength4 (input: string, fromEnd: bool) =
    if input.Length < 4 then
        None
    else
        let substring = if fromEnd then input.Substring(input.Length - 4) else input.Substring(0, 4)
        match substring with
        | "four" -> Some 4
        | "five" -> Some 5
        | "nine" -> Some 9
        | _ -> None

let textualDigitToNumberLength5 (input: string, fromEnd: bool) =
    if input.Length < 5 then
        None
    else
        let substring = if fromEnd then input.Substring(input.Length - 5) else input.Substring(0, 5);
        match substring with
        | "three" -> Some 3
        | "seven" -> Some 7
        | "eight" -> Some 8
        | _ -> None

let digitToNumber (input: string, fromEnd: bool) =
    let character = if fromEnd then input[input.Length - 1] else input[0];
    match Char.IsDigit(character) with
    | true -> Some (int(character - '0'))
    | false -> None

let rec readDigitFromString (input: string, fromEnd: bool) =
    match digitToNumber(input, fromEnd) with
    | Some value -> value
    | None ->
        match textualDigitToNumberLength3(input, fromEnd) with
        | Some value -> value
        | None ->
            match textualDigitToNumberLength4(input, fromEnd) with
            | Some value -> value
            | None ->
                match textualDigitToNumberLength5(input, fromEnd) with
                | Some value -> value
                | None ->
                    let substring = if fromEnd then input.Substring(0, input.Length - 1) else input.Substring(1)
                    readDigitFromString(substring, fromEnd)

let resultPartTwo =
    use fileStream = File.OpenRead("input.txt")
    use streamReader = new StreamReader(fileStream)

    readInputLines streamReader
    |> Seq.map(fun line -> (readDigitFromString(line, false), readDigitFromString(line, true)))
    |> Seq.map(createNumberFromTwoDigits)
    |> Seq.sum

printfn $"Part 2 result: %d{resultPartTwo}"
