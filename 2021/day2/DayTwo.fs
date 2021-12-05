module DayTwo

open System

let convertToXy(bar: string) = 
    match bar with 
    | "forward" -> "x"
    | "up" -> "y"
    | "down" -> "y"

let convertToValue(bar: string, baz: int) =
    match bar with
    | "forward" -> baz
    | "up" -> -1 * baz
    | "down" -> baz


let parseDirection (direction: string array) = (
    convertToXy(direction.[0]), 
    convertToValue(direction.[0], System.Int32.Parse(direction.[1]))
)

let splitBy (text: string) = text.Split [|' '|] |> parseDirection


type DayTwo() =
    inherit Base.Day()
    override u.partOne(input: seq<string>) =
        let depths = input |> Seq.map splitBy

        let mutable x = 0
        let mutable y = 0
        for (xy,value) in depths do
            match xy with
            | "x" -> 
                x <- x + value
            | "y" -> 
                y <- y + value
            | _ -> printf "%s" "oops"

        (x * y)

    override u.partTwo(input: seq<string>) =
        let depths = input |> Seq.map splitBy

        // TODO: fold or reduce to make this a one liner
        let mutable x = 0
        let mutable y = 0
        let mutable aim = 0
        for (xy,value) in depths do
            match xy with
            | "x" -> 
                x <- x + value
                y <- y + (aim * value)
            | "y" -> 
                // y <- y + value
                aim <- aim + value
            | _ -> printf "%s" "oops"

        (x * y)





