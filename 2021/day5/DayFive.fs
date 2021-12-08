module DayFive

open System.Linq

type Point =
    struct
        val x: int
        val y: int
        member this.ToString =
            sprintf "%d,%d" this.x this.y
        new (point: string) = { x = System.Int32.Parse(point.Split(",").First()); y = System.Int32.Parse(point.Split(",").Last()) }
        new (X: int, Y: int) = { x = X; y = Y}
    end

let generateRange (start: int, finish: int) =
    if start < finish then
        seq {start..finish} |> Seq.toArray
    else
        seq {finish..start} |> Seq.toArray

type Line(start: Point, finish: Point) =
    member val start = start
    member val finish = finish

    member this.IsHorizontal =
        start.x = finish.x

    member this.IsDiagonal =
        start.x = finish.x || start.y = finish.y

    member this.Points = 
        let mutable points = Seq.empty<Point>

        if not this.IsDiagonal then
            points
        else
            if this.IsHorizontal then
                let range = generateRange(start.y, finish.y)

                // TODO: how could i pipe this to generate the points?
                for entry in range do
                    points <- points
                        |> Seq.append [ Point(start.x, entry) ]
            else
                let range = generateRange(start.x, finish.x)

                for entry in range do
                    points <- points
                        |> Seq.append [ Point(entry, start.y )]

            points

    new(line: string) =
        Line(Point(line.Split(" -> ").First()), Point(line.Split(" -> ").Last()))

let foo = Line("0,9 -> 5,9").Points |> Seq.map (fun point -> point.ToString) |> Seq.toArray
let baz = Line("9,7 -> 7,7").Points |> Seq.map (fun point -> point.ToString) |> Seq.toArray
let bar = seq { 1..2} |> Seq.toArray

let parseInput (input: seq<string>) =
    input
    |> Seq.map Line


type DayFive() =
    inherit Base.Day()
    override u.partOne(input: seq<string>) =
        let lines = input |> parseInput
        let mutable crossings = Map.empty<Point, bool>

        for line in lines do
            let points = line.Points
            for point in points do
                if crossings.ContainsKey(point) then
                    crossings <- crossings.Add(point, true)
                else
                    crossings <- crossings.Add(point, false)

        crossings.Values
        |> Seq.cast<bool> 
        |> Seq.filter (fun x -> x = true)
        |> Seq.length
    override u.partTwo(input: seq<string>) =
        0