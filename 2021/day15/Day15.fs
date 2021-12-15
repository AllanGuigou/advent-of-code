module Day15

let print (rows: int, columns: int) (risks: Map<int * int, int>) =
    for row = 0 to rows - 1 do
        for column = 0 to columns - 1 do
            printf "%d" risks.[(row, column)]

        printfn ""

let adjacent (index: int * int) =
    let (row, column) = index

    seq {
        yield (row, column - 1)
        yield (row - 1, column)
        yield (row + 1, column)
        yield (row, column + 1)
    }

let indexRisk (index: int * int, risks: Map<int * int, int>) =
    if risks.ContainsKey(index) then
        risks.[index]
    else
        -1

let adjacentIndexRisk (index: int * int, risks: Map<int * int, int>) =
    adjacent index
    |> Seq.map (fun ai -> ai, indexRisk (ai, risks))
    |> Seq.filter (fun air -> snd air >= 0 && snd air <> 2147483647)


type Day15() =
    inherit Base.Day()

    override this.partOne(input: seq<string>) : int =
        let rows = input |> Seq.length

        let columns =
            input |> Seq.head |> Seq.toArray |> Array.length

        let print = print (rows, columns)

        let risks =
            input
            |> Seq.collect Seq.toArray
            |> Seq.mapi (fun i risk -> ((i / columns, i % columns), System.Int32.Parse <| risk.ToString()))
            |> Map.ofSeq


        let max: int = 2147483647

        let mutable dist: Map<int * int, int> =
            risks
            |> Map.map (fun index risk -> max)
            |> Map.add (0, 0) 0

        for index in risks.Keys do
            let adjacent = adjacentIndexRisk (index, risks)

            for a in adjacent do
                if dist.[index] + snd a < dist.[fst a] then
                    dist <- dist |> Map.add (fst a) (dist.[index] + snd a)

        dist.[(rows - 1, columns - 1)]

    override this.partTwo(input: seq<string>) : int = failwith "Not Implemented"
