module Day15

open FSharpx.Collections
open FSharpx.Collections

let max = 2147483647


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
    |> Seq.filter (fun air -> snd air >= 0 && snd air <> max)

let repeatIndex (repeat: int, rows: int, columns: int) (indexRisk: (int * int) * int) =
    let (index, risk) = indexRisk
    let (row, column) = index

    seq {
        for rr = 0 to repeat - 1 do
            for cc = 0 to repeat - 1 do
                let risk = (risk + cc + rr)

                let scaledRisk = (risk - 1) % 9 + 1

                printf "%d" scaledRisk

                ((row + (rr * rows), column + (cc * columns)), scaledRisk)

            printfn ""
    }
    |> Seq.toArray

repeatIndex (5, 15, 15) ((0, 0), 2)

let parseRisks (input: seq<string>, repeat: int) =
    let rows = input |> Seq.length

    let columns =
        input |> Seq.head |> Seq.toArray |> Array.length

    let repeatIndex = repeatIndex (repeat, rows, columns)

    let risks =
        input
        |> Seq.collect Seq.toArray
        |> Seq.mapi (fun i risk -> ((i / columns, i % columns), System.Int32.Parse <| risk.ToString()))

    if repeat > 1 then
        (risks |> Seq.collect repeatIndex |> Map.ofSeq, rows * repeat, columns * repeat)
    else
        (risks |> Map.ofSeq, rows, columns)

let bellmanFord (risks: Map<int * int, int>) =
    let mutable dist: Map<int * int, int> =
        risks
        |> Map.map (fun index risk -> max)
        |> Map.add (0, 0) 0

    for index in risks.Keys do
        let adjacent = adjacentIndexRisk (index, dist)

        for a in adjacent do

            if dist.[index] + snd a < dist.[fst a] then
                dist <-
                    dist
                    |> Map.add (fst a) (dist.[index] + risks.[fst a])

    dist


let djikstra (risks: Map<int * int, int>, rows: int, columns: int) =
    let mutable bestAt = Map.empty<(int * int), int>
    let mutable pq = Heap.empty false

    pq <- pq |> Heap.insert (0, (0, 0))
    bestAt <- bestAt |> Map.add (0, 0) 0

    while pq.Length > 0 do
        let (head, tail) = pq |> Heap.uncons
        pq <- tail
        let (cost, coord) = head

        // iterate over all adjacent index's updating their bestAt cost if it is lower
        for air in adjacentIndexRisk (coord, risks) do
            let (ai, r) = air

            let aiCostOrMax =
                if bestAt |> Map.containsKey ai then
                    cost
                else
                    max

            // if there is a shorter path to ai from coord
            if aiCostOrMax > cost + r then
                bestAt <- bestAt |> Map.add ai (cost + r)
                pq <- pq |> Heap.insert (cost + r, ai)


    bestAt.[(rows - 1, columns - 1)]


type Day15() =
    inherit Base.Day()

    override this.partOne(input: seq<string>) : int =
        let (risks, rows, columns) = parseRisks (input, 1)

        let dist = bellmanFord (risks)

        dist.[(rows - 1, columns - 1)]

    override this.partTwo(input: seq<string>) : int =
        let (risks, rows, columns) = parseRisks (input, 5)

        // my bellmanFord algorithm gave me the incorrect answer, off by 3
        // let dist = bellmanFord (risks)
        // dist.[(rows - 1, columns - 1)]
        djikstra (risks, rows, columns)
