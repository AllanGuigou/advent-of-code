module Day12

let parseCaveConnection (line: string) =
    let caves = line.Split('-') |> Array.toSeq

    let forward = (caves |> Seq.head, caves |> Seq.last)
    let backward = (snd forward, fst forward)

    seq {
        yield forward
        if snd backward <> "start" && fst backward <> "end" then
            yield backward
    }

printfn "%A" <| parseCaveConnection("start-A")

let pop (stack: string list list) =
    stack |> List.removeAt 0

let pathToString (path: List<string>) =
    path
    |> List.reduce (fun accum cave -> sprintf "%s,%s" accum cave)

let shouldVisitCave (path: string list, cave: string) =
    if cave <> cave.ToUpper() then
        not (path |> List.contains cave)
    else
        true

List.append <| ["bar"]

["bar" ] |> List.append ["foo"]

type Day12() =
    inherit Base.Day()
    override this.partOne(input: seq<string>): int = 
        let caveSystem =
            input
            |> Seq.collect parseCaveConnection
            |> Seq.groupBy (fun connection -> fst connection)
            |> Map.ofSeq
            |> Map.map (fun cave connections -> connections |> Seq.map (fun connection -> snd connection) |> Seq.toList)

        // BFS using "queue"
        let mutable cavesToVisit: string list list = [ ["start"] ]
        let mutable paths = 0 

        while cavesToVisit |> List.length > 0 do
            let current = cavesToVisit.Head
            cavesToVisit <- pop cavesToVisit

            if current |> List.last = "end" then
                paths <- paths + 1
                printfn "%s" (pathToString current)
            else
                for cave in caveSystem.[current |> List.last] do
                    if shouldVisitCave (current, cave) then
                        cavesToVisit <- cavesToVisit @ [current @ [cave]]

        paths

    override this.partTwo(input: seq<string>): int = 
        failwith "Not Implemented"
