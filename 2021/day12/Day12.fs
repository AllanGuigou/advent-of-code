module Day12

let parseCaveConnection (line: string) =
    let caves = line.Split('-') |> Array.toSeq

    let forward = (caves |> Seq.head, caves |> Seq.last)
    let backward = (snd forward, fst forward)

    seq {
        if snd forward <> "start" && fst forward <> "end" then
            yield forward
        else
            printfn "forward %A" forward
        if snd backward <> "start" && fst backward <> "end" then
            yield backward
        else
            printfn "backward %A" backward 
    }

printfn "%A" <| parseCaveConnection("start-A")

let pop (stack: string list list) =
    stack |> List.removeAt 0

let pathToString (path: List<string>) =
    path
    |> List.reduce (fun accum cave -> sprintf "%s,%s" accum cave)

let pathDoesNotIncludeLowerCaseCaveTwice (path: string list) =
    path
    |> List.filter (fun x -> x <> "start" && x <> "end" )
    |> List.filter (fun x -> x <> x.ToUpper())
    |> List.groupBy (fun x -> x)
    |> Map.ofList
    |> Map.filter (fun key value -> value |> List.length > 1)
    |> Map.count = 0


printfn "%A" (pathDoesNotIncludeLowerCaseCaveTwice ["start"; "HN"; "c"; "b"; "a"])

let shouldVisitCave (path: string list, cave: string, allowLowerCaseCaveTwice: bool) =
    if cave = cave.ToUpper() || (allowLowerCaseCaveTwice && pathDoesNotIncludeLowerCaseCaveTwice path) then
        true
    else 
        not (path |> List.contains cave)

type Day12() =
    inherit Base.Day()
    override this.partOne(input: seq<string>): int = 
        let caveSystem =
            input
            |> Seq.collect parseCaveConnection
            |> Seq.groupBy (fun connection -> fst connection)
            |> Map.ofSeq
            |> Map.map (fun cave connections -> connections |> Seq.map (fun connection -> snd connection) |> Seq.toList)


        // BFS using queue
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
                    if shouldVisitCave (current, cave, false) then
                        cavesToVisit <- cavesToVisit @ [current @ [cave]]

        paths

    override this.partTwo(input: seq<string>): int = 
        let foo = input |> Seq.collect parseCaveConnection

        let caveSystem =
            input
            |> Seq.collect parseCaveConnection
            |> Seq.groupBy (fun connection -> fst connection)
            |> Map.ofSeq
            |> Map.map (fun cave connections -> connections |> Seq.map (fun connection -> snd connection) |> Seq.toList)

        // BFS using queue
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
                    if shouldVisitCave (current, cave, true) then
                        cavesToVisit <- cavesToVisit @ [current @ [cave]]

        paths
