module Day14

let decrement (letters: Map<string, uint64>, key: string) =
    letters.Add(key, letters.[key] - uint64 (1))


let increment (letters: Map<string, uint64>, key: string) =
    letters.Add(
        key,
        if letters.ContainsKey(key) then
            letters.[key] + uint64 (1)
        else
            uint64 (1)
    )

type Day14() =
    inherit Base.Day()

    override this.partOne(input: seq<string>) : int =
        let (templates, insertions) =
            input
            |> Seq.filter (fun x -> x.Length > 0)
            |> Seq.toList
            |> List.partition (fun x -> not (x.Contains("->")))

        let insertionMap =
            insertions
            |> List.map (fun x -> x.Split(" -> "))
            |> List.map (fun x -> (x.[0], x.[1]))
            |> Map.ofList

        let steps = 10

        printfn "%A" insertionMap

        let mutable letters = Map.empty<string, uint64>

        let mutable template = templates.Head

        for char in template do
            letters <- increment (letters, char.ToString())

        for step = 0 to steps - 1 do
            let mutable next = System.Text.StringBuilder()

            for i = 0 to template.Length - 2 do
                let polymerPair = template.Substring(i, 2)
                let value = insertionMap.[polymerPair]

                letters <- increment (letters, value)

                next <- next.Append(polymerPair.Substring(0, 1).Insert(1, value))

            next <- next.Append(template.Substring(template.Length - 1, 1))
            template <- (next.ToString())

        printfn "%A" letters

        letters.Values |> Seq.max |> (-)
        <| (letters.Values |> Seq.min)
        |> int

    override this.partTwo(input: seq<string>) : int =
        // keep track of the sets of numbers there are only so many combination of those
        let (templates, insertions) =
            input
            |> Seq.filter (fun x -> x.Length > 0)
            |> Seq.toList
            |> List.partition (fun x -> not (x.Contains("->")))

        let insertionMap =
            insertions
            |> List.map (fun x -> x.Split(" -> "))
            |> List.map (fun x -> (x.[0], x.[1].[0]))
            |> Map.ofList

        let foo (pair: string) =
            let insertion = insertionMap.[pair]

            seq {
                sprintf "%c%c" pair.[0] insertion
                sprintf "%c%c" insertion pair.[1]
            }


        let mutable polymerPairs = Map.empty<string, uint64>

        for i = 0 to templates.Head.Length - 2 do
            let polymerPair = templates.Head.Substring(i, 2)
            polymerPairs <- increment (polymerPairs, polymerPair)

        let steps = 1
        printfn "%A" polymerPairs

        for i = 0 to steps - 1 do
            let mutable tmp = Map.empty<string, uint64>

            for pair in polymerPairs.Keys do
                let newPairs = foo (pair)

                for np in newPairs do
                    tmp <- increment (tmp, np)

            printfn "%A" tmp
            polymerPairs <- tmp

        let result =
            (polymerPairs
             |> Map.toSeq
             |> Seq.collect (fun kvp ->
                 seq {
                     ((fst kvp).[0], snd kvp)
                     ((fst kvp).[1], snd kvp)
                 })
             |> Seq.groupBy (fun x -> fst x)
             |> Seq.map (fun x -> (fst x, snd x |> Seq.sumBy (fun y -> snd y))))

        for kvp in result do
            printfn "%c -> %u" (fst kvp) (snd kvp)


        0
