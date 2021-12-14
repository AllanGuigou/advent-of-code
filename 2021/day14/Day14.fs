module Day14

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

        let mutable letters = Map.empty<string, int>

        let increment (letters: Map<string, int>, char: string) =
            letters.Add(
                char,
                if letters.ContainsKey(char) then
                    letters.[char] + 1
                else
                    1
            )


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

    override this.partTwo(input: seq<string>) : int = failwith "Not Implemented"
