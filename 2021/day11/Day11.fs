module Day11

let print (dumbos: Map<int * int, int>) =
    for y = 0 to 9 do
        for x = 0 to 9 do
            let energyLevel = dumbos.[(y, x)]
            printf "%s" <| if energyLevel > 9 then "*" else energyLevel.ToString()
        printfn ""

let safeIndexing (dumbos: Map<int * int, int>, index: int * int) =
    if dumbos.ContainsKey(index) then
        dumbos.[index]
    else
        -1

let incrementAll (dumbos: Map<int * int, int>) =
    dumbos
    |> Map.map (fun key value -> value + 1)

let adjacent (index: int * int) =
    let (y, x) = index
    seq {
        yield (y + 1, x + 1)
        yield (y - 1, x + 1)
        yield (y + 1, x - 1)
        yield (y - 1, x - 1)
        yield (y + 1, x)
        yield (y - 1, x)
        yield (y, x + 1)
        yield (y, x - 1)
    }

let incrementAdjacent (dumbos: Map<int * int, int>, index: int * int) =
    let adjacent = adjacent index

    (dumbos |> Map.map (fun key value -> if adjacent |> Seq.contains key then value + 1 else value), adjacent |> Seq.toList)

let flashAll (dumbos: Map<int * int, int>) =
    let mutable result = dumbos
    let mutable dumbosThatFlashed = []
    for y = 0 to 9 do
        for x = 0 to 9 do
            let mutable dumbosToVisit = [(y, x)]
            while dumbosToVisit.Length > 0 do
                let index = dumbosToVisit.Head
                if safeIndexing (result, index) > 9 && not (dumbosThatFlashed |> List.contains index) then
                    dumbosThatFlashed <- index :: dumbosThatFlashed 
                    let (incrementedDumbos, adjacent) = incrementAdjacent (result, index)
                    result <- incrementedDumbos
                    dumbosToVisit <- dumbosToVisit |> List.removeAt 0 |> List.append adjacent
                else
                    dumbosToVisit <- dumbosToVisit |> List.removeAt 0

    (dumbosThatFlashed.Length, result |> Map.map (fun key value -> if dumbosThatFlashed |> List.contains key then 0 else value))

type Day11() =
    inherit Base.Day()
    override this.partOne(input: seq<string>): int = 
        let mutable dumbos =
            input
            |> Seq.collect Seq.toArray 
            |> Seq.mapi (fun i d -> ((i / 10, i % 10), System.Int32.Parse <| d.ToString()))
            |> Map.ofSeq

        let steps = 100


        printfn "Before any steps:"
        print dumbos
        printfn ""

        let mutable sum = 0
        for step = 0 to steps - 1 do
            dumbos <- incrementAll dumbos
            let (numberOfFlashes, updatedDumbos) = flashAll dumbos
            sum <- sum + numberOfFlashes
            dumbos <- updatedDumbos
            printfn "After step %d" (step + 1)
            print dumbos
            printfn "----------"



        sum

    override this.partTwo(input: seq<string>): int = 
        let mutable dumbos =
            input
            |> Seq.collect Seq.toArray 
            |> Seq.mapi (fun i d -> ((i / 10, i % 10), System.Int32.Parse <| d.ToString()))
            |> Map.ofSeq

        printfn "Before any steps:"
        print dumbos
        printfn ""


        let step =
            (+) 0
            |> Seq.initInfinite
            |> Seq.tryFindIndex (fun step ->
                dumbos <- incrementAll dumbos
                let (numberOfFlashes, updatedDumbos) = flashAll dumbos
                dumbos <- updatedDumbos
                numberOfFlashes = 100
            )

        if step.IsNone then 0 else step.Value + 1