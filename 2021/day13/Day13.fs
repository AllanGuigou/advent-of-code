module Day13

let parseDots (input: seq<string>) =
    input
    |> Seq.filter (fun line -> line.Contains(','))
    |> Seq.map (fun dot -> dot.Split(","))
    |> Seq.map (fun dot -> (dot |> Array.head |> System.Int32.Parse, dot |> Array.last |> System.Int32.Parse))
    |> Set.ofSeq

let parseFolds (input: seq<string>) =
    input
    |> Seq.filter (fun line -> line.Contains('='))
    |> Seq.map (fun fold -> fold.Split(" ") |> Array.last)
    |> Seq.map (fun fold -> fold.Split("="))
    |> Seq.map (fun fold -> (fold |> Array.head, fold |> Array.last |> System.Int32.Parse))

let foldDot (dot: int * int, fold: string * int) =
    let axis = fst fold
    let value = snd fold

    if axis = "y" && snd dot > value then
        let dotY = snd dot
        (fst dot, value - dotY + value)
    else if axis = "x" && fst dot > value then
        let dotX = fst dot
        (value - dotX + value, snd dot)
    else
        dot

let print (dots: Set<int * int>) =
    for y = 0 to 5 do
        for x = 0 to 40 do
            if dots.Contains((x, y)) then
                printf "#"
            else
                printf " "
        printfn ""


type Day13() =
    inherit Base.Day()

    override this.partOne(input: seq<string>) : int =
        let dots = parseDots input
        let folds = parseFolds input

        let mutable result = Set.empty<int * int>

        let firstFold = folds |> Seq.head

        for dot in dots do
            result <- result |> Set.add (foldDot (dot, firstFold))

        print result
        result |> Set.count

    override this.partTwo(input: seq<string>) : int =
        let mutable dots = parseDots input
        let folds = parseFolds input

        for fold in folds do
            let mutable tmp = Set.empty<int * int>

            for dot in dots do
                tmp <- tmp |> Set.add (foldDot (dot, fold))

            dots <- tmp

        print dots
        printfn ""

        dots |> Set.count
