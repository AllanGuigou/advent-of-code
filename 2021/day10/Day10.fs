module Day10

let brackets = [ '(', ')'; '[', ']'; '{', '}'; '<', '>';] |> Map.ofList

let isChunkCorrupted (chunks: char list, bracket: char) =
    if brackets.[chunks |> List.head] = bracket then
        (false, chunks |> List.removeAt 0)
    else 
        (true, List.empty<char>)

let popChunk (chunks: char list, bracket: char) =
    if chunks |> List.length > 0 then
        let (isChunkCorrupted, poppedOrEmptyChunks) = isChunkCorrupted(chunks, bracket)

        if isChunkCorrupted then
            (true, poppedOrEmptyChunks)
        else
            (false, poppedOrEmptyChunks)
    else
        (false, List.empty<char>)

let pushOrPop (chunks: char list, bracket: char) =
    if brackets |> Map.containsKey bracket then
        (false, bracket :: chunks)
    else 
        popChunk(chunks, bracket) 

let scorePartOne (bracket: char) =
    match bracket with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137

let isLineCorrupt (line: char array) =
    let mutable chunks = List.empty<char>
    let result =
        match
            line
            |> Array.tryFindIndex (fun bracket -> 
                    let (isBracketCorrupt, c) = pushOrPop(chunks, bracket)
                    chunks <- c
                    isBracketCorrupt 
                )
        with
        | Some n -> scorePartOne(line.[n])
        | None -> 0
    
    result

let scorePartTwo (bracket: char) =
    let result =
        match bracket with
        | '(' -> 1
        | '[' -> 2
        | '{' -> 3
        | '<' -> 4

    uint64(result)

let isLineIncomplete (line: char array): uint64 =
    let mutable chunks = List.empty<char>
    let result =
        match
            line
            |> Array.tryFindIndex (fun bracket -> 
                    let (isBracketCorrupt, c) = pushOrPop(chunks, bracket)
                    chunks <- c
                    isBracketCorrupt 
                )
        with
        | Some n -> uint64(0)
        | None -> chunks |> List.map scorePartTwo |> List.reduce (fun accum score -> accum * uint64(5) + score)

    result

type Day10() =
    inherit Base.Day()
    override this.partOne(input: seq<string>): int = 
        let lines =
            input
            |> Seq.map Seq.toArray


        let scores = lines |> Seq.map isLineCorrupt  |> Seq.filter (fun x -> x > 0)

        scores |> Seq.sum

    override this.partTwo(input: seq<string>): int = 
        let scores = input |> Seq.map Seq.toArray |> Seq.map isLineIncomplete |> Seq.filter (fun x -> x > uint64(0))
        printf "Big Answer: %u\n" (scores |> Seq.sort |> Seq.splitInto 2 |> Seq.head|> Seq.last)

        0