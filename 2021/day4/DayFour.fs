module DayFour
open System.Linq

let generateMarkableNumber(int: int) = (int, false)

let splitBy (text: string) = text.Split([|','|]) |> Seq.map System.Int32.Parse
let splitBySpace (text: string) = 
    text.Split([|' '|])
    |> Seq.filter (fun s -> s.Length > 0)
    |> Seq.map System.Int32.Parse
    |> Seq.map generateMarkableNumber
    |> Seq.toArray
    
let buildBoard (input: seq<string>) =
    input
    |> Seq.skip 2
    |> Seq.filter(fun n -> n.Length > 0)
    |> Seq.map splitBySpace
    |> Seq.chunkBySize 5
    |> Seq.toArray


let printBoard (board: (int * bool)[][]) =
    for i in 0 .. board.Count() - 1 do
        for j in board[i] do
            let (number, marked) = j
            if marked then
                printf "%01d " number
            else 
                printf "   "
        printf "\n"

let isItBingo (board: (int * bool)[][], row: int, column: int) =
    let mutable rowCount = 0
    let mutable colCount = 0 
    for i in 0 .. 4 do
        let (_, rowMarked) = board[row][i]
        let (_, colMarked) = board[i][column]
        rowCount <- rowCount + if rowMarked then 1 else 0
        colCount <- colCount + if colMarked then 1 else 0


    rowCount = 5 || colCount = 5 

let sumUnmarkedNumbers (board: (int * bool)[][]) =
    let foo = 
        board 
        |> Array.map Array.toSeq 
        |> Array.toSeq 
        |> Seq.reduce Seq.append 

    let mutable sum = 0
    for f in foo do
        let (number, marked) = f
        if not marked then
            sum <- sum + number

    sum

type DayFour() =
    inherit Base.Day()
    override u.partOne(input: seq<string>) =
        let numbersToDraw = input.First() |> splitBy

        let boards = buildBoard(input)

        let mutable gameOver = false
        let mutable result = 0

        for numberDrawn in numbersToDraw do
            for b in 0 .. boards.Count() - 1 do
                for i in 0 .. 4  do
                    for j in 0 .. 4 do
                        if (not gameOver) then
                            let board = boards[b]
                            let (number, _) = board[i][j]
                            if number = numberDrawn then
                                board[i][j] <- (number, true)
                                if isItBingo(boards[b], i, j) then
                                    printf "BINGO %d\n" b
                                    printBoard(boards[b])
                                    result <- numberDrawn * sumUnmarkedNumbers(boards[b])
                                    gameOver <- true


        result

    override u.partTwo(input: seq<string>) =
        let numbersToDraw = input.First() |> splitBy

        let boards = buildBoard(input)

        let mutable gameOver = false
        let mutable result = 0

        let mutable whichBoardsHaveBingo = Map.empty<int, bool>

        for numberDrawn in numbersToDraw do
            for b in 0 .. boards.Count() - 1 do
                for i in 0 .. 4  do
                    for j in 0 .. 4 do
                        if (not gameOver) then
                            let board = boards[b]
                            let (number, _) = board[i][j]
                            if number = numberDrawn then
                                board[i][j] <- (number, true)
                                if isItBingo(boards[b], i, j) then
                                    if not (whichBoardsHaveBingo |> Map.containsKey b) then
                                        whichBoardsHaveBingo <- whichBoardsHaveBingo.Add(b, true)

                                    if whichBoardsHaveBingo.Keys.Count = boards.Count() then
                                        result <- numberDrawn * sumUnmarkedNumbers(boards[b])
                                        gameOver <- true


        result