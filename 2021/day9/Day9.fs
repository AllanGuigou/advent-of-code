module Day9

open System.Linq

let adjacentPoints (currentPoint: int, rowLength: int, numberOfPoints: int) =
    let mutable points = []

    if (currentPoint % rowLength) > 0 then
        points <- (currentPoint - 1) :: points

    if (currentPoint + 1) % rowLength > 0 then
        points <- (currentPoint + 1) :: points

    if currentPoint + rowLength <= numberOfPoints then
        points <- (currentPoint + rowLength) :: points

    if currentPoint - rowLength >= 0 then
        points <- (currentPoint - rowLength) :: points

    points

let selectIndicies (heightMap: int array, adjacentPoints: int array) =
    seq {
        for point in adjacentPoints do
            yield heightMap.[point]
    }

let findLowPointIndicies (input: seq<string>) =
    let heightMap =
        input
        |> Seq.collect Seq.toArray
        |> Seq.map (fun c -> System.Int32.Parse(c.ToString()))
        |> Seq.toArray

    let rowLength = heightMap.Count() / input.Count()

    let selectAdjacentValues x = selectIndicies (heightMap, x)

    let mutable sum = 0
    let mutable lowPointIndicies = Seq.empty<int>

    for i = 0 to heightMap.Length - 1 do
        let numberOfLowerPoints =
            adjacentPoints (i, rowLength, heightMap.Length - 1)
            |> Seq.toArray
            |> selectAdjacentValues
            |> Seq.filter (fun x -> heightMap.[i] >= x)
            |> Seq.length

        if numberOfLowerPoints = 0 then
            lowPointIndicies <- lowPointIndicies.Append i
            sum <- sum + 1 + heightMap.[i]

    (heightMap, lowPointIndicies, sum)


let findAdjacentBasinNumbers (heightMap: int array, rowLength: int) =
    let rec reallyFindAdjacentNumbers (index: int) =
        // printf "index %d value %d\n" index heightMap.[index]

        // skip points that we have already reached in other interations
        if heightMap.[index] = 9 || heightMap.[index] = -1 then
            []
        else
            heightMap.[index] <- -1

            adjacentPoints (index, rowLength, heightMap.Length - 1)
            |> List.filter (fun x -> heightMap.[x] >= 0) // filter out heights we have already obsereved to avoid overflow
            |> List.collect reallyFindAdjacentNumbers
            |> List.append [ index ]


    reallyFindAdjacentNumbers

type Day9() =
    inherit Base.Day()

    override this.partOne(input: seq<string>) : int =
        let (_, _, sum) = findLowPointIndicies (input)
        sum

    override this.partTwo(input: seq<string>) : int =
        let (heightMap, lowPointIndicies, _) = findLowPointIndicies (input)
        let rowLength = heightMap.Count() / input.Count()

        printf "%A\n" lowPointIndicies

        // printf "%A\n" (findAdjacentBasinNumbers (heightMap, rowLength) (9))

        let mutable basins = Seq.empty<list<int>>

        let findBasin =
            findAdjacentBasinNumbers (heightMap, rowLength)

        for lpi in lowPointIndicies do
            basins <- basins.Append(findBasin lpi)

        printf "%A\n" (basins |> Seq.map Seq.length |> Seq.sort)

        basins
        |> Seq.map Seq.length
        |> Seq.sortDescending
        |> Seq.take 3
        |> Seq.reduce (fun a b -> a * b)
