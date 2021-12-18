module Day17

open System.Linq

let parseTarget (target: string) =
    let tmp = target.Split(", ")

    let xRange = tmp.First().Split(": x=").Last().Split("..") |> Array.map System.Int32.Parse

    let yRange = tmp.Last().Split("y=").Last().Split("..") |> Array.map System.Int32.Parse

    ((xRange.First(), xRange.Last()), (yRange.First(), yRange.Last()))

let isPositionInTarget (position: int * int, target: (int * int) * (int * int)) =
    let (xPosition, yPosition) = position
    let (minXTarget, maxXTarget) = fst target
    let (minYTarget, maxYTarget) = snd target
    xPosition >= minXTarget && xPosition <= maxXTarget && yPosition >= minYTarget && yPosition <= maxYTarget

let isPositionPastTarget (position: int * int, target: (int * int) * (int * int)) =
    let (xPosition, yPosition) = position
    let (_, maxXTarget) = fst target
    let (minYTarget, _) = snd target

    xPosition > maxXTarget || yPosition < minYTarget


let step (position, velocity) =
    let x = fst position + fst velocity
    let y = snd position + snd velocity

    let xVelocity =
        match fst velocity with
        | xv when xv > 0 -> xv - 1
        | xv when xv < 0 -> xv + 1
        | _ -> 0

    ((x, y), (xVelocity, snd velocity - 1))

type Day17() =
    inherit Base.Day()
    override this.partOne(input: seq<string>): int = 
        let target = input |> Seq.head |> parseTarget

        let mutable maxY = 0

        for xv = 0 to 200 do
            for yv = 0 to 400 do
                let mutable position = (0,0)
                let mutable velocity = (xv, yv)
                let mutable tmpMaxY = 0
                while not (isPositionInTarget (position, target)) && not (isPositionPastTarget (position, target)) do
                    printfn "%A" position
                    let (p, v) = step(position, velocity)
                    position <- p
                    velocity <- v
                    tmpMaxY <- if snd position > tmpMaxY then snd position else tmpMaxY

                maxY <- if isPositionInTarget (position, target) && tmpMaxY > maxY then tmpMaxY else maxY

        maxY
    override this.partTwo(input: seq<string>): int = 
        let target = input |> Seq.head |> parseTarget

        let mutable initVelocities = []

        let (xt, yt) = target
        for xv = 1 to (snd xt) do // cant go past the max x in a single step
            for yv = fst yt to abs(fst yt) do // cant start lower than the min y and can't go past the abs min y
                let mutable position = (0,0)
                let mutable velocity = (xv, yv)
                while not (isPositionInTarget (position, target)) && not (isPositionPastTarget (position, target)) do
                    printfn "%A" position
                    let (p, v) = step(position, velocity)
                    position <- p
                    velocity <- v

                if isPositionInTarget (position, target) then initVelocities <- (xv, yv) :: initVelocities

        initVelocities |> List.length


