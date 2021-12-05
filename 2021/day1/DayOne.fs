module DayOne

open System.Linq

type DayOne() =
    inherit Base.Day()
    override u.partOne(input: seq<string>) = 
        let depths = input |> Seq.map System.Int32.Parse 

        let mutable numberOfDepthIncreases = -1 

        let mutable previousDepth = -1

        // https://adventofcode.com/2021/day/1
        for depth in depths do
            if (depth > previousDepth) then
                numberOfDepthIncreases <- numberOfDepthIncreases + 1
            
            previousDepth <- depth
            
        numberOfDepthIncreases
    override u.partTwo(input: seq<string>) = 
        let depths = input |> Seq.map System.Int32.Parse |> Array.ofSeq 

        let slidingWindowDepths = ResizeArray<int>(10)

        for i in 0 .. depths.Count() - 3 do
            slidingWindowDepths.Add(depths.[i] + depths.[i+1] + depths.[i+2])

        let mutable numberOfDepthIncreases = -1 

        let mutable previousDepth = -1

        // https://adventofcode.com/2020/day/1#part2
        for depth in slidingWindowDepths do
            if (depth > previousDepth) then
                numberOfDepthIncreases <- numberOfDepthIncreases + 1
            
            previousDepth <- depth
            
        numberOfDepthIncreases