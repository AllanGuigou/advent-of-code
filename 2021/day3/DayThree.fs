module DayThree

type DayThree() =
    inherit Base.Day()
    override u.partOne(input: seq<string>) = input |> Seq.map System.Int32.Parse |> Seq.sum
    override u.partTwo(input: seq<string>) = input |> Seq.map System.Int32.Parse |> Seq.sum
