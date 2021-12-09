module Day8

let splitDisplayOutputValues (displayOutputValues: string) = displayOutputValues.Split ' '

let displayOutputValues (signalPattern: string) =
    signalPattern.Split " | "
    |> Seq.last
    |> splitDisplayOutputValues

// 1; 4; 7; 8; have unique number of segments
let uniqueNumberOfSegments = [ 2; 3; 4; 7 ]

let isUniqueNumberOfSeqments (segments: int) =
    uniqueNumberOfSegments |> List.contains segments

let countUniqueNumberOfSegments (displayOutputValues: seq<string>) : int =
    displayOutputValues
    |> Seq.map (fun (x: string) -> x.Length)
    |> Seq.filter isUniqueNumberOfSeqments
    |> Seq.length

type Day8() =
    inherit Base.Day()

    override this.partOne(input: seq<string>) : int =
        let numberOfUniqueSegments =
            input
            |> Seq.collect displayOutputValues
            |> countUniqueNumberOfSegments

        numberOfUniqueSegments

    override this.partTwo(input: seq<string>) : int = failwith "Not Implemented"
