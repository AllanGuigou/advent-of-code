module Day8

let splitValues (displayOutputValues: string) = displayOutputValues.Split ' '

let displayOutputValues (signalPattern: string) =
    signalPattern.Split " | "
    |> Seq.last
    |> splitValues

let signalPatternValues (signalPattern: string) =
    signalPattern.Split " | "
    |> Seq.head
    |> splitValues

// 1; 4; 7; 8; have unique number of segments
let uniqueNumberOfSegments = [ 2; 3; 4; 7 ]

let isUniqueNumberOfSeqments (segments: int) =
    uniqueNumberOfSegments |> List.contains segments

let countUniqueNumberOfSegments (displayOutputValues: seq<string>) : int =
    displayOutputValues
    |> Seq.map (fun (x: string) -> x.Length)
    |> Seq.filter isUniqueNumberOfSeqments
    |> Seq.length

let generateCharsToPrint (char: char, horizontal: bool) =
    if horizontal then
        String.replicate 4 (char.ToString())
    else
        char.ToString()

let isHorizontalChar (char: char) =
    match char with
    | 'a'
    | 'd'
    | 'g' -> true
    | 'b'
    | 'c'
    | 'e'
    | 'f' -> false


let shouldPrintSegment (segmentWirings: Map<char, char>, segmentToPrint: char) =
    if (segmentWirings.ContainsKey(segmentToPrint)) then
        generateCharsToPrint (segmentToPrint, isHorizontalChar (segmentToPrint))
    else
        generateCharsToPrint ('.', isHorizontalChar (segmentToPrint))

let printSegmentWirings (segmentWirings: Map<char, char>) =
    printf "\n%s\n" (shouldPrintSegment (segmentWirings, 'a'))
    printf "%s  %s" (shouldPrintSegment (segmentWirings, 'b')) (shouldPrintSegment (segmentWirings, 'c'))
    printf "\n%s\n" (shouldPrintSegment (segmentWirings, 'd'))
    printf "%s  %s" (shouldPrintSegment (segmentWirings, 'e')) (shouldPrintSegment (segmentWirings, 'f'))
    printf "\n%s\n" (shouldPrintSegment (segmentWirings, 'g'))



type Day8() =
    inherit Base.Day()

    override this.partOne(input: seq<string>) : int =
        let numberOfUniqueSegments =
            input
            |> Seq.collect displayOutputValues
            |> countUniqueNumberOfSegments

        numberOfUniqueSegments

    override this.partTwo(input: seq<string>) : int =
        let signalPatterns = input |> Seq.map signalPatternValues

        // do a pass with the unique numbers to build a base data set of which numbers map to which segment
        let mutable segmentWirings = Map.empty<char, char> // first character is the intended position on the display // second character is the miswiring

        segmentWirings <- segmentWirings.Add('a', 'a')
        segmentWirings <- segmentWirings.Add('c', 'c')
        printSegmentWirings (segmentWirings)


        let displayOutputValues = input |> Seq.map displayOutputValues
        0
