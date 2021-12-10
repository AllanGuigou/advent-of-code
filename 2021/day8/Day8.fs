module Day8

open System.Linq

let splitValues (displayOutputValues: string) = displayOutputValues.Split ' '

let displayOutputValues (signalPattern: string) =
    signalPattern.Split " | "
    |> Seq.last
    |> splitValues

let signalPatternValues (signalPattern: string) =
    signalPattern.Split " | "
    |> Seq.head
    |> splitValues
    |> Array.map Seq.sort

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
        generateCharsToPrint (segmentWirings.[segmentToPrint], isHorizontalChar (segmentToPrint))
    else
        generateCharsToPrint ('.', isHorizontalChar (segmentToPrint))

let printSegmentWirings (segmentWirings: Map<char, char>) =
    printf "\n %s\n" (shouldPrintSegment (segmentWirings, 'a'))
    printf "%s    %s" (shouldPrintSegment (segmentWirings, 'b')) (shouldPrintSegment (segmentWirings, 'c'))
    printf "\n %s\n" (shouldPrintSegment (segmentWirings, 'd'))
    printf "%s    %s" (shouldPrintSegment (segmentWirings, 'e')) (shouldPrintSegment (segmentWirings, 'f'))
    printf "\n %s\n" (shouldPrintSegment (segmentWirings, 'g'))

type SegmentNumber = { Segments: char array; Number: int}
let zero = { Segments = [| 'a'; 'b'; 'c'; 'e'; 'f'; 'g'|]; Number = 0}
let one = { Segments = [| 'c'; 'f'|]; Number = 1}
let two = { Segments = [| 'a'; 'c'; 'd'; 'e'; 'g'|]; Number = 2}
let three = { Segments = [| 'a'; 'c'; 'd'; 'f'; 'g'|]; Number = 3}
let four = { Segments = [| 'b'; 'c'; 'd'; 'f'|]; Number = 4}
let five = { Segments = [| 'a'; 'b'; 'd'; 'f'; 'g'|]; Number = 5}
let six = { Segments = [| 'a'; 'b'; 'd'; 'e'; 'f'; 'g'|]; Number = 6}
let seven = { Segments = [| 'a'; 'c'; 'f'|]; Number = 7}
let eight = { Segments = [| 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'|]; Number = 8}
let nine = { Segments = [| 'a'; 'b'; 'c'; 'd'; 'f'; 'g'|]; Number = 9}
let numbers = [zero; one; two; three; four; five; six; seven; eight; nine]

let printSegmentNumber (number: SegmentNumber) =
    number.Segments
    |> Array.map (fun x -> (x, x))
    |> Map.ofArray
    |> printSegmentWirings

let seqDifference (left: seq<char>, right: seq<char>) =
    set left - set right
    |> Set.toSeq

let swap (x, y) = y, x
let swapAll tuples = List.map swap tuples
let invert map = map |> Map.toList |> swapAll |> Map.ofList


type Day8() =
    inherit Base.Day()

    override this.partOne(input: seq<string>) : int =
        let numberOfUniqueSegments =
            input
            |> Seq.collect displayOutputValues
            |> countUniqueNumberOfSegments

        numberOfUniqueSegments

    override this.partTwo(input: seq<string>) : int =
        let patterns = input |> Seq.map signalPatternValues  |> Seq.toArray
        let outputValues = input |> Seq.map displayOutputValues |> Seq.toArray

        let mutable finalResult = 0
        for i = 0 to patterns.Count() - 1 do
            let signalPatterns = patterns.[i]
            let displayOutputValues = outputValues.[i]
            let mutable segmentWirings = Map.empty<char, char> // first character is the intended position on the display // second character is the miswiring

            let oneSegmentWirings = 
                signalPatterns
                |> Array.find (fun x -> x.Count() = one.Segments.Length)

            let sevenSegmentWirings =
                signalPatterns
                |> Array.find (fun x -> x.Count() = seven.Segments.Length)

            segmentWirings <- segmentWirings.Add('a', seqDifference(sevenSegmentWirings, oneSegmentWirings) |> Seq.head)

            let fourSegmentWirings =
                signalPatterns
                |> Array.find (fun x -> x.Count() = four.Segments.Length)

            let bOrDSegmentWirings = seqDifference(fourSegmentWirings, oneSegmentWirings)

            let eightSegmentWirings =
                signalPatterns
                |> Array.find (fun x -> x.Count() = eight.Segments.Length)

            let nineSixOrZeroSegmentWirings =
                signalPatterns
                |> Array.filter (fun x -> x.Count() = 6)

            let zeroSegmentWirings =
                // the one that is missing d is zero
                nineSixOrZeroSegmentWirings
                |> Seq.find (fun x -> seqDifference(bOrDSegmentWirings, x).Count() > 0)
                // |> Seq.toArray
                // |> (fun x -> (System.String x))
                // |> printf "%A\n" 

            let dSegmentWirings = seqDifference(eightSegmentWirings, zeroSegmentWirings) 
            segmentWirings <- segmentWirings.Add('d', dSegmentWirings |> Seq.head)
            segmentWirings <- segmentWirings.Add('b', seqDifference(bOrDSegmentWirings, dSegmentWirings) |> Seq.head)


            let sixSegmentWirings =
                nineSixOrZeroSegmentWirings
                |> Seq.find (fun x -> seqDifference(sevenSegmentWirings, x).Count() > 0)

            segmentWirings <- segmentWirings.Add('c', seqDifference(sevenSegmentWirings, sixSegmentWirings) |> Seq.head)

            let nineSegmentWirings =
                nineSixOrZeroSegmentWirings
                |> Seq.find (fun x -> seqDifference(x, zeroSegmentWirings).Count() > 0 && seqDifference(x, sixSegmentWirings).Count() > 0)
            segmentWirings <- segmentWirings.Add('e', seqDifference(eightSegmentWirings, nineSegmentWirings) |> Seq.head)
            segmentWirings <- segmentWirings.Add('g', seqDifference(nineSegmentWirings, fourSegmentWirings) |> Seq.find (fun x -> not (x = segmentWirings.['a'])))
            segmentWirings <- segmentWirings.Add('f', oneSegmentWirings |> Seq.find (fun x -> not (x = segmentWirings.['c'])))

            let wonkyWiring = invert segmentWirings

            let mutable result = ""
            for value in displayOutputValues do
                let unwonkyValue = (value |> Seq.toArray |> Seq.map (fun x -> wonkyWiring.[x])) |> Seq.toArray |> Array.sort |> System.String

                result <- result + (numbers |> (Seq.find (fun (x: SegmentNumber) -> (x.Segments |> System.String) = unwonkyValue))).Number.ToString()

            // printSegmentWirings (segmentWirings)

            finalResult <- finalResult + (System.Int32.Parse result)

        finalResult