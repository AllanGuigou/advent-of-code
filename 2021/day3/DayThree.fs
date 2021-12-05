module DayThree

open System.Linq
open System.Collections

let charToString (char: char) = char.ToString()
let splitBy (text: string) = text |> Array.ofSeq |> Array.map charToString |> Array.map System.Int32.Parse

let calculateGammaAndEpsilonRate (diagnosticReport: int array array) =
    let numberOfColumns = diagnosticReport.[0].Count()
    let numberOfRows = diagnosticReport.Count()

    let mutable gammaRate = 0;
    let mutable epsilonRate = 0;

    for column in 0 .. numberOfColumns - 1 do
        let mutable numberOfOnes = 0
        let mutable numberOfZeros = 0
        for row in 0 .. numberOfRows - 1 do
            numberOfOnes <- numberOfOnes + diagnosticReport.[row].[column]  
            
        numberOfZeros <- numberOfRows - numberOfOnes
        gammaRate <- (gammaRate <<< 1) + (if numberOfOnes >= numberOfZeros then 1 else 0)
        epsilonRate <- (epsilonRate <<< 1) + (if numberOfOnes >= numberOfZeros then 0 else 1)

    printf "Gamma Rate: %d Epsilon Rate: %d\n" gammaRate epsilonRate    
    (gammaRate, epsilonRate)

let majorityBit (rows: int array) =
    let mutable numberOfOnes = 0
    let mutable numberOfZeroes = 0
    for row in rows do
        numberOfOnes <- numberOfOnes + if row = 1 then 1 else 0
        numberOfZeroes <- numberOfZeroes + if row = 0 then 1 else 0

    if numberOfOnes >= numberOfZeroes then 1 else 0

type DayThree() =
    inherit Base.Day()
    override u.partOne(input: seq<string>) =
        // printf "%d\n" (input |> Seq.map System.Int32.Parse |> Seq.sum)

        let diagnosticReport = input |> Seq.map splitBy |> Seq.toArray

        let (gammaRate, epsilonRate) = calculateGammaAndEpsilonRate(diagnosticReport)
        gammaRate * epsilonRate
        
    override u.partTwo(input: seq<string>) = 
        // oxygen rate is the most common
        // co2 scrubber rate is the least common
        let diagnosticReport = input |> Seq.map splitBy |> Seq.toArray

        let numberOfColumns = diagnosticReport.[0].Count()
        let numberOfRows = diagnosticReport.Count()

        // oxygen rate


        let mutable oxygenDiagnosticReport = diagnosticReport
        for column in 0 .. numberOfColumns - 1 do
            let majorityBit = majorityBit(oxygenDiagnosticReport |> Array.map ( fun n -> n.[column] ))

            oxygenDiagnosticReport <- oxygenDiagnosticReport |> Array.filter (fun n -> n.[column] = majorityBit)

        let mutable break = false
        let mutable co2ScrubberDiagnosticReport = diagnosticReport
        for column in 0 .. numberOfColumns - 1 do
            if co2ScrubberDiagnosticReport.Count() = 1 then
                break <- true
            else 
                let minorityBit = if majorityBit(co2ScrubberDiagnosticReport |> Array.map ( fun n -> n.[column] )) = 1 then 0 else 1

                co2ScrubberDiagnosticReport <- co2ScrubberDiagnosticReport |> Array.filter (fun n -> n.[column] = minorityBit)


        let convertArrayToInt (array: int array) =
            let mutable result = 0
            for digit in array do
                result <- (result <<< 1) + digit
            result

        let mutable oxygenRate = convertArrayToInt(oxygenDiagnosticReport.[0]);
        let mutable co2ScrubberRate = convertArrayToInt(co2ScrubberDiagnosticReport.[0]);

        printf "Oxygen Rate: %d Co2 Scrubber Rate: %d\n" oxygenRate co2ScrubberRate 
        oxygenRate * co2ScrubberRate