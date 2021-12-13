module Advent_Of_Code

open System
open Argu
open DayOne
open DayTwo
open DayThree
open DayFour
open DayFive
open DaySix
open DaySeven
open Day8
open Day9
open Day10
open Day11
open Day12
open Day13

type CliArguments =
    | Day of int
    | Part of int
    | Input of string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Day _ -> "specify a day."
            | Part _ -> "specify a part."
            | Input _ -> "specify an input (allan, amy, sample)."

let parser =
    ArgumentParser.Create<CliArguments>(programName = "advent-of-code")

let results = parser.ParseCommandLine()

// TODO: default value to the latest day and part
let day = results.GetResult Day
let part = results.GetResult Part

let input =
    results.GetResult(Input, defaultValue = "sample")

let data =
    IO.File.ReadLines $"day{day}/{input}.txt"

let result =
    match (day, part) with
    | (1, 1) -> (DayOne().partOne (data))
    | (1, 2) -> (DayOne().partTwo (data))
    | (2, 1) -> (DayTwo().partOne (data))
    | (2, 2) -> (DayTwo().partTwo (data))
    | (3, 1) -> (DayThree().partOne (data))
    | (3, 2) -> (DayThree().partTwo (data))
    | (4, 1) -> (DayFour().partOne (data))
    | (4, 2) -> (DayFour().partTwo (data))
    | (5, 1) -> (DayFive().partOne (data))
    | (5, 2) -> (DayFive().partTwo (data))
    | (6, 1) -> (DaySix().partOne (data))
    | (6, 2) -> (DaySix().partTwo (data))
    | (7, 1) -> (DaySeven().partOne (data))
    | (7, 2) -> (DaySeven().partTwo (data))
    | (8, 1) -> (Day8().partOne (data))
    | (8, 2) -> (Day8().partTwo (data))
    | (9, 1) -> (Day9().partOne (data))
    | (9, 2) -> (Day9().partTwo (data))
    | (10, 1) -> (Day10().partOne (data))
    | (10, 2) -> (Day10().partOne (data))
    | (11, 1) -> (Day11().partOne (data))
    | (11, 2) -> (Day11().partTwo (data))
    | (12, 1) -> (Day12().partOne (data))
    | (12, 2) -> (Day12().partTwo (data))
    | (13, 1) -> (Day13().partOne (data))
    | (13, 2) -> (Day13().partTwo (data))
    | (_, _) -> raise (System.ArgumentException())


printf "%d\n" result
