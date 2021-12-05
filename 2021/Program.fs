module Advent_Of_Code
open System
open Argu
open DayOne
open DayTwo
open DayThree

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

let parser = ArgumentParser.Create<CliArguments>(programName = "advent-of-code")

let results = parser.ParseCommandLine()

// TODO: default value to the latest day and part
let day = results.GetResult Day
let part = results.GetResult Part
let input = results.GetResult (Input, defaultValue = "sample" )

let data = IO.File.ReadLines $"day{day}/{input}.txt"

let result = 
    match (day, part) with 
        | (1, 1) -> (DayOne().partOne(data))
        | (1, 2) -> (DayOne().partTwo(data))
        | (2, 1) -> (DayTwo().partOne(data))
        | (2, 2) -> (DayTwo().partTwo(data))
        | (3, 1) -> (DayThree().partOne(data))
        | (3, 2) -> (DayThree().partTwo(data))
        | (_, _) -> raise (System.ArgumentException())


printf "%d\n" result
