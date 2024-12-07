module Aoc2024.Framework

open Microsoft.FSharp.Reflection
open Spectre.Console
open System
open System.IO
open System.Reflection

type Solver =
    string -> int32

type Solver64 =
    string -> int64

module Solver =
    let undefined: Solver = fun _ -> 0
    let undefined64: Solver64 = fun _ -> 0

type Solution = {
    FirstPart: Solver64
    SecondPart: Solver64
}

let i64 (solver: Solver) : Solver64 =
    fun input -> solver input |> Convert.ToInt64

module Solution =
    let create (firstPart: Solver) (secondPart: Solver) =
        { FirstPart = firstPart |> i64; SecondPart = secondPart |> i64 }
        
    let create64 (firstPart: Solver64) (secondPart: Solver64) =
        { FirstPart = firstPart; SecondPart = secondPart }

let numberOfDay (day: string) =
    let num = Convert.ToInt32(day.Substring(3).TrimStart('0'))
    let suffix =
        match num % 10 with
        | 1 when num <> 11 -> "st"
        | 2 when num <> 12 -> "nd"
        | 3 when num <> 13 -> "rd"
        | _ -> "th"
    $"%d{num}%s{suffix}"

let run (dayModule: Type) =
    match dayModule.GetProperty("solution") with
    | null -> failwith "Solution not found"
    | propertyInfo ->
        let firstInputPath = Path.Combine("Data", $"%s{dayModule.Name}-1.txt")
        if not (File.Exists firstInputPath) then failwithf $"Problem input '%s{firstInputPath}' not found"
        let firstInput = File.ReadAllText(firstInputPath)
        let secondInputPath = Path.Combine("Data", $"%s{dayModule.Name}-2.txt")
        let secondInput = if File.Exists secondInputPath then File.ReadAllText(secondInputPath) else firstInput
        let solution = propertyInfo.GetValue(null) |> unbox<Solution>
        let dayNum = numberOfDay dayModule.Name
        AnsiConsole.MarkupLine($"[teal]Results of [b][red]{dayNum}[/][/] day:[/]")
        AnsiConsole.MarkupLine(firstInput |> solution.FirstPart |> sprintf "Part 1 => [lime]%d[/]")
        AnsiConsole.MarkupLine(secondInput |> solution.SecondPart |> sprintf "Part 2 => [lime]%d[/]")

let dayRegex =
    System.Text.RegularExpressions.Regex(@"^Day\d{2}$")

let days =
    Assembly.GetExecutingAssembly().GetTypes()
    |> Seq.filter FSharpType.IsModule
    |> Seq.filter (fun t -> dayRegex.IsMatch(t.Name))
    |> Seq.map (fun x -> x.Name, x)
    |> Map.ofSeq
