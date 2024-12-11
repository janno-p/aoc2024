module Aoc2024.Framework

open Microsoft.FSharp.Reflection
open Spectre.Console
open System
open System.IO
open System.Reflection

type Input =
    Input of string

type Solution =
    Input -> string * string

type Solver<'T> =
    Input -> 'T

module Solver =
    let undefined: Solver<_> = fun _ -> 0

module Solution =
    let create (firstPart: Solver<'T>) (secondPart: Solver<'T>) : Solution =
        fun input -> (firstPart input |> sprintf "%O", secondPart input |> sprintf "%O")

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
        let inputPath = Path.Combine("Data", $"%s{dayModule.Name}.txt")
        if not (File.Exists inputPath) then failwithf $"Problem input '%s{inputPath}' not found"
        let inputData = Input (File.ReadAllText(inputPath))
        let firstPart, secondPart =
            match propertyInfo.GetValue(null) with
            | :? Solution as solution -> solution inputData
            | _ -> failwith "Unexpected solution type"
        let dayNum = numberOfDay dayModule.Name
        AnsiConsole.MarkupLine($"[teal]Results of [b][red]{dayNum}[/][/] day:[/]")
        AnsiConsole.MarkupLine($"Part 1 => [lime]%O{firstPart}[/]")
        AnsiConsole.MarkupLine($"Part 2 => [lime]%O{secondPart}[/]")

let dayRegex =
    System.Text.RegularExpressions.Regex(@"^Day\d{2}$")

let days =
    Assembly.GetExecutingAssembly().GetTypes()
    |> Seq.filter FSharpType.IsModule
    |> Seq.filter (fun t -> dayRegex.IsMatch(t.Name))
    |> Seq.map (fun x -> x.Name, x)
    |> Map.ofSeq
