module Aoc2024.Framework

open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Reflection

type Solver =
    string -> int

module Solver =
    let undefined: Solver = fun _ -> 0

type Solution = {
    FirstPart: Solver
    SecondPart: Solver
}

module Solution =
    let create (firstPart: Solver) (secondPart: Solver) =
        { FirstPart = firstPart; SecondPart = secondPart }

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
        firstInput |> solution.FirstPart |> printfn "Part 1 => %d"
        secondInput |> solution.SecondPart |> printfn "Part 2 => %d"

let dayRegex =
    System.Text.RegularExpressions.Regex(@"^Day\d{2}$")

let days =
    Assembly.GetExecutingAssembly().GetTypes()
    |> Seq.filter FSharpType.IsModule
    |> Seq.filter (fun t -> dayRegex.IsMatch(t.Name))
    |> Seq.map (fun x -> x.Name, x)
    |> Map.ofSeq
