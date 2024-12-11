module Aoc2024.Day02

open System
open System.IO
open Aoc2024.Framework

let safeReports useDampener (Input input) =
    let enumerate (levels: int list) =
        if useDampener then [for i in 1..levels.Length -> levels |> List.removeAt (i - 1)]
        else [levels]
    let doCheck levels =
        let diffs = levels |> List.pairwise |> List.map (fun (a, b) -> b - a)
        let min, max = List.min diffs, List.max diffs
        (min >= -3 && max <= -1) || (min >= 1 && max <= 3)
    let isSafe (report: string) =
        let levels =
            report.Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
            |> Array.map Convert.ToInt32
            |> Array.toList
        enumerate levels |> List.exists doCheck
    use reader = new StringReader(input)
    let rec scanReports safeCount =
        match reader.ReadLine() with
        | x when String.IsNullOrEmpty(x) -> safeCount
        | report -> scanReports (safeCount + (if isSafe report then 1 else 0))
    scanReports 0

let solution =
    Solution.create (safeReports false) (safeReports true)
