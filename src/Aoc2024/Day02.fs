module Aoc2024.Day02

open System
open System.IO
open Aoc2024.Framework

type ScannerState = {
    CurrentValue: int
    InvalidLevels: int
}

type ReportScanner =
    ReportScanner of ScannerState * ScannerState

module ReportScanner =
    let init v =
        let incr: ScannerState = { CurrentValue = v; InvalidLevels = 0 }
        let decr: ScannerState = { CurrentValue = v; InvalidLevels = 0 }
        ReportScanner (incr, decr)

    let addLevel (ReportScanner (incr, decr)) level =
        let updatedIncr =
            match level - incr.CurrentValue with
            | 1 | 2 | 3 -> { incr with CurrentValue = level }
            | _ -> { incr with InvalidLevels = incr.InvalidLevels + 1 }
        let updatedDecr =
            match decr.CurrentValue - level with
            | 1 | 2 | 3 -> { decr with CurrentValue = level }
            | _ -> { decr with InvalidLevels = decr.InvalidLevels + 1 }
        ReportScanner (updatedIncr, updatedDecr)

    let isSafe (ReportScanner (incr, decr)) =
        incr.InvalidLevels = 0 || decr.InvalidLevels = 0

let safeReports hasThreshold : Solver = fun input ->
    let enumerate (levels: int list) =
        if hasThreshold then [for i in 1..levels.Length -> levels |> List.removeAt (i - 1)]
        else [levels]
    let doCheck levels =
        List.tail levels
        |> List.fold ReportScanner.addLevel (ReportScanner.init (List.head levels))
        |> ReportScanner.isSafe
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
