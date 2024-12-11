module Aoc2024.Day07

open System
open Aoc2024.Framework

let concat (x: int64) (y: int64) =
    let rec multiplier curr =
        let curr = 10L * curr
        if curr > y then curr else multiplier curr
    x * (multiplier 1L) + y

let evaluate2op (testValue: int64, args: int64 list) =
    let rec calc acc xs =
        match xs with
        | [] -> acc = testValue
        | x::xs ->  calc (acc * x) xs || calc (acc + x) xs
    calc (List.head args) (List.tail args)

let evaluate3op (testValue: int64, args: int64 list) =
    let rec calc acc xs =
        match xs with
        | [] -> acc = testValue
        | x::xs ->  calc (acc * x) xs || calc (acc + x) xs || calc (concat acc x) xs
    calc (List.head args) (List.tail args)

let totalCalibration (evaluate: int64 * int64 list -> bool) (Input input) =
    let equations =
        input.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        |> Seq.choose (fun row ->
            match row.Split(':', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries) with
            | [| testValue; remainder |] ->
                let v = Convert.ToInt64(testValue)
                let vs =
                    remainder.Split(' ', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
                    |> Seq.map Convert.ToInt64
                    |> Seq.toList
                Some(v, vs)
            | _ -> None)
        |> Seq.toList
    equations |> List.filter evaluate |> List.map fst |> List.sum

let solution =
    Solution.create (totalCalibration evaluate2op) (totalCalibration evaluate3op)
