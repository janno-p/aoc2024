module Aoc2024.Day01

open System
open Aoc2024.Framework

let readLists (input: string) =
    let splitRowToValues (pair: string) =
        match pair.Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) with
        | [| x1; x2 |] -> (Convert.ToInt32(x1), Convert.ToInt32(x2))
        | _ -> failwith "Invalid input"
    input.Split('\n', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map splitRowToValues
    |> Seq.fold (fun (lst1, lst2) (v1, v2) -> (v1::lst1, v2::lst2)) ([], [])

let distance: Solver = fun input ->
    let lst1, lst2 = readLists input
    List.zip (List.sort lst1) (List.sort lst2)
    |> List.map (fun (a, b) -> Math.Abs(a - b))
    |> List.sum 

let similarity: Solver = fun input ->
    let lst1, lst2 = readLists input
    let scores =
        lst2
        |> List.fold (fun (acc: Map<int,int>) x ->
            acc |> Map.change x (fun v -> v |> Option.map ((+) 1) |> Option.orElse (Some 1))
            ) Map.empty
    lst1 |> List.map (fun x -> x * (scores |> Map.tryFind x |> Option.defaultValue 0)) |> List.sum

let solution =
    Solution.create distance similarity
