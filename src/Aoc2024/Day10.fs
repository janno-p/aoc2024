module Aoc2024.Day10

open System
open System.Collections.Generic
open Aoc2024.Framework

let readMap (input: string) =
    let chars =
        input.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        |> Array.map _.ToCharArray()
    let width, height = chars[0].Length, chars.Length
    Array2D.init width height (fun i j -> Convert.ToInt32(chars[i][j] - '0'))

let directions =
    [(-1, 0); (0, -1); (1, 0); (0, 1)]

let isOnMap map (x, y) =
    x >= 0 && x < Array2D.length1 map && y >= 0 && y < Array2D.length2 map

let trailheads: Solver = fun input ->
    let map = readMap input
    let isOnMap = isOnMap map
    let scores = Array2D.init (Array2D.length1 map) (Array2D.length2 map) (fun _ _ -> Option<HashSet<int * int>>.None)
    let rec getScore (x, y) =
        let currentValue = map[y, x]
        match scores[y, x] with
        | None ->
            let set = HashSet<_>()
            if currentValue = 9 then
                set.Add(x, y) |> ignore
            else
                directions
                |> List.map (fun (dx, dy) -> (x + dx, y + dy))
                |> List.filter isOnMap
                |> List.filter (fun (x, y) -> map[y, x] = currentValue + 1)
                |> List.collect getScore
                |> List.iter (set.Add >> ignore)
            scores[y, x] <- Some(set)
            set |> Seq.toList
        | Some v -> v |> Seq.toList
    let mutable sum = 0
    map
    |> Array2D.iteri (fun y x v ->
        if v = 0 then
            let scoreAt = getScore (x, y)
            sum <- sum + scoreAt.Length)
    sum

let ratings: Solver = fun input ->
    let map = readMap input
    let isOnMap = isOnMap map
    let paths = Array2D.init (Array2D.length1 map) (Array2D.length2 map) (fun _ _ -> Option<int>.None)
    let rec getScore (x, y) : int =
        let currentValue = map[y, x]
        match paths[y, x] with
        | None ->
            let v =
                if currentValue = 9 then 1 else
                directions
                |> List.map (fun (dx, dy) -> (x + dx, y + dy))
                |> List.filter isOnMap
                |> List.filter (fun (x, y) -> map[y, x] = currentValue + 1)
                |> List.map getScore
                |> List.sum
            paths[y, x] <- Some v
            v
        | Some v -> v
    let mutable sum = 0
    map |> Array2D.iteri (fun y x v -> if v = 0 then sum <- sum + getScore (x, y))
    sum

let solution =
    Solution.create trailheads ratings
