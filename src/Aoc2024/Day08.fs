module Aoc2024.Day08

open System
open System.Collections.Generic
open Aoc2024.Framework

let markNodes pos1 pos2 (width, height) findAll (nodeMap: HashSet<int>) =
    let pos1_x, pos1_y = pos1 % width, pos1 / width
    let pos2_x, pos2_y = pos2 % width, pos2 / width
    if findAll then
        nodeMap.Add(pos1_y * width + pos1_x) |> ignore
        nodeMap.Add(pos2_y * width + pos2_x) |> ignore
    let delta_x, delta_y = pos2_x - pos1_x, pos2_y - pos1_y
    let rec markNodesBefore (x, y) =
        let new_x, new_y = x - delta_x, y - delta_y
        if new_x >= 0 && new_x < width && new_y >= 0 && new_y < height then
            nodeMap.Add(new_y * width + new_x) |> ignore
            if findAll then
                markNodesBefore (new_x, new_y)
    let rec markNodesAfter (x, y) =
        let new_x, new_y = x + delta_x, y + delta_y
        if new_x >= 0 && new_x < width && new_y >= 0 && new_y < height then
            nodeMap.Add(new_y * width + new_x) |> ignore
            if findAll then
                markNodesAfter (new_x, new_y)
    markNodesBefore (pos1_x, pos1_y)
    markNodesAfter (pos2_x, pos2_y)

let antinodes findAll: Solver = fun input ->
    let lines = input.Split('\n', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
    let width, height = lines[0].ToCharArray().Length, lines.Length
    let map =
        lines
        |> Array.fold (fun (acc: ResizeArray<char>) r -> acc.AddRange(r.ToCharArray()); acc) (ResizeArray<char>())
        |> Seq.toArray
    let nodeMap = HashSet<int>()
    map
    |> Array.iteri (fun i c ->
        if c = '.' then () else
        let offset = i + 1
        map
        |> Array.skip offset
        |> Array.iteri (fun j d -> if c <> d then () else markNodes i (offset + j) (width, height) findAll nodeMap))
    nodeMap.Count

let solution =
    Solution.create (antinodes false) (antinodes true)
