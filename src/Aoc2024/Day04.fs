module Aoc2024.Day04

open System
open Aoc2024.Framework

let cti width x y = y * width + x

let getSearchPaths (i, w, h) =
    let x, y = i % w, i / w
    let rangeOf size v = [
        if v > 2 then yield [|v;v-1;v-2;v-3|]
        if v < (size - 3) then yield [|v;v+1;v+2;v+3|]
    ]
    let xs = rangeOf w x
    let ys = rangeOf h y
    (List.allPairs xs ys) @ (List.allPairs xs [[|y;y;y;y|]]) @ (List.allPairs [[|x;x;x;x|]] ys)
    |> List.map (fun (a, b) -> Array.zip a b)

let xmas = ['X';'M';'A';'S']

let countXmas (i, w, h) (table: char array) =
    if table[i] <> xmas[0] then 0 else
    let cti = cti w
    getSearchPaths (i, w, h)
    |> List.map (fun v -> if (v |> Array.mapi (fun i pos -> (i, pos)) |> Array.forall (fun (i, (x, y)) -> xmas[i] = table[cti x y])) then 1 else 0)
    |> List.sum

let countMas (i, w, h) (table: char array) =
    let x, y = i % w, i / w
    if table[i] <> 'A' || x = 0 || x = (w - 1) || y = 0 || y = (h - 1) then 0 else
    let cti = cti w
    let d1 = match table[cti (x - 1) (y - 1)], table[cti (x + 1) (y + 1)] with 'M', 'S' | 'S', 'M' -> 1 | _ -> 0
    let d2 = match table[cti (x - 1) (y + 1)], table[cti (x + 1) (y - 1)] with 'M', 'S' | 'S', 'M' -> 1 | _ -> 0
    d1 * d2

let counter func (Input input) =
    let lines = input.Split('\n', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
    let width, height = lines[0].ToCharArray().Length, lines.Length
    let table = lines |> Array.fold (fun (acc: ResizeArray<char>) r -> acc.AddRange(r.ToCharArray()); acc) (ResizeArray<char>()) |> Seq.toArray
    table |> Seq.indexed |> Seq.map (fun (i, _) -> func (i, width, height) table) |> Seq.sum

let solution =
    Solution.create (counter countXmas) (counter countMas)
