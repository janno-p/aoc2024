module Aoc2024.Day05

open System
open System.Collections.Generic
open System.IO
open Aoc2024.Framework

let parseConstraints (reader: TextReader) =
    let constraints = Dictionary<int, int list>()
    let rec parseNextConstraint () =
        match reader.ReadLine().Trim() with
        | "" -> ()
        | criteria ->
            let a, b =
                match criteria.Split('|', 2, StringSplitOptions.TrimEntries) |> Array.map Convert.ToInt32 with
                | [| a; b |] -> a, b
                | _ -> failwith "Invalid constraint definition"
            match constraints.TryGetValue a with
            | true, bs -> constraints[a] <- b::bs
            | _ -> constraints[a] <- [b]
            parseNextConstraint()
    parseNextConstraint ()
    Map.ofSeq (constraints |> Seq.map (fun kvp -> kvp.Key, kvp.Value))

let rec isValidSequence (constraints: Map<int, int list>) (values: int list) =
    match values with
    | [] -> true
    | cur::other ->
        let res =
            other
            |> List.forall (fun x ->
                match constraints.TryGetValue x with
                | true, xs -> xs |> List.contains cur |> not
                | _ -> true)
        if not res then false else isValidSequence constraints other

let validOrderScore constraints pages =
    if isValidSequence constraints pages then pages[pages.Length / 2] else 0
    
let isValidOrder (constraints: Map<int, int list>) fst snd =
    let p1 = match constraints.TryGetValue fst with true, xs -> xs |> List.contains snd | _ -> true
    let p2 = match constraints.TryGetValue snd with true, xs -> not (xs |> List.contains fst) | _ -> true
    p1 && p2
    
let invalidOrderScore constraints pages =
    if isValidSequence constraints pages then 0 else
    let isValidOrder = isValidOrder constraints
    let fixedOrder = ResizeArray<int>()
    fixedOrder.AddRange(pages)
    let findFirst (items: int list) =
        let isValid n = items |> List.forall (fun x -> x = n || isValidOrder n x)
        let rec findValid i =
            if isValid items[i] then items[i] else findValid (i + 1)
        findValid 0
    for i in 0..(pages.Length - 1) do
        let fst = findFirst (fixedOrder |> Seq.skip i |> Seq.toList)
        fixedOrder.Remove(fst) |> ignore
        fixedOrder.Insert(i, fst)
    fixedOrder[fixedOrder.Count / 2]

let calculate func (Input input) =
    use reader = new StringReader(input)
    let constraints = parseConstraints reader
    reader.ReadToEnd().Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> Seq.map (fun row -> row.Split(',') |> Array.map Convert.ToInt32 |> Array.toList)
    |> Seq.map (func constraints)
    |> Seq.sum

let solution =
    Solution.create (calculate validOrderScore) (calculate invalidOrderScore)
