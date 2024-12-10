module Aoc2024.Day09

open System
open Aoc2024.Framework

let readSequence (input: string) =
    input.Trim().ToCharArray()
    |> Seq.map (fun c -> Convert.ToInt32(c - '0'))
    |> Seq.toList

let compact: Solver64 = fun input ->
    let disk =
        input
        |> readSequence
        |> Seq.chunkBySize 2
        |> Seq.fold (fun (acc: ResizeArray<int>, fileId) v ->
            match v with
            | [| file; free |] ->
                acc.AddRange(Array.create file fileId)
                acc.AddRange(Array.create free -1)
            | [| file |] ->
                acc.AddRange(Array.create file fileId)
            | _ -> failwith "never"
            (acc, fileId + 1)) (ResizeArray<int>(), 0)
        |> fst
        |> Seq.toArray
    let rec reduce (acc: ResizeArray<int>) i j =
        if i > j then acc else
        match disk[i], disk[j] with
        | -1, -1 -> reduce acc i (j - 1)
        | -1, v -> acc.Add(v); reduce acc (i + 1) (j - 1)
        | v, _ -> acc.Add(v); reduce acc (i + 1) j
    reduce (ResizeArray<int>()) 0 (disk.Length - 1)
    |> Seq.mapi (fun i x -> Convert.ToInt64(i * x))
    |> Seq.sum

let defragment: Solver64 = fun input ->
    let sequence = readSequence input
    let files = ResizeArray(sequence |> Seq.mapi (fun i x -> (i, x)) |> Seq.filter (fun (i, _) -> i % 2 = 0) |> Seq.map (fun (i, x) -> (x, i / 2, false)))
    let gaps = ResizeArray(sequence |> Seq.mapi (fun i x -> (i, x)) |> Seq.filter (fun (i, _) -> i % 2 = 1) |> Seq.map snd)
    gaps.Add(0)
    let rec moveFile index =
        if index = 0 then () else
        let fileSize, fileId, isMoved = files[index]
        let x = gaps.FindIndex(fun size -> size >= fileSize)
        if not isMoved && x >= 0 && x < index then
            gaps[x] <- gaps[x] - fileSize
            gaps.Insert(x, 0)
            gaps[index] <- gaps[index] + gaps[index + 1] + fileSize
            gaps.RemoveAt(index + 1)
            files.RemoveAt(index)
            files.Insert(x + 1, (fileSize, fileId, true))
            moveFile index
        else
            moveFile (index - 1)
    moveFile (files.Count - 1)
    let mutable checksum = 0L
    let mutable pos = 0
    for i in 1..files.Count do
        let size, id, _ = files[i - 1]
        let gap = gaps[i - 1]
        for j in 1..size do
            checksum <- checksum + Convert.ToInt64(pos * id)
            pos <- pos + 1
        pos <- pos + gap
    checksum

let solution =
    Solution.create64 compact defragment
