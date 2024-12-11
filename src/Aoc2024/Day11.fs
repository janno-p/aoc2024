module Aoc2024.Day11

open System
open System.Collections.Generic
open Aoc2024.Framework

let (|EvenDigits|_|) (v: int64) =
    let mutable rank = 0
    let mutable pow = 1L
    let mutable current = v
    while current > 0 do
        rank <- rank + 1
        pow <- if rank % 2 = 1 then pow else pow * 10L
        current <- current / 10L
    if rank % 2 = 1 then None else Some(v / pow, v % pow)

let rec countStones it (cache: Dictionary<int64 * int, int64>) (num: int64) =
    if it = 0 then 1L else
    match cache.TryGetValue ((num, it)) with
    | true, v -> v
    | _ ->
        let v =
            match num with
            | 0L ->
                countStones (it - 1) cache 1L
            | EvenDigits (first, second) ->
                let a = countStones (it - 1) cache first
                let b = countStones (it - 1) cache second
                a + b
            | _ ->
                countStones (it - 1) cache (2024L * num)
        cache[(num, it)] <- v
        v

let blink times (Input input) =
    input.Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map Convert.ToInt64
    |> Seq.map (countStones times (Dictionary()))
    |> Seq.sum

let solution =
    Solution.create (blink 25) (blink 75)
