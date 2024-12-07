module Aoc2024.Day03

open System
open Aoc2024.Framework

let multiplications : Solver = fun input ->
    let getOperand stopChar (span: char list) =
        if span.Length > 1 && Char.IsDigit(span[0]) then
            if span[1] = stopChar then Some(1)
            elif span.Length > 2 && Char.IsDigit(span[1]) then
                if span[2] = stopChar then Some(2)
                elif span.Length > 3 && Char.IsDigit(span[2]) then
                    if span[3] = stopChar then Some(3)
                    else None
                else None
            else None
        else None
    let rec getSum (span: string) (sum: int) =
        let next = span.IndexOf("mul(")
        if next < 0 then sum else
        match getOperand ',' (span.Substring(next + 4, 4).ToCharArray() |> List.ofArray) with
        | Some(n) ->
            match getOperand ')' (span.Substring(next + 4 + n + 1, 4).ToCharArray() |> List.ofArray) with
            | Some(m) ->
                let a = Convert.ToInt32(span.Substring(next + 4, n))
                let b = Convert.ToInt32(span.Substring(next + 4 + n + 1, m))
                getSum (span.Substring(next + 4 + n + 1 + m + 1)) (sum + (a * b))
            | None -> getSum (span.Substring(next + 4)) sum
        | None -> getSum (span.Substring(next + 4)) sum
    getSum input 0

let enabledMultiplications : Solver = fun input ->
    let rec getEnabledSum (span: string) sum =
        let blockEnd = span.IndexOf("don't()")
        let subSpan = if blockEnd < 0 then span else span.Substring(0, blockEnd)
        let subSum = multiplications subSpan
        let nextStart = span.IndexOf("do()", subSpan.Length)
        if nextStart < 0 then sum + subSum else
        getEnabledSum (span.Substring(nextStart)) (sum + subSum)
    getEnabledSum input 0

let solution =
    Solution.create multiplications enabledMultiplications
