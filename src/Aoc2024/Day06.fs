module Aoc2024.Day06

open System
open System.Collections.Generic
open System.IO
open Aoc2024.Framework

type Dir =
    Up | Down | Left | Right
with
    static member FromChar(c) =
        match c with '^' -> Up | 'v' -> Down | '<' -> Left | '>' -> Right | _ -> failwith "Invalid direction value"
    member this.Turn() =
        match this with Up -> Right | Right -> Down | Down -> Left | Left -> Up
    member this.Delta with get() =
        match this with Up -> (-1, 0) | Right -> (0, 1) | Down -> (1, 0) | Left -> (0, -1)

let sum (a, b) (c, d) =
    (a + c, b + d)

let mapSize map =
    let maxX = (Array2D.length2 map) - 1
    let maxY = (Array2D.length1 map) - 1
    (maxY, maxX)

let (|OutOfMap|_|) mapSize pos =
    let maxY, maxX = mapSize
    match pos with y, x when y < 0 || x < 0 || y > maxY || x > maxX -> Some(OutOfMap) | _ -> None

let escape map startPos startDir =
    let mapSize = mapSize map
    let markPos (y, x) =
        map[y,x] <- 'X'
    let rec navMap (dir: Dir) (pos: int * int) n =
        match sum pos dir.Delta with
        | OutOfMap mapSize -> n
        | y, x when map[y, x] = '#' -> navMap (dir.Turn()) pos n
        | y, x when map[y, x] = 'X' -> navMap dir (y, x) n
        | newPos -> markPos newPos; navMap dir newPos (n + 1)
    markPos startPos
    navMap startDir startPos 1

let isLoop map startPos startDir =
    let mapSize = mapSize map
    let _, maxX = mapSize
    let toi (y, x) =
        y * maxX + x
    let rec nextObstruction (dir: Dir) (pos: int * int) =
        match sum pos dir.Delta with
        | OutOfMap mapSize -> None
        | y, x when map[y, x] = '#' -> Some(pos)
        | newPos -> nextObstruction dir newPos
    let rec navMap (paths: HashSet<int * int>) (dir: Dir) (pos: int * int) =
        match nextObstruction dir pos with
        | Some newPos ->
            let newPath = (toi pos, toi newPos)
            if paths.Add(newPath) then navMap paths (dir.Turn()) newPos else true
        | None -> false
    navMap (HashSet<_>()) startDir startPos

let countLoops map startPos startDir =
    let maxY, maxX = mapSize map
    let mutable count = 0
    for y in 0..maxY do
        for x in 0..maxX do
            let copy = map |> Array2D.copy
            copy[y, x] <- '#'
            if isLoop copy startPos startDir then count <- count + 1
    count

let analyzeMap f: Solver = fun input ->
    use reader = new StringReader(input)
    let map, startPos, startDir =
        let chars =
            reader.ReadToEnd().Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
            |> Array.map _.ToCharArray()
        let mutable startPos = (-1, -1)
        let mutable dir = Dir.Up
        let map =
            Array2D.init chars[0].Length chars.Length (fun i j ->
                match chars[i][j] with '^' | '<' | '>' | 'v' as c -> startPos <- i, j; dir <- Dir.FromChar(c) | _ -> ()
                chars[i][j])
        map, startPos, dir
    f map startPos startDir

let solution =
    Solution.create (analyzeMap escape) (analyzeMap countLoops)
