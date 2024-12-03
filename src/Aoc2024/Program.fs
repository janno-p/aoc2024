module Aoc2024.Program

open FsSpectre
open Spectre.Console

[<EntryPoint>]
let main _ =
    let days = Framework.days
    let day =
        selectionPrompt {
            title "Which [green]day[/] results to calculate?"
            page_size 10
            more_choices_text "[grey](Move up and down to reveal available solutions)[/]"
            choices (days.Keys |> Seq.sortDescending |> Seq.toArray)
        }
        |> AnsiConsole.Prompt
    Framework.run (Map.find day days)
    0
