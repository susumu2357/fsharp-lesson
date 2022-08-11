﻿open RadLine
open System
open Eval

[<EntryPoint>]
let main _ =
    // printfn "first argument: %s" args[0]
    let lineEditor = LineEditor()
    lineEditor.Prompt <- LineEditorPrompt(">", ".")
    lineEditor.KeyBindings.Add<PreviousHistoryCommand>(ConsoleKey.LeftArrow, ConsoleModifiers.Control)
    lineEditor.KeyBindings.Add<NextHistoryCommand>(ConsoleKey.RightArrow, ConsoleModifiers.Control)

    let rec repl () =
        let text =
            lineEditor
                .ReadLine(
                    System.Threading.CancellationToken.None
                )
                .Result

        match eval text with
        | Ok _ -> repl ()
        | Error err ->
            match err with
            | ParseError parseError ->
                printfn "%A" parseError
                repl ()
            | ExecutionError executionError ->
                printfn "%A" executionError
                repl ()

    repl ()
    0
