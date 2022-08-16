module Interpreter

open RadLine
open System

open Common
open Parser
open Eval

let paserResultAdapted str =
    paserResult pStmt str
    |> Result.mapError EvaluationError.ParseError

let eval str =
    paserResultAdapted str |> Result.bind evalAdapted

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
        | EvaluationError.ParseError parseError ->
            printfn "%A" parseError
            repl ()
        | EvaluationError.ExecutionError executionError ->
            printfn "%A" executionError
            repl ()
