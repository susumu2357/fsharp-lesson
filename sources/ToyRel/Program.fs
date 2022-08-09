open RadLine
open System

open Common
open StateM
open Eval

[<EntryPoint>]
let main _ =
    let dbInit = Database "master"

    let state = State()

    let setDB dbName =
        state {
            let db = Database dbName
            do! setValue db
        }

    let getDB: StateM<Database, Database> =
        state {
            let! dbName = getValue
            return dbName
        }

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

        // eval text
        state {
            do! setDB text
            let! currentDB = getDB
            return currentDB
        }
        |> printfn "currentDB: %A"

        repl ()

    repl ()
    0
