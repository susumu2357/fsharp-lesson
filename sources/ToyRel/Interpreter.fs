module Interpreter

open RadLine
open System

open Common
open Parser
open Eval

let paserResultAdapted str =
    paserResult pStmt str
    |> Result.mapError EvaluationError.ParseError

/// <summary>Evaluate the ToyRel statement.</summary>
/// <remarks>Available statements are
/// <para><c>print [relation name]</c>: print out the first few rows of the relation.</para>
/// <para><c>use [database name]</c>: switch the database to be used.</para>
/// <para><c>list</c>: print out the list of relations stored in the database.</para>
/// <para><c>quit</c>: quit ToyRel.</para>
/// <para><c>project ([Expression]) [column1], [column2], [...]</c>: limit columns of Expression to the selected columns.
/// The returned relation will be named with a random string.
/// To specify the name, use <c>[relation name] = project ([Expression]) [column1], [column2], [...]</c></para>
/// <para>Example: <c>project (project (Employee) Name, DeptName) Name</c></para>
/// <para><c>restrict ([Expression]) ([Condition])</c>: limit rows of Expression to satisfy the Condition.
/// The condition is described as [column] [binary operator] [column | value],
/// The string value should be surrounded by the double quotation.
/// 'not', 'and', and 'or' can be used as logical operators. When combining conditions with logical operators,
/// each condition should be surrounded by the parenthesis.
/// The returned relation name convention is the same as the <c>project</c>.</para>
/// <para>Example: <c>restrict (stock) ((date_out = \"INSTOCK\") and (sell_price >= 5))</c></para>
/// <para><c>([Expression]) difference ([Expression])</c>: subtract columns appeared in the right expression from the left expression.
/// The returned relation name convention is the same as the <c>project</c>.</para>
/// <para>Example: <c>(project (Dept) DeptName) difference (project (Empl) DeptName)</c></para>
/// </remarks>
/// <param name="str">The input ToyRel statement. The double quote should be escaped.</param>
/// <returns>If the evaluation succeeds, the evaluated result will be printed out to the terminal.
/// Otherwise, raise one of cases of EvaluationError.</returns>
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
    | Result.Ok _ -> repl ()
    | Result.Error err ->
        match err with
        | EvaluationError.ParseError parseError ->
            printfn "%A" parseError
            repl ()
        | EvaluationError.ExecutionError executionError ->
            printfn "%A" executionError
            repl ()
