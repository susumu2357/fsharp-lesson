module Relation

open System
open Deedle

open Common
open Parser

let r = Random()

let randName unit =
    let randomChars =
        [ for x in 0..3 do
              string (char (r.Next((int 'a'), (int 'z') + 1))) ]

    "zz"
    // + List.fold (fun acc elem -> acc + string elem) "" randomChars
    + String.concat "" randomChars

module Relation =
    type T = Relation of Frame<int, string>

    let create (df: Frame<int, string>) =
        df.Rows.Values
        |> Seq.distinct
        |> Series.ofValues
        |> Frame.ofRows
        |> Relation

    let value (Relation rel) = rel

    let print rel = (value rel).Print()

    let openRelation relationName =
        match paserResult pIdentifierStmt relationName with
        | Expression exp ->
            match exp with
            | Identifier identifier ->
                let filepath =
                    // ".\\sources\\ToyRel\\database\\master\\"
                    ".\\database\\master\\" + identifier + ".csv"

                Frame.ReadCsv filepath |> create
            | _ -> failwithf "Failure"
        | _ -> failwithf "Failure"

    let project (Relation df) (columns: string list) = create df.Columns.[columns]

    let saveAs rel basename =
        let df = value rel
        df.SaveCsv(databasePath + basename + ".csv")

    let save rel =
        let rndName = randName ()
        saveAs rel rndName
        rndName
