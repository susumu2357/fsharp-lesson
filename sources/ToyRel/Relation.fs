module Relation

open System
open Deedle

open Common

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
        let filepath = ".\\database\\master\\" + relationName + ".csv"

        Frame.ReadCsv filepath |> create

    let project (Relation df) (columns: string list) = create df.Columns.[columns]

    let saveAs rel basename =
        let df = value rel
        df.SaveCsv(databasePath + basename + ".csv")

    let save rel =
        let rndName = randName ()
        saveAs rel rndName
        rndName
