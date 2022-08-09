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

    let openRelation dbName relationName =
        let filepath =
            ".\\database\\"
            + dbName
            + @"\\"
            + relationName
            + ".csv"

        Frame.ReadCsv filepath |> create

    let project (Relation df) (columns: string list) = create df.Columns.[columns]

    let saveAs dbName rel basename =
        let df = value rel

        df.SaveCsv(
            ".\\database\\"
            + dbName
            + @"\\"
            + basename
            + ".csv"
        )

    let save dbName rel =
        let rndName = randName ()
        saveAs dbName rel rndName
        rndName
