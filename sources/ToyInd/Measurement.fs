module Measurement

open System

type Measurement (key: string) =
    let mutable totalTime = int64(0)
    let mutable num = 0
    let mutable startTime = int64(0)

    member this.start () =
        startTime <- DateTimeOffset(DateTime.Now).ToUnixTimeSeconds()

    member this.stop () =
        let diff = DateTimeOffset(DateTime.Now).ToUnixTimeSeconds() - startTime
        totalTime <- totalTime + diff
        num <- num + 1

    member this.showRecord () =
        printfn "Key: %s" key
        printfn "Total time (sec): %i" totalTime
        printfn "The number of measurements: %i" num    
        (key, totalTime, num)
