module Measurement

open System
open System.Collections.Generic

module Measurement =
    [<Measure>] type msec
    type T = {TotalTime: int32<msec>; NumCalls: int}
    let payload = new Dictionary<string, T>()
    let ticker = new Dictionary<string, int32>()

    let start (key: string) =
        if not (payload.ContainsKey(key)) then
            ticker.Add(key, Environment.TickCount)
            payload.Add(
                key, 
                {TotalTime=0<msec>; NumCalls=int(0)}
            )
        else
            ticker[key] <- Environment.TickCount 
        printfn "initial ticker: %i" ticker[key]

    let stop (key: string) =
        let diff = 
            Environment.TickCount - ticker[key] 
            |> (*) 1<msec>
        let prevTime = payload[key].TotalTime
        let prevNum = payload[key].NumCalls
        payload[key] <- {TotalTime=prevTime+diff; NumCalls=prevNum+1}
        printfn "last ticker: %i" Environment.TickCount

    let reset () =
        payload.Clear ()
        ticker.Clear ()
        |> ignore

    let showRecord () =
        payload




// type Measurement (key: string) =
//     let mutable totalTime = int64(0)
//     let mutable num = 0
//     let mutable startTime = int64(0)

//     member this.start () =
//         startTime <- DateTimeOffset(DateTime.Now).ToUnixTimeSeconds()

//     member this.stop () =
//         let diff = DateTimeOffset(DateTime.Now).ToUnixTimeSeconds() - startTime
//         totalTime <- totalTime + diff
//         num <- num + 1

//     member this.showRecord () =
//         printfn "Key: %s" key
//         printfn "Total time (sec): %i" totalTime
//         printfn "The number of measurements: %i" num    
//         (key, totalTime, num)
