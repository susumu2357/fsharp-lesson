open System
open System.IO

#load "SimpleSearch.fs"
open SimpleSearch

// Environment.CurrentDirectory <- @"C:\Users\susum\OneDrive\Documents\fsharp\fsharp-lesson\sources\ToyInd"
// Environment.CurrentDirectory <- @"C:\Users\susum\Documents\fsharp-lesson\sources\ToyInd"

searchWordUnderDir "test_target" "pipe3"


let testProcess = Diagnostics.Process.Start(
    new Diagnostics.ProcessStartInfo("grep", "-R pipe3 test_target")
    )
let mutable peakWorkingSet = int64 0
while not testProcess.HasExited do
    testProcess.Refresh()
    Console.WriteLine("-------------------------------------")
    Console.WriteLine($"  Physical memory usage     : {testProcess.WorkingSet64}")
    Console.WriteLine($"  Peak physical memory usage     : {testProcess.PeakWorkingSet64}")
    // Console.WriteLine($"  Base priority             : {testProcess.BasePriority}")
    // Console.WriteLine($"  Priority class            : {testProcess.PriorityClass}")
    // Console.WriteLine($"  User processor time       : {testProcess.UserProcessorTime}")
    // Console.WriteLine($"  Privileged processor time : {testProcess.PrivilegedProcessorTime}")
    // Console.WriteLine($"  Total processor time      : {testProcess.TotalProcessorTime}")
    // Console.WriteLine($"  Paged system memory size  : {testProcess.PagedSystemMemorySize64}")
    // Console.WriteLine($"  Paged memory size         : {testProcess.PagedMemorySize64}")

    if testProcess.PeakWorkingSet64 > peakWorkingSet then
        peakWorkingSet <- testProcess.PeakWorkingSet64
Console.WriteLine($"  Max peak physical memory usage : {peakWorkingSet}")
Console.WriteLine($"  Peak physical memory usage : {testProcess.PeakWorkingSet64}")

#load "CleanFsharpRepository.fs"
open CleanFsharpRepository

deleteFsprojUnderDir "test_target/fparsec"
deleteFsprojUnderDir "test_target/fsharp"

#load "FileIndex.fs"
open FileIndex

// Measure FParsec
// Create path_list
let mutable stopWatch = Diagnostics.Stopwatch.StartNew()
FileIndex.createFileIndex "test_target/fparsec"
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds

// Load path_list and look up once
stopWatch <- Diagnostics.Stopwatch.StartNew()
let fileIndexFparsec = FileIndex.loadFileIndex "test_index/fparsec"
FileIndex.lookupFileId fileIndexFparsec "test_target/fparsec/Samples/FSharpParsingSample/FParsecVersion/parser.fs" // 166
printfn "%d" (Diagnostics.Process.GetCurrentProcess().WorkingSet64)
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds

// Load path_list and look up 1000 times
let shuffle (r : Random) xs = xs |> Seq.sortBy (fun _ -> r.Next())
let randomFparsecFiles = 
    shuffle (Random ()) (File.ReadAllLines("test_index/fparsec/path_list.txt"))
    |> Seq.take 1000

stopWatch <- Diagnostics.Stopwatch.StartNew()
let lookupFparsec = FileIndex.lookupFileId (FileIndex.loadFileIndex "test_index/fparsec")
randomFparsecFiles
|> Seq.map lookupFparsec
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds

// Measure Fsharp
// Create path_list
stopWatch <- Diagnostics.Stopwatch.StartNew()
FileIndex.createFileIndex "test_target/fsharp"
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds

// Load path_list and look up once
stopWatch <- Diagnostics.Stopwatch.StartNew()
let fileIndexFsharp = FileIndex.loadFileIndex "test_index/fsharp"
FileIndex.lookupFileId fileIndexFsharp "test_target/fsharp/tests/fsharp/core/unicode/kanji-unicode-utf8-withsig-codepage-65001.fs" // 3559
printfn "%d" (Diagnostics.Process.GetCurrentProcess().WorkingSet64)
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds

// Load path_list and look up 1000 times
let randomFsharpFiles = 
    shuffle (Random ()) (File.ReadAllLines("test_index/fsharp/path_list.txt"))
    |> Seq.take 1000

stopWatch <- Diagnostics.Stopwatch.StartNew()
let lookupFsharp = FileIndex.lookupFileId (FileIndex.loadFileIndex "test_index/fsharp")
randomFsharpFiles
|> Seq.map lookupFsharp
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds


// Measure MongoDB
// Create path_list
stopWatch <- Diagnostics.Stopwatch.StartNew()
FileIndex.createFileIndex "test_target/mongo"
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds

// Load path_list and look up once
stopWatch <- Diagnostics.Stopwatch.StartNew()
let fileIndexMongo = FileIndex.loadFileIndex "test_index/mongo"
FileIndex.lookupFileId fileIndexMongo "test_target/mongo/src/third_party/IntelRDFPMathLib20U1/LIBRARY/float128/sizeof.c" // 15820
printfn "%d" (Diagnostics.Process.GetCurrentProcess().WorkingSet64)
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds

// Load path_list and look up 1000 times
let randomMongoFiles = 
    shuffle (Random ()) (File.ReadAllLines("test_index/mongo/path_list.txt"))
    |> Seq.take 1000

stopWatch <- Diagnostics.Stopwatch.StartNew()
let lookupMongo = FileIndex.lookupFileId (FileIndex.loadFileIndex "test_index/mongo")
randomMongoFiles
|> Seq.map lookupMongo
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds

// Measure Elsevier
// Create path_list
stopWatch <- Diagnostics.Stopwatch.StartNew()
FileIndex.createFileIndex "test_target/elsevier"
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds

// Load path_list and look up once
stopWatch <- Diagnostics.Stopwatch.StartNew()
let fileIndexElsevier = FileIndex.loadFileIndex "test_index/elsevier"
FileIndex.lookupFileId fileIndexElsevier "test_target/elsevier/json/S0378377418304001.json" // 36437
printfn "%d" (Diagnostics.Process.GetCurrentProcess().WorkingSet64)
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds

// Load path_list and look up 1000 times
let randomElsevierFiles = 
    shuffle (Random ()) (File.ReadAllLines("test_index/elsevier/path_list.txt"))
    |> Seq.take 1000

stopWatch <- Diagnostics.Stopwatch.StartNew()
let lookupElsevier = FileIndex.lookupFileId (FileIndex.loadFileIndex "test_index/elsevier")
randomElsevierFiles
|> Seq.map lookupElsevier
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds

// Create trigram index
#load "TrigramIndex.fs"
open TrigramIndex

TrigramIndex.createTrigramIndex "test_target/fparsec"

let trigramIndex = 
    TrigramIndex.loadWholeTrigramIndex "test_index/fparsec"
    |> Async.RunSynchronously

trigramIndex["000"]
(*
  ["11"; "12"; "13"; "14"; "16"; "18"; "20"; "21"; "22"; "23"; "26"; "27";
   "31"; "32"; "35"; "36"; "37"; "38"; "44"; "45"; "58"; "60"; "63"; "64";
   "74"; "81"; "82"; "83"; "85"; "92"; "97"; "98"; "99"; "101"; "110"; "116";
   "121"; "143"; "146"; "149"; "151"; "158"; "161"; "162"; "164"; "176"; "180";
   "181"]
*)

TrigramIndex.fetchTrigramIndex "test_index/fparsec" "pipe3"
// map [("ipe", ["7"; ...]); ("pe3", ["13"; ...]); ("pip", ["14"; ...])]

let trigramMap = TrigramIndex.fetchTrigramIndex "test_index/fparsec" "pipe3"

#load "SimpleSearch.fs"
open SimpleSearch
#load "FileIndex.fs"
open FileIndex
#load "TrigramIndex.fs"
open TrigramIndex

let hits = TrigramIndex.searchWord "generics" "test_target/fsharp" true
// The number of file ids for the first trigram: 1495
// The number of file ids after taking intersection: 295

Seq.length hits
// > Seq.length hits;;
// val it: int = 46

TrigramIndex.searchWord "match" "test_target/fsharp" true
// The number of file ids for the first trigram: 1826
// The number of file ids after taking intersection: 1638

TrigramIndex.searchWord "EntryPoint" "test_target/fsharp" true
// The number of file ids for the first trigram: 4370
// The number of file ids after taking intersection: 217


// Measure FParsec
// Create trigram_index
stopWatch <- Diagnostics.Stopwatch.StartNew()
TrigramIndex.createTrigramIndex "test_target/fparsec"
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds

// Measure FSharp
// Create trigram_index
stopWatch <- Diagnostics.Stopwatch.StartNew()
TrigramIndex.createTrigramIndex "test_target/fsharp"
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalSeconds

// Measure Mongo
// Create trigram_index
stopWatch <- Diagnostics.Stopwatch.StartNew()
TrigramIndex.createTrigramIndex "test_target/mongo"
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMinutes

// Measure Elsevier
// Create trigram_index
stopWatch <- Diagnostics.Stopwatch.StartNew()
TrigramIndex.createTrigramIndex "test_target/elsevier"
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMinutes


#load "Measurement.fs"
open Measurement
#load "SimpleSearch.fs"
open SimpleSearch
#load "FileIndex.fs"
open FileIndex
#load "TrigramIndex.fs"
open TrigramIndex

Measurement.start "test"
System.Threading.Thread.Sleep(1000)
Measurement.stop "test"
let payload = Measurement.showRecord ()
payload

// Top level measurements
TrigramIndex.createTrigramIndex "test_target/fparsec"

// Measure the TrigramIndex.searchWord
let fparsecResults = TrigramIndex.searchWord "pipe3" "test_target/fparsec" false
Seq.length fparsecResults
// val it: int = 46

Measurement.showRecord ()
//   dict
//     [("fileIndex", { TotalTime = 0
//                      NumCalls = 1 }); ("relevantTrigramMap", { TotalTime = 0
//                                                                NumCalls = 1 });
//      ("filePaths", { TotalTime = 0
//                      NumCalls = 1 }); ("SimpleSearch", { TotalTime = 0
//                                                          NumCalls = 1 });
//      ("filterLines", { TotalTime = 0
//                        NumCalls = 1 }); ("aggregateLines", { TotalTime = 0
//                                                              NumCalls = 1 })]

Measurement.reset ()
let fsharpResults = TrigramIndex.searchWord "generics" "test_target/fsharp" false
Seq.length fsharpResults
// val it: int = 18

Measurement.showRecord ()
//   dict
//     [("fileIndex", { TotalTime = 20
//                      NumCalls = 1 }); ("relevantTrigramMap", { TotalTime = 0
//                                                                NumCalls = 1 });
//      ("filePaths", { TotalTime = 40
//                      NumCalls = 1 }); ("SimpleSearch", { TotalTime = 0
//                                                          NumCalls = 1 });
//      ("filterLines", { TotalTime = 0
//                        NumCalls = 1 }); ("aggregateLines", { TotalTime = 0
//                                                              NumCalls = 1 })]
Measurement.reset ()
let simpleSearchResults = searchWordUnderDir "generics" "test_target/fsharp"
Seq.length simpleSearchResults
// val it: int = 90

Measurement.showRecord ()
//   dict
//     [("filterLines", { TotalTime = 0
//                        NumCalls = 1 }); ("aggregateLines", { TotalTime = 0
//                                                              NumCalls = 1 })]