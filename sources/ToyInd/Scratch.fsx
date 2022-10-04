open System
open System.IO

#load "SimpleSearch.fs"
open SimpleSearch

// Environment.CurrentDirectory <- @"C:\Users\susum\OneDrive\Documents\fsharp\fsharp-lesson\sources\ToyInd"
// Environment.CurrentDirectory <- @"C:\Users\susum\Documents\fsharp-lesson\sources\ToyInd"

searchWordUnderDir "pipe3" "test_target"


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

FileIndex.createFileIndex "test_target/fparsec"
let fileIndexFparsec = FileIndex.loadFileIndex "test_index/fparsec"
FileIndex.lookupFileId fileIndexFparsec "test_target/fparsec/Samples/FSharpParsingSample/FParsecVersion/parser.fs" // 166

FileIndex.createFileIndex "test_target/fsharp"
let fileIndexFsharp = FileIndex.loadFileIndex "test_index/fsharp"
FileIndex.lookupFileId fileIndexFsharp "test_target/fsharp/tests/fsharp/core/unicode/kanji-unicode-utf8-withsig-codepage-65001.fs" // 3559

FileIndex.createFileIndex "test_target/elsevier"
let fileIndexElsevier = FileIndex.loadFileIndex "test_index/elsevier"
FileIndex.lookupFileId fileIndexElsevier "test_target/elsevier/json/S0378377418304001.json" // 36437