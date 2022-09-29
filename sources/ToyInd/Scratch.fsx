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