open System
open System.IO
open System.Text

open Interpreter
open Compiler

let WriteHelp = fun () ->
    Console.WriteLine "Usage : BrainFsharp [OPTION] SourceFile [ExeFile]"
    Console.WriteLine ""
    Console.WriteLine "OPTIONS:"
    Console.WriteLine "/C (default) : Compile brainfuck program in SourceFile to ExeFile"
    Console.WriteLine "/I : Execute brainfuck program in SourceFile and display output"
    Console.WriteLine "/TEST compile test program (debugging only)"

[<EntryPoint>]
let main argv = 
    
    if argv.Length = 0 then 
        WriteHelp()
    elif argv.[0] = "/TEST" then
        let outputFile = @"C:\Users\Matthieu\Desktop\TheRealBrainfuck.exe"
        compile "[-]>[-]<>+++++++[<+++++++>-]<+++.--." outputFile
    elif File.Exists(argv.[0]) || argv.[0] = "/C" && File.Exists(argv.[1]) then
        let sourceIndex = if argv.[0] = "/C" then 1 else 0
        let exeIndex = sourceIndex + 1
        let sourcePath = argv.[sourceIndex]
        let exePath = if argv.Length > exeIndex then argv.[exeIndex] else (argv.[sourceIndex] + ".exe")
        compile (File.ReadAllText sourcePath) exePath
    elif argv.[0] = "/I" && File.Exists(argv.[1]) then
        Console.WriteLine (getProgramOutput (File.ReadAllText argv.[1]))
    else
        WriteHelp()    
    0 // return an integer exit code
