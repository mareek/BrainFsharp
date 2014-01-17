open System
open System.Text
open System.IO
open Interpreter
open Compiler

let openProgram (programFile : FileInfo) = 
    let programStream = programFile.OpenText()
    let program = programStream.ReadToEnd()
    getProgramOutput program

[<EntryPoint>]
let main argv = 
    
    if argv.Length = 0 then
        let outputFile = @"C:\Users\Matthieu\Desktop\TheRealBrainfuck.exe"
        //compile ",+." outputFile
        compile "[-]>[-]<>+++++++[<+++++++>-]<+++.--." outputFile
        ()
    elif File.Exists(argv.[0]) then
        openProgram (new FileInfo(argv.[0]))
    else
        getProgramOutput (String.Join(" ",  argv))
        let input = Console.ReadLine()
        () // stupid language
    
    0 // return an integer exit code
