module Compiler

open System
open System.Diagnostics
open System.IO
open System.Text

open ILBlocks

// http://esolangs.org/wiki/Brainfuck
//  >  Increment the pointer.
//  <  Decrement the pointer
//  +  Increment the byte at the pointer.
//  -  Decrement the byte at the pointer.
//  .  Output the byte at the pointer.
//  ,  Input a byte and store it in the byte at the pointer.
//  [  Jump forward past the matching ] if the byte at the pointer is zero.
//  ]  Jump backward to the matching [ unless the byte at the pointer is zero.

let private verifyProgram program = 
    let firstOpeningBracketIndex = program |> List.tryFindIndex  ((=) '[')
    let firstClosingBracketIndex = program |> List.tryFindIndex  ((=) ']')
    let hasOpeningBracket = firstOpeningBracketIndex <> None
    let hasClosingBracket = firstClosingBracketIndex <> None

    if hasOpeningBracket <> hasClosingBracket then
        failwith "brackets do not match!"
    elif hasOpeningBracket  && firstClosingBracketIndex < firstOpeningBracketIndex then
        failwith "brackets do not match!"
    elif hasOpeningBracket then
        let revProgram = program |> List.rev
        let lastOpeningBracketIndex = program.Length - 1 - (revProgram |> List.findIndex  ((=) '['))
        let lastClosingBracketIndex = program.Length - 1 - (revProgram |> List.findIndex  ((=) ']'))

        if lastOpeningBracketIndex > lastOpeningBracketIndex then
            failwith "brackets do not match!"


let private cleanProgram program = 
    let validChars = ['>';'<';'+';'-';'.';',';'[';']']
    let isValidChar c = validChars |> List.exists ((=) c)
    program |> List.filter isValidChar

let private tokenize program =
    program |> List.map (fun c->c.ToString())

let private anotateBracket bracket label anchor =
    let destinationPrefix = match bracket with 
                            | "[" -> "]" 
                            | "]" -> "[" 
                            |  _  -> ""
    bracket + label.ToString() + "-" + destinationPrefix + anchor.ToString()

let rec private anotateBrackets program openCount closeCount = 
    match program with
    | head :: tail -> match head with
                      | "[" -> (anotateBracket head openCount closeCount) :: (anotateBrackets tail (openCount + 1) closeCount)
                      | "]" -> (anotateBracket head closeCount (openCount - 1)) :: (anotateBrackets tail openCount (closeCount + 1))
                      |  _  -> head :: (anotateBrackets tail openCount closeCount)
    | [] -> []

let private getLabelAndDestination (anotatedBracket : string) = 
    let parts = anotatedBracket.Split([|'-'|])
    let replaceBrackets (str : string) = str.Replace("[", "StartLoop_").Replace("]", "EndLoop_")
    let label = "_" + replaceBrackets parts.[0]
    let destination = "_" + replaceBrackets parts.[1]
    label, destination

let private BracketToCIL (anotatedBracket : string) =
    let label, destination = getLabelAndDestination anotatedBracket
    match anotatedBracket.[0] with
    | '[' -> getILOpeningBracketBlock label destination
    | ']' -> getILClosingBracketBlock label destination
    |  _  -> failwith ("This code should be unreachable - " + anotatedBracket)
    

let rec private toCIL program =
    match program with
    | head :: tail -> match head with
                      | ">" -> ILIncrementPointerBlock @ toCIL tail
                      | "<" -> ILDecrementPointerBlock @ toCIL tail
                      | "+" -> ILIncrementValueBlock @ toCIL tail
                      | "-" -> ILDecrementValueBlock @ toCIL tail
                      | "." -> ILOutputBlock @ toCIL tail
                      | "," -> ILInputBlock @ toCIL tail
                      |  b  -> (BracketToCIL b)  @ toCIL tail
    | [] -> []

let compile (program : string) (outputFile : string) = 
    let programAsList = Array.toList (program.ToCharArray())

    //clean program from garbage chars and comments
    let cleanedProgram = cleanProgram programAsList
    
    //check program correctness
    verifyProgram cleanedProgram

    //char to strings + add anchors
    let tokenizedProgram = tokenize cleanedProgram
    
    //associate closing bracket to anchor
    let anotatedProgram = anotateBrackets tokenizedProgram 0 0

    //convert to CIL
    let ilProgram = toCIL anotatedProgram
    
    //add initilization code
    let ilMainBody = ILInitMethodBlock @ ilProgram

    let ilFull = ILInitProgramBlock @ ilMainBody @ ILEndProgramBlock

    let ilFullString = String.Join("\r\n", (List.toArray ilFull)) 

    //assemble the result into an exe with ilasm.
    let tempIlFile = Path.GetTempFileName()
    try
        File.WriteAllText(tempIlFile, ilFullString)
        let compilerPath = @"C:\Windows\Microsoft.NET\Framework\v4.0.30319\ilasm.exe"
        let commandLineArguments = String.Format("/NOLOGO /QUIET \"{0}\" /output=\"{1}\"", tempIlFile, outputFile)
        let processInfo = new ProcessStartInfo(compilerPath, commandLineArguments)
        processInfo.UseShellExecute <- false
        let compileProcess = Process.Start(processInfo)
        compileProcess.WaitForExit()
        Console.WriteLine "BrainFuck program successully compiled !"
    finally
        File.Delete tempIlFile
