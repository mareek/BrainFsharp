module Compiler

open System
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
    let hasOpeningBracket = program |> List.exists ((=) '[')
    let hasClosingBracket = program |> List.exists ((=) ']')

    if hasClosingBracket <> hasOpeningBracket then
        failwith "brackets do not match!"
    elif hasOpeningBracket then
        let firstOpeningBracketIndex = program |> List.findIndex  ((=) '[')
        let firstClosingBracketIndex = program |> List.findIndex  ((=) ']')
    
        let revProgram = program |> List.rev
        let lastOpeningBracketIndex = program.Length - 1 - (revProgram |> List.findIndex  ((=) '['))
        let lastClosingBracketIndex = program.Length - 1 - (revProgram |> List.findIndex  ((=) ']'))

        if firstClosingBracketIndex < firstOpeningBracketIndex || lastOpeningBracketIndex > lastOpeningBracketIndex then
            failwith "brackets do not match!"


let private cleanProgram program = 
    let validChars = ['>';'<';'+';'-';'.';',';'[';']']
    let isValidChar c = validChars |> List.exists ((=) c)
    program |> List.filter isValidChar

let private tokenize program =
    program |> List.map (fun c->c.ToString())

let private anotateBracket bracket label anchor =
    let destinationPrefix = match bracket with |"[" -> "]" | "]" -> "[" | _ -> ""
    bracket + label.ToString() + "-" + destinationPrefix + anchor.ToString()

let rec private anotateBrackets program openCount closeCount = 
    match program with
    | head :: tail -> match head with
                      | "[" -> [(anotateBracket head openCount closeCount)] @ (anotateBrackets tail (openCount + 1) closeCount)
                      | "]" -> [(anotateBracket head closeCount (openCount - 1))] @ (anotateBrackets tail openCount (closeCount + 1))
                      |  _  -> head :: (anotateBrackets tail openCount closeCount)
    | [] -> []

let private getLabelAndDestination (anotatedBracket : string) = 
    let parts = anotatedBracket.Split([|'-'|])
    let replaceBrackets (str : string) = str.Replace('[', 'O').Replace(']', 'C')
    let label = "_" + replaceBrackets parts.[0]
    let destination = "_" + replaceBrackets parts.[1]
    label, destination

let private BracketToCIL (anotatedBracket : string) =
    let label, destination = getLabelAndDestination anotatedBracket
    match anotatedBracket.[0] with
    | '[' -> getILOpeningBracketBlock label destination
    | ']' -> getILClosingBracketBlock label destination
    |  _  -> failwith "there's a bug in my code dear liza"
    

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

    // clean program from garbage chars
    let cleanedProgram = cleanProgram programAsList
    
    //check program correctness
    verifyProgram cleanedProgram

    // char to strings + add anchors
    let tokenizedProgram = tokenize cleanedProgram
    
    // associate closing bracket to anchor
    let anotatedProgram = anotateBrackets tokenizedProgram 0 0

    //convert to CIL
    let ilProgram = toCIL anotatedProgram
    
    let ilMainBody = ILInitMethodBlock @ ilProgram

    let ilFull = ILInitProgramBlock @ ilMainBody @ ILEndProgramBlock

    let ilFullString = String.Join("\r\n", (List.toArray ilFull)) 

    let tempIlFile = Path.GetTempFileName()
    try
        File.WriteAllText(tempIlFile, ilFullString)
        let compilerPath = @"C:\Windows\Microsoft.NET\Framework\v4.0.30319\ilasm.exe"
        let commandLineArgumentsTemplate = "\"{0}\" /output=\"{1}\""
        let commandLineArguments = String.Format(commandLineArgumentsTemplate, tempIlFile, outputFile)
        let compileProcess = System.Diagnostics.Process.Start(compilerPath, commandLineArguments)
        ()
    finally
        File.Delete tempIlFile
    ()
