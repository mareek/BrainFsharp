module Compiler

open System
open System.Text

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

let rec private anchorClosingBracket (program : string list) lastOpeningBracket = 
    match program with
    | head :: tail -> match head.[0] with
                      | '[' -> [head] @ (anchorClosingBracket tail (lastOpeningBracket + 1))
                      | ']' -> ["]" + lastOpeningBracket.ToString()] @ (anchorClosingBracket tail lastOpeningBracket)
                      |  _  -> [head] @ (anchorClosingBracket tail lastOpeningBracket)
    | [] -> []

let rec private anchorOpeningBracket (program : string list) nextClosingBracket = 
    match program with
    | head :: tail -> match head.[0] with
                      | '[' -> ["[" + nextClosingBracket.ToString()] @ (anchorOpeningBracket tail nextClosingBracket)
                      | ']' -> [head] @ (anchorClosingBracket tail (nextClosingBracket + 1))
                      |  _  -> [head] @ (anchorClosingBracket tail nextClosingBracket)
    | [] -> []

let compile program outputFile = 
    let programAsList = Array.toList program

    // clean program from garbage chars
    let cleanedProgram = cleanProgram programAsList
    
    //check program correctness
    verifyProgram cleanedProgram

    // char to strings + add anchors
    let tokenizedProgram = tokenize cleanedProgram
    
    // associate closing bracket to anchor
    let halfAnchoredProgram = anchorClosingBracket tokenizedProgram -1

    // associate closing bracket to anchor
    let fullyAnchoredProgram = anchorOpeningBracket halfAnchoredProgram 0

    ()