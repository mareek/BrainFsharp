module Interpreter

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

let rec private findMatchingBracket (program : String) instruction direction matchingBracket = 
    if instruction >= program.Length || instruction < 0 then
        failwith "no matching bracket"
    elif program.[instruction] = matchingBracket then
        instruction
    else
        findMatchingBracket program (instruction + direction) direction matchingBracket

let private processBracket program instruction jump direction matchingBracket =
    if not jump then
        1 + instruction
    else
        1 + findMatchingBracket program instruction direction matchingBracket

let private processOpenBracket program instruction jump =
    processBracket program instruction jump 1 ']'

let private processCloseBracket program instruction jump =
    processBracket program instruction jump -1 '['

let private changeTapeValue (tape : Byte[]) pointer newValue = 
    Array.set tape pointer newValue
    tape

let rec private step (program : string) tape instruction pointer =
    if instruction >= program.Length then
        "" 
    else
        match program.[instruction] with
        | '>' -> step program tape (instruction + 1) (pointer + 1)
        | '<' -> step program tape (instruction + 1) (pointer - 1)
        | '+' -> step program (changeTapeValue tape pointer (tape.[pointer] + 1uy)) (instruction + 1) pointer
        | '-' -> step program (changeTapeValue tape pointer (tape.[pointer] - 1uy)) (instruction + 1) pointer
        | '.' -> UnicodeEncoding.UTF8.GetString(tape, pointer, 1)  + (step program tape (instruction + 1) pointer)
        | ',' -> "" //TODO
        | '[' -> step program tape (processOpenBracket program instruction (tape.[pointer] = 0uy)) pointer
        | ']' -> step program tape (processCloseBracket program instruction (tape.[pointer] <> 0uy)) pointer
        // not an instruction
        |  _  -> ""

let getProgramOutput program =
    let tape = Array.create 640000 0uy; //ought to be enough for anybody
    let output = (step program tape 0 0)
    Console.WriteLine output
