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
let ascii = ASCIIEncoding.ASCII

let private setTapeValue (tape : byte[]) (pointer : int) newValue =
    Array.set tape pointer newValue
    tape

let private increment tape pointer =
    setTapeValue tape pointer (tape.[pointer] + 1uy)
    
let private decrement tape pointer =
    setTapeValue tape pointer (tape.[pointer] - 1uy)

let private input tape pointer =
    setTapeValue tape pointer (ascii.GetBytes([|Console.ReadKey(true).KeyChar|]).[0])

let private output (tape : byte[]) (pointer : int) =
    Console.Write(ascii.GetChars([|tape.[pointer]|]))
    tape

let rec private splitList listToSearch leftover charSplit = 
    match listToSearch with
    | head :: tail when head = charSplit -> (tail, leftover)
    | head :: tail -> splitList tail (head::leftover) charSplit
    | [] -> ([], leftover)

let rec private stepThroughProgram prevChars nextChars pointer tape = 
    match nextChars with
    | head :: tail -> match head with
                      | '>' -> stepThroughProgram (head :: prevChars) tail (pointer + 1) tape
                      | '<' -> stepThroughProgram (head :: prevChars) tail (pointer - 1) tape
                      | '+' -> stepThroughProgram (head :: prevChars) tail pointer (increment tape pointer)
                      | '-' -> stepThroughProgram (head :: prevChars) tail pointer (decrement tape pointer)
                      | '.' -> stepThroughProgram (head :: prevChars) tail pointer (output tape pointer)
                      | ',' -> stepThroughProgram (head :: prevChars) tail pointer (input tape pointer)
                      | '[' -> handleStartLoop prevChars nextChars pointer tape
                      | ']' -> handleEndLoop prevChars nextChars pointer tape
                      |  _  -> stepThroughProgram prevChars tail pointer tape
    | [] -> ()

and private handleStartLoop prevChars nextChars pointer tape = 
    let jump() = 
        let nextIfJump, prevIfJump = splitList nextChars prevChars ']'
        stepThroughProgram (']' :: prevIfJump) nextIfJump pointer tape
    
    let head = nextChars|> List.head
    let tail = nextChars|> List.tail

    match tape.[pointer] with
    | 0uy -> jump()
    |  _  -> stepThroughProgram (head :: prevChars) tail pointer tape

and private handleEndLoop prevChars nextChars pointer tape = 
    let jump() = 
        let prevIfJump, nextIfJump = splitList prevChars nextChars '['
        stepThroughProgram ('[' :: prevIfJump) nextIfJump pointer tape
    
    let head = nextChars|> List.head
    let tail = nextChars|> List.tail

    match tape.[pointer] with
    | 0uy -> stepThroughProgram (head :: prevChars) tail pointer tape
    |  _  -> jump()

let interpretBrainfuckProgram (program : string) = 
    let tape = Array.create 640000 0uy //ought to be enough for anybody
    let programAsList = Array.toList (program.ToCharArray())
    stepThroughProgram [] programAsList 0 tape
