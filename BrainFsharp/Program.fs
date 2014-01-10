open System
open System.Text

// http://esolangs.org/wiki/Brainfuck
let rec findMatchingBracket (program : String) instruction direction matchingBracket = 
    if instruction >= program.Length || instruction < 0 then
        failwith "no matching bracket"
    elif program.[instruction] = matchingBracket then
        instruction
    else
        findMatchingBracket program (instruction + direction) direction matchingBracket

let processBracket program instruction jump direction matchingBracket =
    if not jump then
        1 + instruction
    else
        1 + findMatchingBracket program instruction direction matchingBracket

let processOpenBracket program instruction jump =
    processBracket program instruction jump 1 ']'

let processCloseBracket program instruction jump =
    processBracket program instruction jump -1 '['

let changeTapeValue (tape : Byte[]) pointer newValue = 
    Array.set tape pointer newValue
    tape

let rec programStep (program : string) tape instruction pointer =
    if instruction >= program.Length then
        "" 
    else
        match program.[instruction] with
        //   >  Increment the pointer.
        | '>' -> programStep program tape (instruction + 1) (pointer + 1)
        //   <  Decrement the pointer
        | '<' -> programStep program tape (instruction + 1) (pointer - 1)
        //   +  Increment the byte at the pointer.
        | '+' -> programStep program (changeTapeValue tape pointer (tape.[pointer] + 1uy)) (instruction + 1) pointer
        //   -  Decrement the byte at the pointer.
        | '-' -> programStep program (changeTapeValue tape pointer (tape.[pointer] - 1uy)) (instruction + 1) pointer
        //   .  Output the byte at the pointer.
        | '.' -> UnicodeEncoding.UTF8.GetString(tape, pointer, 1)  + (programStep program tape (instruction + 1) pointer)
        //   ,  Input a byte and store it in the byte at the pointer.
        | ',' -> "" //TODO
        //   [  Jump forward past the matching ] if the byte at the pointer is zero.
        | '[' -> programStep program tape (processOpenBracket program instruction (tape.[pointer] = 0uy)) pointer
        //   ]  Jump backward to the matching [ unless the byte at the pointer is zero.
        | ']' -> programStep program tape (processCloseBracket program instruction (tape.[pointer] <> 0uy)) pointer
        // not an instruction
        |  _  -> ""

let executeProgram program =
    let tape = Array.create 640000 0uy; //ought to be enough for anybody
    let output = (programStep program tape 0 0)
    Console.WriteLine output

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let program = String.Join(" ", argv)
    // "[-]>[-]<>+++++++[<+++++++>-]<+++.--."
    executeProgram program
    let input = Console.ReadLine()
    0 // return an integer exit code
