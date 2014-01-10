open System
open System.Text

// http://esolangs.org/wiki/Brainfuck


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
        //   <  Decrement the pointer.
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
        | '[' -> "" //TODO
        //   ]  Jump backward to the matching [ unless the byte at the pointer is zero.
        | ']' -> "" //TODO
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
    executeProgram program
    let input = Console.ReadLine()
    0 // return an integer exit code
