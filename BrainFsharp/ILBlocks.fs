module ILBlocks

// http://esolangs.org/wiki/Brainfuck
//  >  Increment the pointer.
//  <  Decrement the pointer
//  +  Increment the byte at the pointer.
//  -  Decrement the byte at the pointer.
//  .  Output the byte at the pointer.
//  ,  Input a byte and store it in the byte at the pointer.
//  [  Jump forward past the matching ] if the byte at the pointer is zero.
//  ]  Jump backward to the matching [ unless the byte at the pointer is zero.

let ILInitBlock =  [".maxstack 10";
                    ".locals init (int8[] tape, int32 pointer, int8 byteTemp, System.Char[] charBuffer, int8[] byteBuffer)";
                    "ldc.i4.s 640000";
                    "newarr int32";
                    "stloc tape";
                    "ldc.i4.0";
                    "stloc pointer"]

let ILIncrementPointerBlock = ["ldloc pointer";
                               "ldc.i4.1";
                               "add";
                               "stloc pointer"]

let ILDecrementPointerBlock = ["ldloc pointer";
                               "ldc.i4.1";
                               "sub";
                               "stloc pointer"]

let ILIncrementValueBlock = ["ldloc tape";
                             "ldloc pointer";
                             "ldelem.u1";
                             "ldc.i1.1";
                             "add";
                             "stloc byteTemp"
                             "ldloc tape";
                             "ldloc pointer";
                             "ldloc byteTemp";
                             "stelem"]

let ILDecrementValueBlock = ["ldloc tape";
                             "ldloc pointer";
                             "ldelem.u1";
                             "ldc.i1.1";
                             "sub";
                             "stloc byteTemp"
                             "ldloc tape";
                             "ldloc pointer";
                             "ldloc byteTemp";
                             "stelem"]

let ILOutputBlock = ["ldloc tape";
                     "ldloc pointer";
                     "ldelem.u1";
                     "stloc byteTemp";
                     "ldc.i4.1";
                     "newarr int8";
                     "stloc bytebuffer";
                     "ldloc bytebuffer";
                     "ldc.i4.0";
                     "ldloc byteTemp";
                     "stelem";
                     "ldloc bytebuffer";
                     "call System.Char[] [mscorlib]System.Text.ASCIIEncoding::GetChars(int8[])";
                     "ldc.i4.0";
                     "ldelem";
                     "call void [mscorlib]System.Console::Write(System.Char)"]

let ILInputBlock = ["";""]

let getILOpeningBracketBlock label destination = 
    ["";""]

let getILClosingBracketBlock label destination = 
    ["";""]
