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
                    ".locals init (";
                    "    uint8[] tape, ";
                    "    int32 pointer, ";
                    "    uint8 byteTemp, ";
                    "    char charTemp, ";
                    "    char[] charBuffer, ";
                    "    uint8[] byteBuffer, ";
                    "    class [mscorlib]System.Text.Encoding encoder)";
                    "ldc.i4.s 640000";
                    "newarr int32";
                    "stloc tape";
                    "ldc.i4.0";
                    "stloc pointer";
                    "call class [mscorlib]System.Text.Encoding [mscorlib]System.Text.ASCIIEncoding::get_ASCII()";
                    "stloc encoder"]

let ILIncrementPointerBlock = ["ldloc pointer";
                               "ldc.i4.1";
                               "add";
                               "stloc pointer"]

let ILDecrementPointerBlock = ["ldloc pointer";
                               "ldc.i4.1";
                               "sub";
                               "stloc pointer"]

let private ILLoadPointerValueBlock = ["ldloc tape";
                                       "ldloc pointer";
                                       "ldelem.u1"]

let private ILStorePointerValueBlock = ["ldloc tape";
                                        "ldloc pointer";
                                        "ldloc byteTemp";
                                        "stelem.i1"]


let ILIncrementValueBlock = ILLoadPointerValueBlock @
                            ["ldc.i4.1";
                             "add";
                             "stloc byteTemp"] @
                            ILStorePointerValueBlock

let ILDecrementValueBlock = ILLoadPointerValueBlock @
                            ["ldc.i4.1";
                             "sub";
                             "stloc byteTemp"] @
                            ILStorePointerValueBlock

let ILOutputBlock = ILLoadPointerValueBlock @
                    ["stloc byteTemp";
                     "ldc.i4.1";
                     "newarr uint8";
                     "stloc byteBuffer";
                     "ldloc byteBuffer";
                     "ldc.i4.0";
                     "ldloc byteTemp";
                     "stelem.i1";
                     "ldloc encoder";
                     "ldloc byteBuffer";
                     "callvirt instance char[] [mscorlib]System.Text.ASCIIEncoding::GetChars(uint8[])";
                     "ldc.i4.0";
                     "ldelem.i2";
                     "call void [mscorlib]System.Console::Write(char)"]

let ILInputBlock = ["ldc.i4.1";
                    "call System.ConsoleKeyInfo [mscorlib] System.Console::ReadKey(bool)";
                    "callvirt instance char [mscorlib]System.ConsoleKeyInfo::get_KeyChar()";
                    "stloc charTemp";
                    "ldc.i4.1";
                    "newarr char";
                    "stloc charBuffer";
                    "ldloc charBuffer";
                    "ldc.i4.0";
                    "ldloc charTemp";
                    "stelem";
                    "ldloc encoder";
                    "ldloc charBuffer";
                    "callvirt instance uint8[] [mscorlib]System.Text.ASCIIEncoding::GetBytes(char[])";
                    "ldc.i4.0";
                    "ldElem.i1";
                    "stloc byteTemp"] @
                    ILStorePointerValueBlock

//  [  Jump forward past the matching ] if the byte at the pointer is zero.
let getILOpeningBracketBlock label destination = 
    ILLoadPointerValueBlock @
    ["brfalse " + destination;
     label + " :"]

//  ]  Jump backward to the matching [ unless the byte at the pointer is zero.
let getILClosingBracketBlock label destination = 
    ILLoadPointerValueBlock @
    ["brtrue " + destination;
     label + " :"]
