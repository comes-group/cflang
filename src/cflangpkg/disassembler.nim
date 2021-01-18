import std/strutils

import chunk

proc disassemble*(chunk: Chunk): string =

  # just a very rudimentary disassembler. nothing special.

  var pc = 0
  while pc < chunk.bytecode.len:

    result.add('[' & align($pc, 4) & "] ")

    let opcode = chunk.readOpcode(pc)
    result.add(($opcode)[3..^1])
    result.add(' ')

    case opcode
    of opcNumber:
      result.addFloat(chunk.readFloat(pc))
    of opcClosure, opcLocal, opcLiftedLocal:
      result.add($chunk.readU16(pc))
    of opcCall, opcCallTail:
      result.add($chunk.readU8(pc))
    of opcDiscard..opcShowAscii, opcReturn, opcHalt: discard

    result.add('\n')
