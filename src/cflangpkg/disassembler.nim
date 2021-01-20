## A very rudimentary disassembler for debugging purposes.

import std/strutils

import chunk

proc disassemble*(chunk: Chunk): string =
  ## Disassembles the bytecode in the given chunk into a human-readable string.

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
