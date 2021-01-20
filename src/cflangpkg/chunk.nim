## VM opcodes and bytecode chunks.

import std/bitops

type
  Opcode* = enum
    ## A VM opcode.

    opcNumber       ## push a number onto the stack
    opcClosure      ## push a closure onto the stack
    opcLocal        ## copy a local onto the top of the stack
    opcLiftedLocal  ## copy a lifted local onto the top of the stack
    opcDiscard      ## discard the value at the top of the stack

    # arithmetic and logic operations. these are self-explanatory.
    opcNeg, opcAdd, opcSub, opcMul, opcDiv
    opcEq, opcNotEq, opcLess, opcGreater, opcLessEq, opcGreaterEq
    opcNot, opcAnd, opcOr

    # ask/show operations
    opcAsk, opcAskAscii
    opcShow, opcShowAscii

    opcCall      ## call a closure with n arguments
    opcCallTail  ## tail call a closure with n arguments
    opcReturn    ## return from a closure

    opcHalt  ## halt the VM

  Function* = ref object
    ## A function. This can be considered a "bare" closure, or a prototype.
    chunk*: Chunk
    paramCount*: int
    closesOver*: seq[tuple[i: int, lifted: bool]]  # indices of locals

  Assembly* = ref object
    ## An assembly. This stores all functions that need to be accessed by
    ## the interpreter, as well as the program's main chunk.
    functions*: seq[Function]
    main*: Chunk

  Chunk* = ref object
    ## A chunk of bytecode with line information.
    bytecode*: seq[uint8]
    lineInfo*: seq[int]
    currentLine*: int  # this is a piece of state used when emitting bytecode
                       # to know what line should be added into lineInfo

proc newAssembly*(main: Chunk): Assembly =
  ## Creates a new assembly with the given main chunk.
  Assembly(main: main)

proc newChunk*(): Chunk =
  ## Creates a new, empty chunk of bytecode.
  Chunk()

proc addLineInfo(chunk: Chunk, n: int) =
  ## Adds n line info to the chunk.

  for _ in 1..n:
    chunk.lineInfo.add chunk.currentLine

proc emitU8*(chunk: Chunk, x: uint8) =
  ## Emits an 8-bit unsigned int.

  chunk.addLineInfo(x.sizeof)
  chunk.bytecode.add(x)

proc emitU16*(chunk: Chunk, x: uint16) =
  ## Emits a 16-bit unsigned int.

  chunk.addLineInfo(x.sizeof)
  chunk.bytecode.add([
    uint8 x.bitsliced(0..<8),
    uint8 x.bitsliced(8..<16)
  ])

proc emitU32*(chunk: Chunk, x: uint32) =
  ## Emits a 32-bit unsigned int.

  chunk.addLineInfo(x.sizeof)
  chunk.bytecode.add([
    uint8 x.bitsliced(0..<8),
    uint8 x.bitsliced(8..<16),
    uint8 x.bitsliced(16..<24),
    uint8 x.bitsliced(24..<32),
  ])

proc emitU64*(chunk: Chunk, x: uint64) =
  ## Emits a 64-bit unsigned int.

  chunk.addLineInfo(x.sizeof)
  chunk.bytecode.add([
    uint8 x.bitsliced(0..<8),
    uint8 x.bitsliced(8..<16),
    uint8 x.bitsliced(16..<24),
    uint8 x.bitsliced(24..<32),
    uint8 x.bitsliced(32..<40),
    uint8 x.bitsliced(40..<48),
    uint8 x.bitsliced(48..<56),
    uint8 x.bitsliced(56..<64),
  ])

proc emitOpcode*(chunk: Chunk, op: Opcode) =
  ## Emits an opcode.
  chunk.emitU8(op.uint8)

proc emitFloat*(chunk: Chunk, f: float) =
  ## Emits a 64-bit float.
  chunk.emitU64(cast[uint64](f))

proc readU8*(chunk: Chunk, i: var int): uint8 =
  ## Reads an 8-bit unsigned int from the chunk. `i` is incremented by the
  ## uint's size.

  result = chunk.bytecode[i]
  inc i, uint8.sizeof

proc readU16*(chunk: Chunk, i: var int): uint16 =
  ## Reads a 16-bit unsigned int from the chunk. `i` is incremented by the
  ## uint's size.

  result =
    chunk.bytecode[i].uint16 or
    (chunk.bytecode[i + 1].uint16 shl 8)
  inc i, uint16.sizeof

proc readU32*(chunk: Chunk, i: var int): uint32 =
  ## Reads a 32-bit unsigned int from the chunk. `i` is incremented by the
  ## uint's size.

  result =
    chunk.bytecode[i].uint32 or
    (chunk.bytecode[i + 1].uint32 shl 8) or
    (chunk.bytecode[i + 2].uint32 shl 16) or
    (chunk.bytecode[i + 3].uint32 shl 24)
  inc i, uint32.sizeof

proc readU64*(chunk: Chunk, i: var int): uint64 =
  ## Reads a 64-bit unsigned int from the chunk. `i` is incremented by the
  ## uint's size.

  result =
    chunk.bytecode[i].uint64 or
    (chunk.bytecode[i + 1].uint64 shl 8) or
    (chunk.bytecode[i + 2].uint64 shl 16) or
    (chunk.bytecode[i + 3].uint64 shl 24) or
    (chunk.bytecode[i + 4].uint64 shl 32) or
    (chunk.bytecode[i + 5].uint64 shl 40) or
    (chunk.bytecode[i + 6].uint64 shl 48) or
    (chunk.bytecode[i + 7].uint64 shl 56)
  inc i, uint64.sizeof

proc readOpcode*(chunk: Chunk, i: var int): Opcode =
  ## Reads an opcode from the chunk. `i` is incremented by the opcode's size.

  result = chunk.bytecode[i].Opcode
  inc i

proc readFloat*(chunk: Chunk, i: var int): float =
  ## Reads a 64-bit float from the chunk. `i` is incremented by the float's
  ## size.

  cast[float](chunk.readU64(i))
