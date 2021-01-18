import std/bitops

type
  Opcode* = enum
    opcNumber
    opcClosure
    opcLocal
    opcLiftedLocal
    opcDiscard

    opcNeg, opcAdd, opcSub, opcMul, opcDiv
    opcEq, opcNotEq, opcLess, opcGreater, opcLessEq, opcGreaterEq
    opcNot, opcAnd, opcOr

    opcAsk, opcAskAscii
    opcShow, opcShowAscii

    opcCall
    opcCallTail
    opcReturn

    opcHalt

  Function* = ref object
    chunk*: Chunk
    paramCount*: int
    closesOver*: seq[tuple[i: int, lifted: bool]]  # indices of locals

  Assembly* = ref object
    functions*: seq[Function]
    main*: Chunk

  Chunk* = ref object
    bytecode*: seq[uint8]
    lineInfo*: seq[int]
    currentLine*: int

proc newAssembly*(main: Chunk): Assembly =
  Assembly(main: main)

proc newChunk*(): Chunk =
  Chunk()

proc addLineInfo(chunk: Chunk, n: int) =

  for _ in 1..n:
    chunk.lineInfo.add chunk.currentLine

proc emitU8*(chunk: Chunk, x: uint8) =

  chunk.addLineInfo(x.sizeof)
  chunk.bytecode.add(x)

proc emitU16*(chunk: Chunk, x: uint16) =

  chunk.addLineInfo(x.sizeof)
  chunk.bytecode.add([
    uint8 x.bitsliced(0..<8),
    uint8 x.bitsliced(8..<16)
  ])

proc emitU32*(chunk: Chunk, x: uint32) =

  chunk.addLineInfo(x.sizeof)
  chunk.bytecode.add([
    uint8 x.bitsliced(0..<8),
    uint8 x.bitsliced(8..<16),
    uint8 x.bitsliced(16..<24),
    uint8 x.bitsliced(24..<32),
  ])

proc emitU64*(chunk: Chunk, x: uint64) =

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
  chunk.emitU8(op.uint8)

proc emitFloat*(chunk: Chunk, f: float) =
  chunk.emitU64(cast[uint64](f))

proc readU8*(chunk: Chunk, i: var int): uint8 =

  result = chunk.bytecode[i]
  inc i, uint8.sizeof

proc readU16*(chunk: Chunk, i: var int): uint16 =

  result =
    chunk.bytecode[i].uint16 or
    (chunk.bytecode[i + 1].uint16 shl 8)
  inc i, uint16.sizeof

proc readU32*(chunk: Chunk, i: var int): uint32 =

  result =
    chunk.bytecode[i].uint32 or
    (chunk.bytecode[i + 1].uint32 shl 8) or
    (chunk.bytecode[i + 2].uint32 shl 16) or
    (chunk.bytecode[i + 3].uint32 shl 24)
  inc i, uint32.sizeof

proc readU64*(chunk: Chunk, i: var int): uint64 =

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

  result = chunk.bytecode[i].Opcode
  inc i

proc readFloat*(chunk: Chunk, i: var int): float =
  cast[float](chunk.readU64(i))
