import std/tables

import chunk
import parser

type
  Local = object
    index: int
    isLifted: bool

  CodeGen* = ref object
    parent: CodeGen  # nil if toplevel
    assembly: Assembly
    chunk: Chunk
    function: Function
    locals: OrderedTable[string, Local]

  CompileError* = object of ValueError

proc newCodeGen*(assembly: Assembly, chunk: Chunk): CodeGen =
  CodeGen(assembly: assembly, chunk: chunk)

proc error(n: Node, msg: string) =
  raise newException(CompileError,
    "compile error on line " & $n.line & ": " & msg)

proc lineInfoFrom(g: CodeGen, n: Node) =
  g.chunk.currentLine = n.line

proc lookup(g: CodeGen, name: Node): tuple[ok: bool, i: int, lifted: bool] =

  # try to find a local in the current codegen
  if name.ident in g.locals:
    let local = g.locals[name.ident]
    result = (true, local.index, local.isLifted)

  # try to find and lift a local from a parent codegen
  elif not g.parent.isNil:
    let (ok, i, lifted) = g.parent.lookup(name)
    if ok:
      let ii = g.function.closesOver.find (i, lifted)
      result.ok = true
      result.lifted = true
      if ii == -1:
        result.i = g.function.closesOver.len
        g.function.closesOver.add (i, lifted)
      else:
        result.i = ii

proc genExpr*(g: CodeGen, n: Node, isTail: bool)

proc genNumber(g: CodeGen, n: Node) =

  g.lineInfoFrom n

  g.chunk.emitOpcode opcNumber
  g.chunk.emitFloat n.number

proc genIdent(g: CodeGen, n: Node) =

  g.lineInfoFrom n

  let (ok, index, isLifted) = g.lookup(n)
  if not ok:
    n.error("'" & n.ident & "' is undeclared in this scope")

  if isLifted:
    g.chunk.emitOpcode opcLiftedLocal
  else:
    g.chunk.emitOpcode opcLocal
  g.chunk.emitU16(uint16 index)

proc genPrefix(g: CodeGen, n: Node) =

  g.lineInfoFrom n

  g.genExpr(n.sons[1], isTail = false)
  case n.sons[0].ident
  of "-":
    g.chunk.emitOpcode opcNeg
  of "~":
    g.chunk.emitOpcode opcNot
  else: assert false

proc genInfix(g: CodeGen, n: Node) =

  g.lineInfoFrom n

  g.genExpr(n.sons[1], isTail = false)
  g.genExpr(n.sons[2], isTail = false)
  case n.sons[0].ident
  of "+": g.chunk.emitOpcode opcAdd
  of "-": g.chunk.emitOpcode opcSub
  of "*": g.chunk.emitOpcode opcMul
  of "/": g.chunk.emitOpcode opcDiv
  of "=": g.chunk.emitOpcode opcEq
  of "<>": g.chunk.emitOpcode opcNotEq
  of "<": g.chunk.emitOpcode opcLess
  of ">": g.chunk.emitOpcode opcGreater
  of "<=": g.chunk.emitOpcode opcLessEq
  of ">=": g.chunk.emitOpcode opcGreaterEq
  of "and": g.chunk.emitOpcode opcAnd
  of "or": g.chunk.emitOpcode opcOr
  else: assert false

proc genShow(g: CodeGen, n: Node) =

  g.lineInfoFrom n

  g.genExpr(n.sons[0], isTail = false)
  if n.ascii:
    g.chunk.emitOpcode opcShowAscii
  else:
    g.chunk.emitOpcode opcShow

proc genAsk(g: CodeGen, n: Node) =

  g.lineInfoFrom n

  if n.ascii:
    g.chunk.emitOpcode opcAskAscii
  else:
    g.chunk.emitOpcode opcAsk

proc genCall(g: CodeGen, n: Node, isTail: bool) =

  g.lineInfoFrom n

  for arg in n.sons:
    g.genExpr(arg, isTail = false)

  when defined(cflDisableTailCalls):
    g.chunk.emitOpcode opcCall
  else:
    if isTail:
      g.chunk.emitOpcode opcCallTail
    else:
      g.chunk.emitOpcode opcCall
  g.chunk.emitU8(uint8 n.sons.len - 1)

proc genFunction(g: CodeGen, n: Node) =

  g.lineInfoFrom n

  var
    chunk = newChunk()
    fn = Function(chunk: chunk, paramCount: n.params.len)
    fg = CodeGen(
      parent: g,
      assembly: g.assembly,
      chunk: chunk,
      function: fn,
    )
  for i, name in n.params:
    fg.locals[name] = Local(index: i)

  for i, expr in n.sons:
    let isTail = i == n.sons.high
    fg.genExpr(expr, isTail)
    fg.chunk.emitOpcode opcDiscard
  fg.chunk.emitOpcode opcReturn

  let id = g.assembly.functions.len
  g.assembly.functions.add fn

  g.chunk.emitOpcode opcClosure
  g.chunk.emitU16(uint16 id)

proc genExpr(g: CodeGen, n: Node, isTail: bool) =

  g.lineInfoFrom n

  case n.kind
  of nkNumber: g.genNumber(n)
  of nkIdent: g.genIdent(n)
  of nkPrefix: g.genPrefix(n)
  of nkInfix: g.genInfix(n)
  of nkShow: g.genShow(n)
  of nkAsk: g.genAsk(n)
  of nkCall: g.genCall(n, isTail)
  of nkFunction: g.genFunction(n)

proc genProgram*(g: CodeGen, n: Node) =

  g.genExpr(n, isTail = true)
  g.chunk.emitOpcode opcHalt

when isMainModule:
  import disassembler

  proc compile(s: string) =
    var
      ast = parse(s)
      chunk = newChunk()
      assembly = newAssembly(chunk)
      cg = newCodeGen(assembly, chunk)
    cg.genProgram(ast)

    echo "--- MAIN CHUNK ---"
    echo disassemble chunk

    for i, fn in assembly.functions:
      echo "--- FUNCTION ", i, " (arity: ", fn.paramCount, ") ---"
      echo "* closes over: ", fn.closesOver
      echo disassemble fn.chunk

  # basic arithmetic
  compile "1 + 2"
  compile "1 + 2 * 3"
  compile "? + ?"
  compile "(1 + ?)!"
  compile "??!!"

  # calls
  compile "1 < 2 : 3.14159265"
  compile "2 = 2 : 123"

  # basic functions
  compile "|x| { x * 2; } : 123"

  # closures
  # one level deep
  compile """
    |x| {
      |y| {
        (x * y)!;
      } : 456;
    } : 123
  """
  # two levels deep
  compile """
    |x| {
      |y| {
        |z| {
          (x * y * z)!;
        } : 789;
      } : 456;
    } : 123
  """

