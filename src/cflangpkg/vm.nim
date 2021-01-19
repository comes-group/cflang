import std/math
import std/strutils

import chunk

type
  Closure = ref object
    fn: Function
    lifted: seq[Value]

  ValueKind* = enum
    vkNumber
    vkClosure

  Value* = object
    case kind*: ValueKind
    of vkNumber: number*: float
    of vkClosure: closure: Closure

  VmOptions* = object
    askImpl*: proc (): float {.raises: [].}
    askAsciiImpl*: proc (): float {.raises: [].}
    showImpl*, showAsciiImpl*: proc (s: string) {.raises: [].}

  InterpretError* = object of ValueError

proc `$`*(v: Value): string =

  case v.kind
  of vkNumber:
    if v.number.trunc == v.number: result = $v.number.int
    else: result = $v.number
  of vkClosure: result = "<fn 0x" & $cast[int](v.closure).toHex(8) & ">"

proc ascii(v: Value): string =
  $char(v.number)

proc add(s: var seq[Value], v: Value) =
  system.add(s, v)
  when defined(cflDebugStack):
    echo "add: ", v, " -> ", s

proc pop(s: var seq[Value]): Value =
  result = system.pop(s)
  when defined(cflDebugStack):
    echo "pop: ", result, " <- ", s

proc setLen(s: var seq[Value], l: int) =
  when defined(cflDebugStack):
    echo "setLen: ", s.len, " => ", l
  system.setLen(s, l)

proc interpret*(assembly: Assembly, chunk: Chunk, opts: VmOptions): Value =

  type
    StackFrame = object
      chunk: Chunk
      pc: int
      stackBottom: int
      closure: Closure
      isTailCall: bool

  var
    chunk = chunk
    pc = 0
    stack: seq[Value]
    stackBottom = 0
    closure: Closure
    callStack: seq[StackFrame]
    isTailCall = false
    tailCallArgs: seq[Value]

  while true:
    {.computedGoto.}

    let errpc = pc

    template error(msg: string) =

      let line = chunk.lineInfo[errpc]
      raise newException(InterpretError,
        "interpret error at line " & $line & ": " & msg)

    template iassert(cond: bool, msg: string) =

      if not cond:
        error msg

    template inplBinN(op: untyped, name: static string) =

      const errmsg = "binary " & name & " operands must be numbers"
      let b = pop stack
      iassert b.kind == vkNumber and stack[^1].kind == vkNumber, errmsg
      op(stack[^1].number, b.number)

    template binNB(op: untyped, name: static string) =

      const errmsg = "binary " & name & " operands must be numbers"
      let b = pop stack
      iassert b.kind == vkNumber and stack[^1].kind == vkNumber, errmsg
      stack[^1].number = float(op(stack[^1].number, b.number))

    template binB(op: untyped, name: static string) =

      const errmsg = "binary " & name & " operands must be numbers"
      let (b, a) = (pop stack, pop stack)
      iassert a.kind == vkNumber and b.kind == vkNumber, errmsg
      stack.add Value(
        kind: vkNumber,
        number: float(op(a.number != 0, b.number != 0)),
      )

    template storeFrame() =

      callStack.add(StackFrame(
        chunk: chunk,
        pc: pc,
        stackBottom: stackBottom,
        closure: closure,
        isTailCall: isTailCall
      ))

    template popFrame() =

      let f = pop callStack
      chunk = f.chunk
      pc = f.pc
      stackBottom = f.stackBottom
      closure = f.closure
      isTailCall = f.isTailCall

    template callPrelude(argc: int) =

      stackBottom = stack.high - argc + 1

      let
        callee {.inject.} = stack[stackBottom - 1]
        paramc =
          case callee.kind
          of vkNumber: 1
          of vkClosure: callee.closure.fn.paramCount
        argsBottom = stack.len - argc
      # adjust the number of arguments to the number of parameters.
      # due to how seqs work, any missing params receive 0s
      stack.setLen(argsBottom + paramc)

    template goToClosure(cl: Closure) =
      closure = cl
      chunk = cl.fn.chunk
      pc = 0

    template doReturn(v: Value) =
      # remove the params *and* the closure from the stack
      stack.setLen(stackBottom - 1)
      stack.add(v)
      popFrame()

    let opcode = chunk.readOpcode(pc)
    when defined(cflDebugOpcodes):
      echo '[', cast[int](chunk).toHex(8), ']', " [", errpc, "] ", opcode
    case opcode

    of opcNumber:
      stack.add Value(kind: vkNumber, number: chunk.readFloat(pc))

    of opcClosure:
      let fn = assembly.functions[chunk.readU16(pc)]
      var cl = Closure(fn: fn)
      for i, local in fn.closesOver:
        if local.lifted:
          cl.lifted.add(closure.lifted[local.i])
        else:
          cl.lifted.add(stack[stackBottom + local.i])
      stack.add Value(kind: vkClosure, closure: cl)

    of opcLocal:
      stack.add stack[stackBottom + chunk.readU16(pc).int]

    of opcLiftedLocal:
      stack.add closure.lifted[chunk.readU16(pc)]

    of opcDiscard:
      stack.setLen stack.len - 1

    of opcNeg:
      iassert stack[^1].kind == vkNumber, "unary - can only be used on numbers"
      stack[^1].number = -stack[^1].number

    of opcNot:
      iassert stack[^1].kind == vkNumber, "unary - can only be used on numbers"
      stack[^1].number = float(not bool(stack[^1].number))

    of opcAdd: inplBinN(`+=`, "+")
    of opcSub: inplBinN(`-=`, "-")
    of opcMul: inplBinN(`*=`, "*")
    of opcDiv: inplBinN(`/=`, "/")
    of opcEq: binNB(`==`, "==")
    of opcNotEq: binNB(`!=`, "<>")
    of opcLess: binNB(`<`, "<")
    of opcGreater: binNB(`>`, ">")
    of opcLessEq: binNB(`<=`, "<=")
    of opcGreaterEq: binNB(`>=`, ">=")
    of opcAnd: binB(`and`, "and")
    of opcOr: binB(`or`, "or")

    of opcAsk:
      stack.add Value(kind: vkNumber, number: opts.askImpl())
    of opcAskAscii:
      stack.add Value(kind: vkNumber, number: opts.askAsciiImpl())

    of opcShow:
      opts.showImpl($stack[^1])

    of opcShowAscii:
      let x = stack[^1]
      iassert x.kind == vkNumber, "only numbers can be ASCII-shown"
      opts.showAsciiImpl(x.ascii)

    of opcCall, opcCallTail:

      let argc = chunk.readU8(pc).int
      # store the frame *after* reading the operand
      # also, in a tail call we only want to store the frame at the very
      # first tail call to know where to return to
      if opcode == opcCallTail:
        if not isTailCall:
          storeFrame()
          isTailCall = true
        else:
          # we need to temporarily move the arguments off the stack to clean
          # up the old call
          let nval = argc + 1
          tailCallArgs.setLen(nval)
          for i in 0..<nval:
            tailCallArgs[i] = stack[stack.len - nval + i]
          # now we can clean up the old call, kind of like we do in opcReturn
          stack.setLen(stackBottom - 1)
          # and copy our arguments back onto the stack
          stack.add(tailCallArgs)
      else:
        storeFrame()
      callPrelude(argc)  # declares `callee`

      case callee.kind
      of vkNumber:
        let action = stack[stackBottom]
        iassert action.kind == vkClosure,
          "the parameter to an if-call must be a function"
        if callee.number != 0:
          goToClosure action.closure
        else:
          # the result of an unsuccessful if-call is always 1 to allow for
          # an else branch
          doReturn Value(kind: vkNumber, number: 1)
      of vkClosure:
        goToClosure callee.closure

    of opcReturn:
      # the result of a function call is always 0.
      doReturn Value(kind: vkNumber, number: 0)

    of opcHalt: break

  assert stack.len == 1, "one result value must be left on the stack"
  result = stack[0]

when isMainModule:

  import std/strutils
  import std/terminal

  import codegen
  import disassembler
  import parser

  proc run(s: string) =
    var
      ast = parse(s)
      chunk = newChunk()
      assembly = newAssembly(chunk)
      cg = newCodeGen(assembly, chunk)
    cg.genProgram(ast)

    when defined(disassemble):
      echo "--- MAIN CHUNK ", cast[int](chunk).toHex(8), " ---"
      echo disassemble chunk

      for i, fn in assembly.functions:
        echo "--- FUNCTION ", i, " (arity: ", fn.paramCount, ") ",
             cast[int](fn.chunk).toHex(8), " ---"
        echo "* closes over: ", fn.closesOver
        echo disassemble fn.chunk

    discard interpret(assembly, chunk, VmOptions(
      askImpl: proc (): float {.raises: [].} =
        try:
          result = stdin.readLine.parseFloat
        except:
          result = 0,
      askAsciiImpl: proc (): float {.raises: [].} =
        try:
          result = getch().float
        except:
          result = 0,
      showImpl: proc (s: string) = echo(s),
      showAsciiImpl: proc (s: string) {.raises: [].} =
        try:
          stdout.write(s)
        except:
          echo(s),
    ))

#   run "(2 + 2)!"
#   run "33!!"
# #   run "?!"
#   run "(1 = 1)!"

#   run "|x| { x * 2; }!"
#   run "{}:"
#   run "|x, y| { (x * y)!; } : 10, 2"
#   run "(1 = 1) : { 10!; } : { 20!; }"
#   run """
#     |x, y, z| {
#       |a, b, c| {
#         |i, j, k| {
#           (x * a * i)!;
#           (y * b * j)!;
#           (z * c * k)!;
#         } : 7, 8, 9;
#       } : 4, 5, 6;
#     } : 1, 2, 3
#   """
#   run """
#     |loop| {
#       loop: loop, 0, |x| { x!; };
#     }
#     : |loop, x, yield| {
#       (x < 10) : {
#         yield: x;
#         loop: loop, x + 1, yield;
#       };
#     }
#   """
  run """
    |fac| {
      fac: 10, |x| { x!; };
    }
    : |n, yield| {
      |aux| {
        aux: aux, 1, 1;
      }
      : |aux, i, result| {
        ~(i <= n) : {
          yield: result;
        } : {
          aux: aux, i + 1, result * i;
        };
      };
    }
  """

  echo getOccupiedMem()
