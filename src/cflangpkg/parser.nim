import std/algorithm
import std/strscans
import std/strutils

import npeg

import lexer

type
  NodeKind* = enum
    # literals
    nkNumber, nkIdent
    # operations
    nkPrefix, nkInfix
    nkShow, nkAsk
    # calls
    nkCall, nkFunction

  Node* = ref object
    line*: int
    case kind*: NodeKind
    of nkNumber: number*: float
    of nkIdent: ident*: string
    of nkCall: done: bool
    of nkShow, nkAsk: ascii*: bool
    of nkFunction:
      paramsDone, bodyDone: bool
      params*: seq[string]
    else: discard
    sons*: seq[Node]

  ParseState = object
    stack: seq[Node]

  ParseError* = object of ValueError

proc `$`*(node: Node): string =

  case node.kind
  of nkNumber: result = "Number " & $node.number
  of nkIdent: result = "Ident " & node.ident
  else:
    result = ($node.kind)[2..^1]
    if node.kind == nkFunction:
      result.add(" |")
      result.add(node.params.join(", "))
      result.add('|')
    var children = ""
    for i, child in node.sons:
      children.add('\n' & $child)
    result.add(children.indent(2))

proc `$`*(nodes: seq[Node]): string =

  for node in nodes:
    result.add "---\n"
    result.add $node
    result.add "\n"

proc `==`(t: Token, k: TokenKind): bool =
  t.kind == k

proc repr*(t: Token): string = $t

proc top(s: ParseState): Node =
  s.stack[^1]

proc push(s: var ParseState, n: Node) =
  s.stack.add n
#   echo "-> ", n
#   echo s.stack

proc pop(s: var ParseState): Node =
  result = pop s.stack
#   echo "<- ", result
#   echo s.stack

let parser = peg(program, Token, p: ParseState):

  number <- >[tkNumber]:
    p.push Node(
      line: ($1).line,
      kind: nkNumber,
      number: ($1).number
    )
  ident <- >[tkIdent]:
    p.push Node(
      line: ($1).line,
      kind: nkIdent,
      ident: move ($1).ident,
    )

  unaryOp <- [tkMinus] | [tkNot]
  unary <- >unaryOp * prefix:
    let a = pop p
    p.push Node(
      line: ($1).line,
      kind: nkPrefix,
      sons: @[Node(kind: nkIdent, ident: $($1).kind), a]
    )

  ask <- >([tkAsk] | [tkAsk2]):
    p.push Node(
      line: ($1).line,
      kind: nkAsk,
      ascii: ($1).kind == tkAsk2,
    )

  rparen <- [tkRParen] | E"missing right paren"
  paren <- ([tkLParen] * expr * rparen) ^ 0

  semi <- [tkSemi] | E"missing semicolon after expression"
  pipe <- [tkPipe] | E"missing pipe to close argument list"
  rbrace <- [tkRBrace] | E"missing right curly brace"
  functionParams <- [tkPipe] * ?(ident * *([tkComma] * ident)) * pipe
  functionBody <- ([tkLBrace] * *(expr * semi) * rbrace) ^ 0

  functionOp <- &([tkPipe] | [tkLBrace]):
    p.push Node(
      line: ($0).line,
      kind: nkFunction,
      paramsDone: false,
      bodyDone: false
    )

  functionHead <- functionOp * ?functionParams:
    var params: seq[string]
    while not (p.top.kind == nkFunction and not p.top.paramsDone):
      params.add(move p.pop.ident)
    reverse params
    p.top.params = move params
    p.top.paramsDone = true

  function <- functionHead * functionBody:
    var exprs: seq[Node]
    while not (p.top.kind == nkFunction and not p.top.bodyDone):
      exprs.add p.pop
    reverse exprs
    p.top.sons = move exprs
    p.top.bodyDone = true

  prefix <- unary | ask | paren | function | number | ident

  binary <-
    >([tkStar] | [tkSlash]) * expr ^ 5 |
    >([tkPlus] | [tkMinus]) * expr ^ 4 |
    >([tkEq] | [tkNotEq] |
      [tkLess] | [tkGreater] |
      [tkLessEq] | [tkGreaterEq]) * expr ^ 3 |
    >([tkAnd] | [tkOr]) * expr ^ 2:

      let (right, left) = (p.pop, p.pop)
      p.push Node(
        line: ($1).line,
        kind: nkInfix,
        sons: @[Node(kind: nkIdent, ident: $($1).kind), left, right]
      )

  show <- >([tkShow] | [tkShow2]):
    let a = pop p
    p.push Node(
      line: ($1).line,
      kind: nkShow,
      ascii: ($1).kind == tkShow2,
      sons: @[a],
    )

  callOp <- >[tkColon]:
    p.push Node(
      line: ($1).line,
      kind: nkCall,
      done: false,
      sons: @[pop p],
    )
  callArgs <- expr * *([tkComma] * expr)
  call <- (callOp * ?callArgs) ^ 1:
    var args: seq[Node]
    while not (p.top.kind == nkCall and not p.top.done):
      args.add p.pop
    reverse args
    p.top.sons.add(args)
    p.top.done = true

  infix <- binary | call | show

  expr <- prefix * *infix

  program <- expr * !1

proc parse*(s: string): Node =

  var state: ParseState

  let tokens = s.tokenize
  try:
    let match = parser.match(tokens, state)
    if not match.ok:
      var
        errtok = tokens[match.matchLen]
        msg = "unexpected token at line " & $errtok.line & ": " & $errtok
      raise newException(ParseError, move msg)
  except NPegException as e:
    var
      i: int
      err: string
    if scanf(e.msg, "Parsing error at #$i: expected \"$+\"", i, err):
      var
        errtok = tokens[e.matchLen]
        msg = "syntax error near line " & $errtok.line & ": " & err
      raise newException(ParseError, move msg)
    else:
      raise

  assert state.stack.len == 1,
    "parser bug: stray nodes were left on the stack: " & $state.stack
  result = state.stack[0]

when isMainModule:

  echo parse """
    |a| {} : |b| {}
  """

  echo parse """
    call : call, ??
  """

  echo parse """
    (char <> 0) : {
      self : self, ??;
    } : {
      33!!;
    }
  """

  echo parse """
    |echo| {
      echo : echo, ??;
    }
    : |self, char| {
      (char <> 0) : {
        char!!;
        self : self, ??;
      };
    }
  """

  echo parse """
    |loop| {
      loop: 1, 10, 1, |x| {
        x!;
      };
    }

    -- loop
    : |min, max, stride, callback| {
      |aux| {
        aux: aux, min;
      }
      -- aux
      : |aux, i| {
        (i <= max) : {
          callback: i;
          aux: aux, i + stride;
        };
      };
    }
  """
