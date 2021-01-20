## The cflang lexer using NPeg.

# i'd say that unlike for parsing grammars into ASTs, npeg really shines when
# it comes to parsing grammars into tokens.

import std/strutils
import std/tables

import npeg

type
  TokenKind* = enum
    # literals
    tkNumber = "number"
    tkIdent = "ident"

    # symbols
    tkPlus = "+", tkMinus = "-", tkStar = "*", tkSlash = "/"
    tkEq = "=", tkNotEq = "<>"
    tkLess = "<", tkGreater = ">"
    tkLessEq = "<=", tkGreaterEq = ">="
    tkNot = "~", tkAnd = "and", tkOr = "or"
    tkColon = ":", tkPipe = "|"
    tkShow = "!", tkShow2 = "!!"
    tkAsk = "?", tkAsk2 = "??"
    tkLParen = "(", tkRParen = ")"
    tkLBrace = "{", tkRBrace = "}"
    tkComma = ",", tkSemi = ";"

  Token* = object
    ## A token.

    line*: int
    case kind*: TokenKind
    of tkNumber: number*: float
    of tkIdent: ident*: string
    else: discard

  Lexer = object
    ## Lexer state. This keeps track of the current line to know where to
    ## report errors.

    t: seq[Token]
    line: int

  LexError* = object of ValueError

const
  symbolTable = block:
    var t: Table[string, TokenKind]
    for tk in tkPlus..TokenKind.high:
      t[$tk] = tk
    t

proc add(l: var Lexer, number: float) =
  l.t.add Token(line: l.line, kind: tkNumber, number: number)

proc add(l: var Lexer, ident: string) =
  l.t.add Token(line: l.line, kind: tkIdent, ident: ident)

proc add(l: var Lexer, kind: TokenKind) =
  l.t.add Token(line: l.line, kind: kind)

let lex = peg("tokens", l: Lexer):

  number <- +Digit * ?('.' * +Digit) * ?({'e', 'E'} * ?{'+', '-'} * +Digit):
    l.add parseFloat($0)

  ident <- (Alpha | '_') * *(Alnum | '_'):
    l.add $0

  opArithmetic <- '+' | '-' | '*' | '/'
  opEquality <- '=' | "<>" | "<=" | ">=" | '<' | '>'
  opLogic <- '~' | "and" | "or"
  opCall <- '|' | ':'
  opIO <- "!!" | "??" | '!' | '?'
  punct <- '(' | ')' | '{' | '}' | ',' | ';'

  operator <- opArithmetic | opEquality | opLogic | opCall | opIO | punct:
    l.add symbolTable[$0]

  linebreak <- '\n':
    inc l.line
  whitespace <- +(Blank | linebreak | '\r')

  comment <- "--" * *(1 - '\n')

  ignored <- +(whitespace | comment)

  token <- number | operator | ident
  tokens <- ?ignored * +(token * ?ignored) * !1

proc tokenize*(s: string): seq[Token] =
  ## Tokenizes the given string into cflang tokens.

  var state = Lexer(line: 1)

  let match = lex.match(s, state)
  if not match.ok:
    var msg =
      "unexpected character on line " & $state.line & ": " &
      escape($s[match.matchLen - 1], "'", "'")
    raise newException(LexError, move msg)

  result = move state.t

proc `$`*(t: Token): string =
  ## Stringifies a token.

  case t.kind
  of tkNumber:
    result.addFloat t.number
  of tkIdent:
    result.add t.ident
  else:
    result.add $t.kind

proc `$`*(t: seq[Token]): string =
  ## Stringifies a seq of tokens. This is mostly used for debugging purposes.

  for tok in t:
    result.addSep("\p", 1)
    result.addInt(tok.line)
    result.add(": ")
    result.add($tok)

when isMainModule:

  let l = tokenize """
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
  echo l
