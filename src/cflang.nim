# the cflang repl.
# check out /src/cflangpkg for the language implemenation.

import std/os
import std/parseopt
import std/rdstdin
import std/strutils

import cflangpkg/chunk
import cflangpkg/codegen
import cflangpkg/parser
import cflangpkg/vm

proc vmConfig(): VmOptions =
  VmOptions(
    askImpl: proc (): float {.raises: [].} =
      try:
        result = stdin.readLine.parseFloat
      except:
        result = 0,
    askAsciiImpl: proc (): float {.raises: [].} =
      try:
        result = stdin.readChar.float
      except:
        result = 0,
    showImpl: proc (s: string) = echo(s),
    showAsciiImpl: proc (s: string) {.raises: [].} =
      try:
        stdout.write(s)
      except:
        echo(s),
  )

proc interpret(source: string): Value =
  var
    ast = parse(source)
    chunk = newChunk()
    assembly = newAssembly(chunk)
    cg = newCodeGen(assembly, chunk)
  cg.genProgram(ast)

  when defined(disassemble):
    echo "--- MAIN CHUNK ---"
    echo disassemble chunk

    for i, fn in assembly.functions:
      echo "--- FUNCTION ", i, " (arity: ", fn.paramCount, ") ---"
      echo "* closes over: ", fn.closesOver
      echo disassemble fn.chunk

  result = interpret(assembly, chunk, vmConfig())

proc repl() =

  const NimblePkgVersion {.strdefine.} = "<not compiled with nimble>"

  stderr.writeLine "cflang, version ", NimblePkgVersion
  stderr.writeLine "copyright (C) lqdev, 2021 - " &
    "https://github.com/liquidev/cflang"

  while true:
    let line =
      try: readLineFromStdin("> ")
      except IOError: break
    try:
      echo "< ", interpret(line)
    except ValueError as e:
      echo "! ", e.msg

var inputFilename: string

# just for the future, when CLI options get added.
for kind, key, value in getopt(commandLineParams()):
  assert kind == cmdArgument, "no options are available"
  inputFilename = key

if inputFilename.len == 0:
  repl()
else:
  try:
    discard interpret(readFile(inputFilename))
  except ValueError as e:
    echo "! ", e.msg
    quit 1
