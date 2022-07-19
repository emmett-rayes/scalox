package com.emmettrayes.lox

object Lox:
  var hasError = false
  var hasRuntimeError = false

  def error(line: Int, what: String, where: String, message: String) =
    println(s"[line $line] $what $where: $message")
    hasError = true

  def runtimeError(line: Int, what: String, where: String, message: String) =
    System.err.println(s"[line $line] $what $where: $message")
    hasRuntimeError = true

  def exit() =
    System.exit(if hasError then 65 else if hasRuntimeError then 70 else 0)

@main def runTest() =
  runFile("script.lox")

@main def runFile(path: String) =
  run(io.Source.fromFile(path).mkString)
  if Lox.hasError then Lox.exit()

@main def runPrompt() =
  while true do
    print("> ")
    val line = io.StdIn.readLine()
    if line == null then Lox.exit()
    run(line)
    Lox.hasError = false
    Lox.hasRuntimeError = false

def run(source: String): Unit =
  val scanner = Scanner(source)
  val tokens = scanner.scanTokens();
  val parser = Parser(tokens)
  val stmts = parser.parse()
  Interpreter.interpret(stmts)
