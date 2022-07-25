package com.emmettrayes.lox

import scala.util.Using

object Lox:
  var hasError = false
  var hasRuntimeError = false

  def error(line: Int, what: String, where: String, message: String): Unit =
    println(s"[line $line] $what $where: $message")
    hasError = true

  def runtimeError(
      line: Int,
      what: String,
      where: String,
      message: String,
  ): Unit =
    System.err.nn.println(s"[line $line] $what $where: $message")
    hasRuntimeError = true

  def exit(): Unit =
    System.exit(if hasError then 65 else if hasRuntimeError then 70 else 0)

@main def runTest(): Unit =
  runFile("script.lox")

@main def runFile(path: String): Unit =
  Using(io.Source.fromFile(path)) { reader =>
    run(reader.mkString)
  }
  if Lox.hasError then Lox.exit()

@main def runPrompt(): Unit =
  while true do
    print("> ")
    val line: String | Null = io.StdIn.readLine()
    if line == null then Lox.exit()
    else run(line)
    Lox.hasError = false
    Lox.hasRuntimeError = false

def run(source: String): Unit =
  val scanner = Scanner(source)
  val tokens = scanner.scanTokens()
  val parser = Parser(tokens)
  val stmts = parser.parse()
  Interpreter.interpret(stmts)
