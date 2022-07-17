package de.mhok.lox

case object Lox:
  var hasError = false

@main def runTest() =
  val expr = Expr.Binary(
    left = Expr.Unary(
      op = Token(TokenType.MINUS, "-", null, 0),
      expr = Expr.Literal(123),
    ),
    op = Token(TokenType.STAR, "*", null, 0),
    right = Expr.Grouping(
      expr = Expr.Literal(45.67)
    ),
  )
  println(expr)

@main def runFile(path: String) =
  run(io.Source.fromFile(path).mkString)
  if Lox.hasError then exit()

@main def runPrompt() =
  while true do
    print("> ")
    val line = io.StdIn.readLine()
    if line == null then exit()
    run(line)
    Lox.hasError = false

def run(source: String) =
  val scanner = Scanner(source)
  val tokens = scanner.scanTokens();
  for token <- tokens do println(token)

def error(line: Int, message: String) =
  report(line, "error", "", message)
  Lox.hasError = true

def report(line: Int, what: String, where: String, message: String) =
  println(s"[line $line] $what $where: $message")

def exit() = System.exit(if Lox.hasError then 65 else 0)
