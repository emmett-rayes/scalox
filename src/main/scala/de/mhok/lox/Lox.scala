package de.mhok.lox

object Lox:
  var hasError = false

  def error(line: Int, what: String, where: String, message: String) =
    println(s"[line $line] $what $where: $message")
    hasError = true

  def exit() = System.exit(if hasError then 65 else 0)

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
  if Lox.hasError then Lox.exit()

@main def runPrompt() =
  while true do
    print("> ")
    val line = io.StdIn.readLine()
    if line == null then Lox.exit()
    run(line)
    Lox.hasError = false

def run(source: String): Unit =
  val scanner = Scanner(source)
  val tokens = scanner.scanTokens();
  val parser = Parser(tokens)
  val expr = parser.parse()
  expr match
    case Some(e) => println(e)
    case None => return 
