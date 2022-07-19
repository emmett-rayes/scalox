package com.emmettrayes.lox

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Parser:
  class ParseError extends RuntimeException

  def error(token: Token, message: String): ParseError =
    token.ttype match
      case TokenType.EOF =>
        Lox.error(token.line, "parse error", "at file end", message)
      case _ =>
        Lox.error(token.line, "parse error", s"at ${token.lexeme}", message)
    ParseError()

class Parser(val tokens: List[Token]):
  private var current = 0

  def parse(): List[Stmt] =
    @tailrec
    def addStmt(acc: Seq[Stmt]): Seq[Stmt] =
      val stmtOpt =
        try Some(declaration())
        catch
          case e: Parser.ParseError =>
            synchronize()
            None
      val token = tokens(current)
      val stmts = stmtOpt match
        case None       => acc
        case Some(stmt) => acc :+ stmt
      token.ttype match
        case TokenType.EOF => stmts
        case _             => addStmt(stmts)
    return addStmt(Seq.empty).toList

  // parsers
  private def declaration(): Stmt =
    val token = tokens(current)
    token.ttype match
      case TokenType.VAR =>
        advance()
        varDecl()
      case _ =>
        statement()

  private def statement(): Stmt =
    val token = tokens(current)
    token.ttype match
      case TokenType.PRINT =>
        advance()
        printStmt()
      case TokenType.LEFT_BRACE =>
        advance()
        block()
      case _ =>
        exprStmt()

  private def printStmt(): Stmt =
    val expr = expression()
    val token = tokens(current)
    token.ttype match
      case TokenType.SEMICOLON => advance()
      case _ => throw Parser.error(token, "expecting ';' after expression")
    Stmt.PrintStmt(expr)

  private def exprStmt(): Stmt =
    val expr = expression()
    val token = tokens(current)
    token.ttype match
      case TokenType.SEMICOLON => advance()
      case _ => throw Parser.error(token, "expecting ';' after expression")
    Stmt.ExprStmt(expr)

  private def varDecl(): Stmt =
    val ident = tokens(current)
    ident.ttype match
      case TokenType.IDENTIFIER => advance()
      case _ => throw Parser.error(ident, "expecting a variable name")
    val next = tokens(current)
    val init = next.ttype match
      case TokenType.EQUAL =>
        advance()
        Some(expression())
      case _ => None
    val token = tokens(current)
    token.ttype match
      case TokenType.SEMICOLON => advance()
      case _ =>
        throw Parser.error(token, "expecting ';' after variable declaration")
    Stmt.VarDecl(ident, init)

  private def block(): Stmt =
    val stmts: ListBuffer[Stmt] = ListBuffer.empty
    var token = tokens(current)
    while token.ttype != TokenType.RIGHT_BRACE && token.ttype != TokenType.EOF
    do
      current += 1
      stmts.addOne(statement())
      token = tokens(current)
    val next = tokens(current)
    next.ttype match
      case TokenType.RIGHT_BRACE => advance()
      case _ =>
        throw Parser.error(next, "expecting '}' after block")
    Stmt.Block(stmts.toList)

  private def expression(): Expr =
    assignment()

  private def assignment(): Expr =
    val expr = equality() // consume lhs as rvalue
    val token = tokens(current)
    token.ttype match
      case TokenType.EQUAL =>
        advance()
        val value = assignment() // consume rhs
        expr match
          case Expr.Variable(name) => Expr.Assign(name, value)
          case _ =>
            Parser.error(token, "invalid assignment target") // no panic
            expr
      case _ => expr

  private def equality(): Expr =
    binary(
      parser = comparison,
      TokenType.BANG_EQUAL,
      TokenType.EQUAL_EQUAL,
    )

  private def comparison(): Expr =
    binary(
      parser = term,
      TokenType.GREATER,
      TokenType.GREATER_EQUAL,
      TokenType.LESS,
      TokenType.LESS_EQUAL,
    )

  private def term(): Expr =
    binary(
      parser = factor,
      TokenType.MINUS,
      TokenType.PLUS,
    )

  private def factor(): Expr =
    binary(
      parser = () => unary(),
      TokenType.SLASH,
      TokenType.STAR,
    )

  private def unary(): Expr =
    val token = tokens(current)
    token.ttype match
      case TokenType.BANG | TokenType.MINUS =>
        advance()
        val op = token
        val right = unary()
        Expr.Unary(op, right)
      case _ =>
        primary()

  private def primary(): Expr =
    val token = tokens(current)
    current += 1 // assumption
    token.ttype match
      case TokenType.IDENTIFIER =>
        Expr.Variable(token)
      case TokenType.FALSE =>
        Expr.Literal(false)
      case TokenType.TRUE =>
        Expr.Literal(true)
      case TokenType.NIL =>
        Expr.Literal(null)
      case TokenType.NUMBER | TokenType.STRING =>
        Expr.Literal(token.literal)
      case TokenType.LEFT_PAREN =>
        val expr = expression()
        val next = tokens(current)
        next.ttype match
          case TokenType.RIGHT_PAREN =>
            advance()
            Expr.Grouping(expr)
          case _ =>
            throw Parser.error(next, "expecting ')' after expression")
      case _ =>
        current -= 1 // assumption rollback
        throw Parser.error(token, "expecting an expression")

  private def binary(parser: () => Expr, ops: TokenType*): Expr =
    var left = parser()
    var token = tokens(current)
    while ops.contains(token.ttype) && token.ttype != TokenType.EOF do
      advance()
      val op = token
      val right = parser()
      left = Expr.Binary(left, op, right)
      token = tokens(current)
    return left

  // panic mode
  private def synchronize(): Unit =
    var token = tokens(current)
    while token.ttype != TokenType.EOF do
      token.ttype match
        case TokenType.SEMICOLON =>
          advance()
          return
        case TokenType.CLASS | TokenType.FOR | TokenType.FUN | TokenType.IF |
            TokenType.PRINT | TokenType.RETURN | TokenType.VAR |
            TokenType.WHILE =>
          return
        case _ => advance()
      token = tokens(current)

  // cursor
  private def advance(): Unit =
    current += 1
