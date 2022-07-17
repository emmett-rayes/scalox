package de.mhok.lox

import scala.annotation.tailrec
import de.mhok.lox.Parser.ParseError

/*
expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
 */

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

  def parse(): Option[Expr] =
    try
      val expr = expression()
      if tokens(current).ttype != TokenType.EOF then
        Lox.error(
          tokens(current).line,
          "parse warning",
          s"after $expr",
          "unparsed remaining input",
        )
      Some(expr)
    catch case e: ParseError => None

  // parsers
  private def expression(): Expr =
    equality()

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
        current += 1
        val op = token
        val right = unary()
        Expr.Unary(op, right)
      case _ =>
        primary()

  private def primary(): Expr =
    val token = tokens(current)
    current += 1 // assumption
    token.ttype match
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
            current += 1
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
      current += 1
      val op = token
      val right = parser()
      left = Expr.Binary(left, op, right)
      token = tokens(current)
    return left

  // panic mode
  private def synchronize(): Unit =
    current += 1
    var token = tokens(current)
    while token.ttype != TokenType.EOF do
      if token.ttype == TokenType.SEMICOLON then return

      tokens(current + 1).ttype match
        case TokenType.CLASS | TokenType.FOR | TokenType.FUN | TokenType.IF |
            TokenType.PRINT | TokenType.RETURN | TokenType.VAR |
            TokenType.WHILE =>
          return
        case _ => return
