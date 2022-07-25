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
          case _: Parser.ParseError =>
            synchronize()
            None
      val token = tokens(current)
      val stmts = stmtOpt match
        case None       => acc
        case Some(stmt) => acc :+ stmt
      token.ttype match
        case TokenType.EOF => stmts
        case _             => addStmt(stmts)
    addStmt(Seq.empty).toList

  // parsers
  private def declaration(): Stmt =
    val token = tokens(current)
    token.ttype match
      case TokenType.VAR =>
        advance()
        varDecl()
      case _ =>
        statement()

  // statements
  private def statement(): Stmt =
    val token = tokens(current)
    current += 1 // assumption
    token.ttype match
      case TokenType.IF =>
        ifStmt()
      case TokenType.WHILE =>
        whileStmt()
      case TokenType.FOR =>
        forStmt()
      case TokenType.PRINT =>
        printStmt()
      case TokenType.LEFT_BRACE =>
        block()
      case _ =>
        current -= 1 // rollback assumption
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
      stmts.addOne(declaration())
      token = tokens(current)
    val next = tokens(current)
    next.ttype match
      case TokenType.RIGHT_BRACE => advance()
      case _ =>
        throw Parser.error(next, "expecting '}' after block")
    Stmt.Block(stmts.toList)

  private def ifStmt(): Stmt =
    val lp = tokens(current)
    lp.ttype match
      case TokenType.LEFT_PAREN =>
        advance()
        val condition = expression()
        val rp = tokens(current)
        rp.ttype match
          case TokenType.RIGHT_PAREN =>
            advance()
            val thenBranch = statement()
            val token = tokens(current)
            val elseBranch = token.ttype match
              case TokenType.ELSE =>
                advance()
                Some(statement())
              case _ => None
            Stmt.IfStmt(condition, thenBranch, elseBranch)
          case _ =>
            throw Parser.error(rp, "expecting ')' after if condition")
      case _ =>
        throw Parser.error(lp, "expecting '(' after 'if'")

  private def whileStmt(): Stmt =
    val lp = tokens(current)
    lp.ttype match
      case TokenType.LEFT_PAREN =>
        advance()
        val condition = expression()
        val rp = tokens(current)
        rp.ttype match
          case TokenType.RIGHT_PAREN =>
            advance()
            val body = statement()
            Stmt.WhileStmt(condition, body)
          case _ =>
            throw Parser.error(rp, "expecting ')' after loop condition")
      case _ =>
        throw Parser.error(lp, "expecting '(' after 'while' condition")

  private def forStmt(): Stmt =
    val lp = tokens(current)
    lp.ttype match
      case TokenType.LEFT_PAREN =>
        advance()
        val itoken = tokens(current)
        val init = itoken.ttype match
          case TokenType.SEMICOLON =>
            advance()
            None
          case TokenType.VAR =>
            advance()
            Some(varDecl())
          case _ => Some(exprStmt())
        val ctoken = tokens(current)
        val cond = ctoken.ttype match
          case TokenType.SEMICOLON =>
            advance()
            Expr.Literal(true)
          case _ => expression()
        val stoken = tokens(current)
        stoken.ttype match
          case TokenType.SEMICOLON =>
            advance()
            val utoken = tokens(current)
            val incr = utoken.ttype match
              case TokenType.RIGHT_PAREN =>
                advance()
                None
              case _ => Some(expression())
            val rp = tokens(current)
            rp.ttype match
              case TokenType.RIGHT_PAREN =>
                advance()
                val forBody = statement()
                val whileBody = incr match
                  case None => forBody
                  case Some(expr) =>
                    Stmt.Block(List(forBody, Stmt.ExprStmt(expr)))
                val whileStmt = Stmt.WhileStmt(cond, whileBody)
                init match
                  case None       => whileStmt
                  case Some(stmt) => Stmt.Block(List(stmt, whileStmt))
              case _ =>
                throw Parser.error(lp, "expecting ')' loop clauses")
          case _ =>
            throw Parser.error(lp, "expecting ';' after loop condition")
      case _ =>
        throw Parser.error(lp, "expecting '(' after 'for'")

  // expressions
  private def expression(): Expr =
    assignment()

  private def assignment(): Expr =
    val expr = or() // consume lhs as rvalue
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
    binary(parser = comparison, TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL)

  private def comparison(): Expr =
    binary(
      parser = term,
      TokenType.GREATER,
      TokenType.GREATER_EQUAL,
      TokenType.LESS,
      TokenType.LESS_EQUAL,
    )

  private def term(): Expr =
    binary(parser = factor, TokenType.MINUS, TokenType.PLUS)

  private def factor(): Expr =
    binary(parser = unary, TokenType.SLASH, TokenType.STAR)

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

  private def or(): Expr =
    logical(parser = and, TokenType.OR)

  private def and(): Expr =
    logical(parser = equality, TokenType.AND)

  private def logical(parser: () => Expr, ops: TokenType*): Expr =
    star(Expr.Logical.apply, parser, ops*)

  private def binary(parser: () => Expr, ops: TokenType*): Expr =
    star(Expr.Binary.apply, parser, ops*)

  // helpers
  private def star(
      ast: (Expr, Token, Expr) => Expr,
      parser: () => Expr,
      ops: TokenType*
  ): Expr =
    var left = parser()
    var token = tokens(current)
    while ops.contains(token.ttype) && token.ttype != TokenType.EOF do
      advance()
      val op = token
      val right = parser()
      left = ast(left, op, right)
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
