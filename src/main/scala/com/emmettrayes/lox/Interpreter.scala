package com.emmettrayes.lox

object Interpreter:
  case class RuntimeError(token: Token, message: String)
      extends RuntimeException

  private given Environment()

  def interpret(stmts: List[Stmt]): Unit =
    try for stmt <- stmts do execute(stmt)
    catch
      case e: RuntimeError =>
        Lox.runtimeError(
          e.token.line,
          "runtime error",
          s"at ${e.token.lexeme}",
          e.message,
        )

  private def execute(stmt: Stmt)(using env: Environment): Unit =
    stmt match
      case Stmt.Block(stmts) =>
        executeBlock(stmts)
      case Stmt.ExprStmt(expr) =>
        evaluate(expr)
      case Stmt.PrintStmt(expr) =>
        val value = evaluate(expr)
        println(stringify(value))
      case Stmt.VarDecl(name, init) =>
        val value = init match
          case None       => null
          case Some(expr) => evaluate(expr)
        env.define(name.lexeme, value)
      case Stmt.IfStmt(condition, thenBranch, elseBranch) =>
        if truthy(evaluate(condition)) then execute(thenBranch)
        else
          elseBranch match
            case None       => return
            case Some(stmt) => execute(stmt)

  private def executeBlock(stmts: List[Stmt])(using env: Environment): Unit =
    given Environment(env)
    for stmt <- stmts do execute(stmt)

  private def evaluate(expr: Expr)(using env: Environment): Value =
    expr match
      case Expr.Literal(null)  => null
      case Expr.Literal(value) => value
      case Expr.Grouping(expr) => evaluate(expr)
      case Expr.Variable(name) => env.get(name)
      case Expr.Assign(name, expr) =>
        val value = evaluate(expr)
        env.assign(name, value)
        return value
      case Expr.Logical(left, op, right) =>
        op.ttype match
          case TokenType.OR =>
            val value = evaluate(left)
            if truthy(value) then value else evaluate(right)
          case TokenType.AND =>
            val value = evaluate(left)
            if !truthy(value) then value else evaluate(right)
          case _ => ??? // unreachable
      case Expr.Unary(op, expr) =>
        val value = evaluate(expr)
        op.ttype match
          case TokenType.BANG =>
            !truthy(value)
          case TokenType.MINUS =>
            value match
              case d: Double => -d
              case _ =>
                throw RuntimeError(op, "operand must be a number");
          case _ => ??? // unreachable
      case Expr.Binary(left, op, right) =>
        val lvalue = evaluate(left)
        val rvalue = evaluate(right)
        op.ttype match
          case TokenType.PLUS =>
            (lvalue, rvalue) match
              case (d1: Double, d2: Double) => d1 + d2
              case (s1: String, s2: String) => s1 + s2
              case _ =>
                throw RuntimeError(
                  op,
                  "operands must be both numbers or strings",
                )
          case TokenType.MINUS =>
            (lvalue, rvalue) match
              case (d1: Double, d2: Double) => d1 - d2
              case _ =>
                throw RuntimeError(op, "operands must be numbers")
          case TokenType.STAR =>
            (lvalue, rvalue) match
              case (d1: Double, d2: Double) => d1 * d2
              case _ =>
                throw RuntimeError(op, "operands must be numbers")
          case TokenType.SLASH =>
            (lvalue, rvalue) match
              case (d1: Double, d2: Double) => d1 / d2
              case _ =>
                throw RuntimeError(op, "operands must be numbers")
          case TokenType.GREATER =>
            (lvalue, rvalue) match
              case (d1: Double, d2: Double) => d1 > d2
              case _ =>
                throw RuntimeError(op, "operands must be numbers")
          case TokenType.GREATER_EQUAL =>
            (lvalue, rvalue) match
              case (d1: Double, d2: Double) => d1 >= d2
              case _ =>
                throw RuntimeError(op, "operands must be numbers")
          case TokenType.LESS =>
            (lvalue, rvalue) match
              case (d1: Double, d2: Double) => d1 < d2
              case _ =>
                throw RuntimeError(op, "operands must be numbers")
          case TokenType.LESS_EQUAL =>
            (lvalue, rvalue) match
              case (d1: Double, d2: Double) => d1 <= d2
              case _ =>
                throw RuntimeError(op, "operands must be numbers")
          case TokenType.EQUAL_EQUAL =>
            lvalue == rvalue
          case TokenType.BANG_EQUAL =>
            lvalue != rvalue
          case _ => ??? // unreachable
  private def truthy(value: Value): Boolean =
    value match
      case false | 0d | "" | null => false
      case _                      => true

  private def stringify(value: Value): String =
    value match
      case null      => "nil"
      case d: Double => d.toString().stripSuffix(".0")
      case _         => value.toString()
