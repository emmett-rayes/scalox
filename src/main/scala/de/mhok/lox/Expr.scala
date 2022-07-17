package de.mhok.lox

enum Expr:
  case Binary(left: Expr, op: Token, right: Expr)
  case Grouping(expr: Expr)
  case Literal(value: Any)
  case Unary(op: Token, expr: Expr)

  override def toString(): String = this match
    case Binary(left, op, right) => s"(${op.lexeme} $left $right)"
    case Grouping(expr)          => s"(group $expr)"
    case Literal(null)           => s"nil"
    case Literal(value)          => s"$value"
    case Unary(op, expr)         => s"(${op.lexeme} $expr)"
