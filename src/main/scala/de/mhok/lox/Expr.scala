package de.mhok.lox

type Value = Any | Boolean | Double | String | Null

enum Expr:
  case Binary(left: Expr, op: Token, right: Expr)
  case Grouping(expr: Expr)
  case Literal(value: Value)
  case Unary(op: Token, expr: Expr)

  override def toString(): String = this match
    case Binary(left, op, right) => s"(${op.lexeme} $left $right)"
    case Grouping(expr)          => s"(group $expr)"
    case Literal(null)           => s"nil"
    case Literal(value)          => s"$value"
    case Unary(op, expr)         => s"(${op.lexeme} $expr)"
