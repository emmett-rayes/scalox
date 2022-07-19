package com.emmettrayes.lox

type Value = Any | Boolean | Double | String | Null

enum Expr:
  case Binary(left: Expr, op: Token, right: Expr)
  case Grouping(expr: Expr)
  case Literal(value: Value)
  case Unary(op: Token, expr: Expr)
  case Variable(name: Token)

  override def toString(): String = this match
    case Binary(left, op, right) => s"(${op.lexeme} $left $right)"
    case Grouping(expr)          => s"(group $expr)"
    case Literal(null)           => s"nil"
    case Literal(value)          => s"$value"
    case Unary(op, expr)         => s"(${op.lexeme} $expr)"
    case Variable(name)          => s"var ${name.lexeme}"
