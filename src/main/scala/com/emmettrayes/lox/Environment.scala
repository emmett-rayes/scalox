package com.emmettrayes.lox

import scala.collection.mutable.Map

object Environment:
  private val values: Map[String, Value] = Map.empty

  def define(name: String, value: Value): Unit =
    values(name) = value

  def assign(name: Token, value: Value): Unit =
    if values.isDefinedAt(name.lexeme) then values(name.lexeme) = value
    else
      throw Interpreter.RuntimeError(
        name,
        s"undefined variable '${name.lexeme}'",
      )

  def get(name: Token): Value =
    values.get(name.lexeme) match
      case Some(value) => value
      case _ =>
        throw Interpreter.RuntimeError(
          name,
          s"undefined variable '${name.lexeme}'",
        )
