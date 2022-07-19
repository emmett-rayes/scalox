package com.emmettrayes.lox

import scala.collection.mutable.Map

private implicit def someEnv(env: Environment): Option[Environment] = Some(env)

class Environment(val enclosing: Option[Environment] = None):
  private val values: Map[String, Value] = Map.empty

  def define(name: String, value: Value): Unit =
    values(name) = value

  def assign(name: Token, value: Value): Unit =
    if values.isDefinedAt(name.lexeme) then values(name.lexeme) = value
    else
      enclosing match
        case Some(env) => env.assign(name, value)
        case None =>
          throw Interpreter.RuntimeError(
            name,
            s"undefined variable '${name.lexeme}'",
          )

  def get(name: Token): Value =
    values.get(name.lexeme) match
      case Some(value) => value
      case None =>
        enclosing match
          case Some(env) => env.get(name)
          case None =>
            throw Interpreter.RuntimeError(
              name,
              s"undefined variable '${name.lexeme}'",
            )
