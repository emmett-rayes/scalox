package com.emmettrayes.lox

case class Token(ttype: TokenType, lexeme: String, literal: Any, line: Int):
  override def toString(): String =
    ttype.toString
      + (if lexeme != "" then " " + lexeme else "")
      + (if literal != null then " " + literal else "")

enum TokenType:
  // single-character tokens
  case LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR

  // one or two character tokens
  case BANG, BANG_EQUAL,
    EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL,
    LESS, LESS_EQUAL

  // literals
  case IDENTIFIER, STRING, NUMBER

  // keywords
  case AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
    PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE

  // other
  case EOF
