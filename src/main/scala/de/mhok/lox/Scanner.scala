package de.mhok.lox

import de.mhok.lox.Token
import de.mhok.lox.TokenType
import scala.annotation.tailrec

object Scanner:
  private val keywords = Map(
    "and" -> TokenType.AND,
    "class" -> TokenType.CLASS,
    "else" -> TokenType.ELSE,
    "false" -> TokenType.FALSE,
    "for" -> TokenType.FOR,
    "fun" -> TokenType.FUN,
    "if" -> TokenType.IF,
    "nil" -> TokenType.NIL,
    "or" -> TokenType.OR,
    "print" -> TokenType.PRINT,
    "return" -> TokenType.RETURN,
    "super" -> TokenType.SUPER,
    "this" -> TokenType.THIS,
    "true" -> TokenType.TRUE,
    "var" -> TokenType.VAR,
    "while" -> TokenType.WHILE,
  ).withDefaultValue(TokenType.IDENTIFIER)

class Scanner(val source: String):
  private var start = 0
  private var current = 0
  private var line = 1

  def scanTokens(): List[Token] =
    @tailrec
    def addToken(tokens: Seq[Token]): Seq[Token] =
      val token = scanToken()
      token.ttype match
        case TokenType.EOF => tokens :+ token
        case _             => addToken(tokens :+ token)

    return addToken(Seq.empty).toList

  @tailrec
  private def scanToken(): Token =
    if isAtEnd() then return makeEOFToken()
    start = current
    advance() match
      // single character
      case '(' => makeToken(TokenType.LEFT_PAREN)
      case ')' => makeToken(TokenType.RIGHT_PAREN)
      case '{' => makeToken(TokenType.LEFT_BRACE)
      case '}' => makeToken(TokenType.RIGHT_BRACE)
      case ',' => makeToken(TokenType.COMMA)
      case '.' => makeToken(TokenType.DOT)
      case '-' => makeToken(TokenType.MINUS)
      case '+' => makeToken(TokenType.PLUS)
      case ';' => makeToken(TokenType.SEMICOLON)
      case '*' => makeToken(TokenType.STAR)
      // lookahead = 1
      case '!' =>
        makeToken(
          if matchNext('=') then TokenType.BANG_EQUAL else TokenType.BANG
        )
      case '=' =>
        makeToken(
          if matchNext('=') then TokenType.EQUAL_EQUAL else TokenType.EQUAL
        )
      case '<' =>
        makeToken(
          if matchNext('=') then TokenType.LESS_EQUAL else TokenType.LESS
        )
      case '>' =>
        makeToken(
          if matchNext('=') then TokenType.GREATER_EQUAL else TokenType.GREATER
        )
      // literals, keywords and identifiers
      case '"'             => makeStringToken()
      case c if isDigit(c) => makeNumberToken()
      case c if isAlpha(c) => makeIdentifierToken()
      // comments
      case '/' =>
        if !matchNext('/') then makeToken(TokenType.SLASH)
        else
          consumeLine()
          scanToken()
      // white space
      case ' ' | '\r' | '\t' =>
        scanToken()
      case '\n' =>
        current += 1
        scanToken()
      // error
      case _ =>
        error(line, "unexpected character")
        scanToken()

  // char categorization
  private def isDigit(c: Char): Boolean =
    '0' <= c && c <= '9'

  private def isAlpha(c: Char): Boolean =
    c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_'

  private def isAlphaNumeric(c: Char): Boolean =
    isDigit(c) || isAlpha(c)

  // token constructors
  private def makeToken(ttype: TokenType): Token =
    val text = source.substring(start, current)
    val literal = ttype match
      case TokenType.STRING => text.substring(1, text.length - 1)
      case TokenType.NUMBER => text.toDouble
      case _                => null
    Token(ttype, text, literal, line)

  private def makeEOFToken(): Token =
    Token(TokenType.EOF, "", null, line)

  private def makeStringToken(): Token =
    while peek() != '"' && !isAtEnd() do
      if peek() == '\n' then line += 1
      advance()
    if isAtEnd() then error(line, "unterminated string")
    advance()
    makeToken(TokenType.STRING)

  private def makeNumberToken(): Token =
    while isDigit(peek()) do advance()
    if peek(offset = 0) == '.' && isDigit(peek(offset = 1)) then advance()
    while isDigit(peek()) do advance()
    makeToken(TokenType.NUMBER)

  private def makeIdentifierToken(): Token =
    while isAlphaNumeric(peek()) do advance()
    makeToken(Scanner.keywords(source.substring(start, current)))

  // cursor movement
  private def isAtEnd(): Boolean =
    current >= source.length

  private def peek(offset: Int = 0): Char =
    if current + offset >= source.length then '\u0000'
    else source.charAt(current + offset)

  private def advance(): Char =
    val c = source.charAt(current)
    current += 1
    return c

  private def matchNext(expected: Char): Boolean =
    if isAtEnd() || source.charAt(current) != expected then false
    else
      current += 1
      true

  private def consumeLine(): Unit =
    while (peek() != '\n' && !isAtEnd()) do advance()
