package com.emmettrayes.lox

enum Stmt:
  case ExprStmt(expr: Expr)
  case PrintStmt(expr: Expr)
  case Block(stmts: List[Stmt])
  case VarDecl(name: Token, init: Option[Expr])
  case IfStmt(condition: Expr, thenBranch: Stmt, elseBranch: Option[Stmt])
