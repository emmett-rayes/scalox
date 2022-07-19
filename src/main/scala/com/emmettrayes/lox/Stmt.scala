package com.emmettrayes.lox

enum Stmt:
    case ExprStmt(expr: Expr)
    case PrintStmt(expr: Expr)
    case VarDecl(name: Token, init: Option[Expr])
