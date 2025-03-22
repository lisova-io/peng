package frontend.lex

enum Token:
  case Fn
  case Val
  case Var
  case Return

  case Identifier(val value: String)
  case Number(val value: Int)

  case LParen
  case RParen
  case LBrace
  case RBrace
  case Comma
  case Dot
  case Colon
  case Semicolon
  case Plus
  case Minus
  case Asterisk
  case Assign
