package frontend.lex

enum Token:
  case Fn
  case Val
  case Var
  case Return

  case Identifier(val value: String)
  case Number(val value: BigInt)

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

  def is(that: Token): Boolean =
    (this, that) match
      case (Fn, Fn) | (Val, Val) | (Var, Var) | (Return, Return) | (Identifier(_), Identifier(_)) |
          (Number(_), Number(_)) | (LParen, LParen) | (RParen, RParen) | (LBrace, LBrace) |
          (RBrace, RBrace) | (Comma, Comma) | (Dot, Dot) | (Colon, Colon) | (Semicolon, Semicolon) |
          (Plus, Plus) | (Minus, Minus) | (Asterisk, Asterisk) | (Assign, Assign) =>
        true
      case _ => false

  override def toString(): String =
    this match
      case Assign                           => "`=`"
      case Fn                               => "`fn`"
      case Val                              => "`val`"
      case Var                              => "`var`"
      case Return                           => "`return`"
      case frontend.lex.Token.Identifier(_) => "identifier"
      case frontend.lex.Token.Number(_)     => "number"
      case LParen                           => "`(`"
      case RParen                           => "`)`"
      case LBrace                           => "`{`"
      case RBrace                           => "`}`"
      case Comma                            => "`,`"
      case Dot                              => "`.`"
      case Colon                            => "`:`"
      case Semicolon                        => "`;`"
      case Plus                             => "`+`"
      case Minus                            => "`-`"
      case Asterisk                         => "`*`"
