package frontend.lex

enum Token:
  case Fn
  case Val
  case Var
  case Return
  case If
  case Else
  case While

  case True
  case False

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
  case Eq
  case Ne
  case Lt
  case Le
  case Gt
  case Ge

  def is(that: Token): Boolean =
    (this, that) match
      case (Number(_), Number(_))         => true
      case (Identifier(_), Identifier(_)) => true
      case (a, b)                         => a == b

  override def toString(): String =
    this match
      case Fn                               => "`fn`"
      case Val                              => "`val`"
      case Var                              => "`var`"
      case Return                           => "`return`"
      case If                               => "`if`"
      case Else                             => "`else`"
      case While                            => "`while`"
      case True                             => "`true`"
      case False                            => "`false`"
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
      case Assign                           => "`=`"
      case Eq                               => "`==`"
      case Ne                               => "`!=`"
      case Lt                               => "`<`"
      case Le                               => "`<=`"
      case Gt                               => "`>`"
      case Ge                               => "`>=`"
