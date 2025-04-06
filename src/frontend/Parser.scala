package frontend.parse

import frontend.ast.*
import frontend.lex.{Lexer, Offset, Span, Token, WithSpan}
import frontend.diagnostics.Diagnostic

import scala.util.boundary, boundary.break
import scala.collection.mutable.HashMap

extension [A, B](e1: Either[A, B])
  def andThen[A1 >: A, B1](e2: Either[A1, B1]): Either[A1, B1] = e1.flatMap(_ => e2)

type ParseResult[T] = Either[List[Diagnostic], T]

private def expected[A](span: Span, expected: Any*): Diagnostic = {
  def formatExpected(l: Seq[Any]): String =
    l match
      case Nil           => ""
      case x +: Nil      => x.toString
      case x +: y +: Nil => s"$x or $y"
      case x +: tail     => s"$x, " + formatExpected(tail)

  Diagnostic.error(span, s"expected ${formatExpected(expected)}")
}

trait Parser:
  def parse: (List[Decl], List[Diagnostic])

class DefaultParser(lexer: Lexer) extends Parser:
  private def matchToken[T](tok: Token, value: T = ()): Token => Option[T] =
    t => if t.is(tok) then Some(value) else None

  private def token[T](filterMap: Token => Option[T], expect: Any*): ParseResult[WithSpan[T]] =
    lexer.next
      .map(ws => {
        val WithSpan(t, s) = ws
        filterMap(t) match
          case Some(value) => Right(ws.map(_ => value))
          case None        => Left(List(expected(s, expect*)))
      })
      .getOrElse(Left(List(expected(lexer.endSpan, expect*))))

  private def peekToken[T](filterMap: Token => Option[T], expect: Any*): ParseResult[WithSpan[T]] =
    lexer.peek
      .map(ws => {
        val WithSpan(t, s) = ws
        filterMap(t) match
          case Some(value) => Right(ws.map(_ => value))
          case None        => Left(List(expected(s, expect*)))
      })
      .getOrElse(Left(List(expected(lexer.endSpan, expect*))))

  private def parseNumber: ParseResult[WithSpan[BigInt]] =
    token(
      _ match
        case Token.Number(value) => Some(value)
        case _                   => None
      ,
      "number",
    )

  private def parseIdentifier: ParseResult[WithSpan[String]] =
    token(
      _ match
        case Token.Identifier(value) => Some(value)
        case _                       => None
      ,
      "identifier",
    )

  private def parseType: ParseResult[WithSpan[Type]] =
    token(
      _ match
        case Token.Identifier(s) => Type.fromString(s)
        case _                   => None
      ,
      "type",
    )

  private def tryParseType: Option[WithSpan[Type]] =
    peekToken(
      _ match
        case Token.Identifier(s) => Type.fromString(s)
        case _                   => None
      ,
      "type",
    ) match
      case Left(err) => None
      case Right(t)  => lexer.next; Some(t)

  private def parseRefExpr: ParseResult[VarRefExpr | CallExpr] = {
    def parseArgs: ParseResult[List[Expr]] = boundary {
      peekToken(
        {
          _ match
            case Token.RParen => Some(false)
            case _            => Some(true)
        },
        "argument",
        Token.RParen,
      ) match
        case Left(err)                 => break(Left(err))
        case Right(WithSpan(false, _)) => lexer.next; break(Right(Nil))
        case Right(WithSpan(true, _))  => ()

      var args: List[Expr] = Nil
      while true do {
        parseExpr match
          case Left(err)  => break(Left(err))
          case Right(arg) => args :+= arg

        token(
          { t =>
            t match
              case Token.Comma  => Some(true)
              case Token.RParen => Some(false)
              case _            => None
          },
          Token.Comma,
          Token.RParen,
        ) match
          case Left(err)                 => break(Left(err))
          case Right(WithSpan(false, _)) => break(Right(args))
          case Right(WithSpan(true, _))  => ()
      }
      Right(args)
    }

    for {
      name <- parseIdentifier
      hasArgs <- peekToken(
        _ match
          case Token.LParen => Some(true)
          case _            => Some(false)
      )
      args <-
        if hasArgs.value then { lexer.next; parseArgs.map(a => Some(a)) }
        else Right(None)
    } yield {
      args match
        case Some(args) => CallExpr(name, args)
        case None       => VarRefExpr(name)
    }
  }

  private def parseNumLiteral: ParseResult[NumLitExpr] =
    token(
      (_ match
        case Token.Number(value) => Some(value)
        case _                   => None
      ),
      Token.Number,
    ).map(v => NumLitExpr(Type.Undef, v))

  private def parseBoolLiteral: ParseResult[BoolLitExpr] =
    token(
      (_ match
        case Token.True  => Some(true)
        case Token.False => Some(false)
        case _           => None
      ),
      Token.True,
      Token.False,
    ).map(v => BoolLitExpr(v))

  private def parseTerm: ParseResult[Expr] = {
    enum ExprType:
      case Subexpr
      case Ref
      case Num
      case Bool

    peekToken(
      (
        _ match
          case Token.LParen             => Some(ExprType.Subexpr)
          case Token.Identifier(_)      => Some(ExprType.Ref)
          case Token.Number(_)          => Some(ExprType.Num)
          case Token.True | Token.False => Some(ExprType.Bool)
          case _                        => None
      ),
      "expression",
    ).flatMap(ws =>
      val WithSpan(ex, _) = ws
      ex match
        case ExprType.Subexpr => {
          lexer.next;
          parseExpr.flatMap(e =>
            token(matchToken(Token.RParen), Token.RParen)
              .andThen(Right(e))
          )
        }
        case ExprType.Ref  => parseRefExpr
        case ExprType.Num  => parseNumLiteral
        case ExprType.Bool => parseBoolLiteral
    )
  }

  private def parseBinaryExpr(opPrec: Precedence, _lhs: Expr): ParseResult[Expr] = {
    def binOpFromToken(tok: Token): Option[BinOp] =
      tok match
        case Token.Plus     => Some(BinOp.Plus)
        case Token.Minus    => Some(BinOp.Minus)
        case Token.Asterisk => Some(BinOp.Mul)
        case Token.Assign   => Some(BinOp.Assign)
        case Token.Eq       => Some(BinOp.Eq)
        case Token.Ne       => Some(BinOp.Ne)
        case Token.Le       => Some(BinOp.Le)
        case Token.Lt       => Some(BinOp.Lt)
        case Token.Gt       => Some(BinOp.Gt)
        case Token.Ge       => Some(BinOp.Ge)
        case _              => None

    def getCurPrecedence: Precedence = lexer.peek
      .flatMap(ws => binOpFromToken(ws.value).map(_.precedence))
      .getOrElse(-1)

    var lhs: Expr = _lhs;
    boundary {
      while true do {
        val curPrec = getCurPrecedence
        if curPrec < opPrec then break(Right(lhs))

        val res = for {
          binOp <- token(binOpFromToken, "binary operator")
          rhs   <- parseTerm
          rhs <-
            if curPrec < getCurPrecedence
            then parseBinaryExpr(curPrec + 1, rhs)
            else Right(rhs)
        } yield BinExpr(binOp, lhs, rhs)

        res match
          case Left(err) => break(Left(err))
          case Right(e)  => lhs = e
      }

      Right(lhs)
    }
  }

  private def parseExpr: ParseResult[Expr] =
    for {
      lhs <- parseTerm
      e   <- parseBinaryExpr(0, lhs)
    } yield e

  private def parseRetStmt: ParseResult[RetStmt | UnitRetStmt] =
    token(matchToken(Token.Return), Token.Return)
      .flatMap(unitws =>
        peekToken(_ match
          case Token.Semicolon => Some(false)
          case _               => Some(true)).map(b => WithSpan(b.value, unitws.span))
      )
      .flatMap(ws => {
        val WithSpan(hasExpr, retSpan) = ws
        if hasExpr then parseExpr.map(e => RetStmt(e)) else Right(UnitRetStmt(retSpan))
      })

  private def parseIfStmt: ParseResult[IfStmt] = {
    def parseIfOrBlock: ParseResult[BlockStmt | IfStmt] =
      for {
        isIf <- peekToken(
          _ match
            case Token.If     => Some(true)
            case Token.LBrace => Some(false)
            case _            => None,
          "statement",
        )
        res <- if isIf.value then parseIfStmt else parseBlock
      } yield res

    def parseElse: ParseResult[Option[BlockStmt | IfStmt]] =
      peekToken(_ match
        case Token.Else => Some(true)
        case _          => Some(false))
        .flatMap(hasElse =>
          if !hasElse.value
          then Right(None)
          else {
            lexer.next
            parseIfOrBlock.map(Some(_))
          }
        )

    token(matchToken(Token.If), Token.If).andThen(
      for {
        cond    <- parseExpr
        onTrue  <- parseBlock
        onFalse <- parseElse
      } yield IfStmt(cond, onTrue, onFalse)
    )
  }

  private def parseWhileStmt: ParseResult[WhileStmt] = {
    token(matchToken(Token.While), Token.While).andThen(
      for {
        cond <- parseExpr
        body <- parseBlock
      } yield WhileStmt(cond, body)
    )
  }

  private def parseStmt: ParseResult[Stmt] = {
    enum StmtType {
      case Expr
      case If
      case While
      case Block
      case Decl
      case Ret
    }

    peekToken(t =>
      t match
        case Token.LBrace            => Some(StmtType.Block)
        case (Token.Val | Token.Var) => Some(StmtType.Decl)
        case Token.Return            => Some(StmtType.Ret)
        case Token.If                => Some(StmtType.If)
        case Token.While             => Some(StmtType.While)
        case _                       => Some(StmtType.Expr)
    ).flatMap(ws =>
      val WithSpan(s, _) = ws
      s match
        case StmtType.Expr  => parseExpr.map(e => ExprStmt(e))
        case StmtType.Block => parseBlock
        case StmtType.If    => parseIfStmt
        case StmtType.While => parseWhileStmt
        case StmtType.Decl  => parseVarDecl.map(d => DeclStmt(d))
        case StmtType.Ret   => parseRetStmt
    )
  }

  private def parseBlock = parseBlockWithLastRParen.map(_._1)

  private def parseBlockWithLastRParen: ParseResult[(BlockStmt, Span)] = {
    def block = boundary {
      var block: Block = Nil

      while true do {
        def peekSemi = peekToken(matchToken(Token.Semicolon))
        def semi     = token(matchToken(Token.Semicolon), Token.Semicolon)

        while peekSemi.isRight do semi

        val r: ParseResult[WithSpan[Boolean]] = peekToken(t =>
          t match
            case Token.RBrace => Some(false)
            case _            => Some(true)
        )

        r match
          case Left(err) => throw RuntimeException(s"unreachable, got error $err")
          case Right(WithSpan(false, span)) => lexer.next; break(Right((block, span)))
          case Right(WithSpan(true, _))     => ()

        val shouldExpectSemi =
          parseStmt match
            case Left(err) => break(Left(err))
            case Right(stmt) =>
              block :+= stmt
              stmt match
                case BlockStmt(_) | IfStmt(_, _, _) | WhileStmt(_, _) => false
                case _                                                => true

        if shouldExpectSemi then
          semi match
            case Left(err) => break(Left(err))
            case Right(_)  => ()
      }
      ???
    }

    token(matchToken(Token.LBrace), Token.LBrace)
      .andThen(block.map(b => BlockStmt(b._1) -> b._2))
  }

  private def parseFnDecl: ParseResult[FnDecl] = {
    def parseParam: ParseResult[(WithSpan[Name], WithSpan[Type])] =
      for {
        name <- parseIdentifier
        _    <- token(matchToken(Token.Colon), Token.Colon)
        tp   <- parseType
      } yield (name, tp)

    def parseParams: ParseResult[List[(WithSpan[Name], WithSpan[Type])]] = {
      def params = boundary {
        peekToken(
          {
            _ match
              case Token.RParen => Some(false)
              case _            => Some(true)
          },
          "parameter",
          Token.RParen,
        ) match
          case Left(err)                 => break(Left(err))
          case Right(WithSpan(false, _)) => lexer.next; break(Right(Nil))
          case Right(WithSpan(true, _))  => ()

        var params: List[(WithSpan[Name], WithSpan[Type])] = Nil
        while true do {
          parseParam match
            case Left(err)    => break(Left(err))
            case Right(param) => params :+= param

          token(
            { t =>
              t match
                case Token.Comma  => Some(true)
                case Token.RParen => Some(false)
                case _            => None
            },
            Token.Comma,
            Token.RParen,
          ) match
            case Left(err)                 => break(Left(err))
            case Right(WithSpan(false, _)) => break(Right(params))
            case Right(WithSpan(true, _))  => ()
        }
        Right(params)
      }

      token(matchToken(Token.LParen), Token.LParen).andThen(params)
    }

    token(matchToken(Token.Fn), Token.Fn).andThen(
      for {
        name   <- parseIdentifier
        params <- parseParams
        rettype = tryParseType.getOrElse(lexer.peek.map(ws => ws.map(_ => Type.Unit)).get)
        (body, rParenSpan) <- parseBlockWithLastRParen
      } yield {
        if rettype.value != Type.Unit then FnDecl(name, params, rettype, body)
        else
          FnDecl(
            name,
            params,
            rettype,
            BlockStmt(body.block.lastOption match
              case Some(UnitRetStmt(_)) => body.block
              case _                    => body.block :+ UnitRetStmt(rParenSpan)),
          )
      }
    )
  }

  private def parseVarDecl: ParseResult[VarDecl] = {
    def t: ParseResult[WithSpan[Type]] =
      peekToken(
        (_ match
          case Token.Colon  => Some(true)
          case Token.Assign => Some(false)
          case _            => None
        ),
        "variable type",
        "value",
      ).flatMap(ws => {
        val WithSpan(typeSpecified, _) = ws
        if typeSpecified then { lexer.next; parseType }
        else Right(ws.map(_ => Type.Undef))
      })

    for {
      const <- token(
        (_ match
          case Token.Val => Some(true)
          case Token.Var => Some(false)
          case _         => None
        ),
        Token.Val,
        Token.Var,
      )
      name  <- parseIdentifier
      tp    <- t
      value <- token(matchToken(Token.Assign), Token.Assign).andThen(parseExpr)
    } yield VarDecl(const.value, name, tp, value)
  }

  def parse: (List[Decl], List[Diagnostic]) = {
    var res: List[Decl]               = Nil
    var diagnostics: List[Diagnostic] = Nil

    while lexer.peek.isDefined do {
      val declRes = lexer.peek.get match
        case WithSpan(Token.Fn, _) => parseFnDecl
        case WithSpan(Token.Val | Token.Var, _) =>
          parseVarDecl.flatMap(d => token(matchToken(Token.Semicolon), Token.Semicolon).map(_ => d))
        case WithSpan(t, s) => lexer.next; Left(List(expected(s, "declaration")))

      declRes match
        case Left(diags) => diagnostics ++= diags
        case Right(decl) => res :+= decl
    }

    (res, diagnostics)
  }
