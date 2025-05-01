package frontend.parse

import frontend.ast.*
import frontend.lex.{Lexer, Offset, Span, Token, Tokens, WithSpan}
import frontend.diagnostics.Diagnostic
import frontend.parse.combinators.Parser as P

import scala.collection.mutable.HashMap
import diagnostics.containsErrors

trait Parser:
  def parse(tokens: Tokens): (List[Diagnostic], List[Decl])

object DefaultParser extends Parser:
  private def parseToken(t: Token): P[WithSpan[Token]] = P.token(
    _ match
      case t1 if t1.value == t => Some(t1)
      case _                   => None,
    t :: Nil,
  )

  private def parseIdentifier: P[WithSpan[String]] = P
    .token(
      _ match
        case WithSpan(Token.Identifier(s), span) => Some(WithSpan(s, span))
        case _                                   => None
      ,
      "identifier",
    )

  private def parseType: P[WithSpan[Type]] = P.token(
    _ match
      case WithSpan(Token.Identifier(s), span) => Type.fromString(s).map(WithSpan(_, span))
      case _                                   => None
    ,
    "type",
  )

  private def parseCallExpr: P[CallExpr] = for {
    name <- parseIdentifier
    _    <- parseToken(Token.LParen)
    args <- parseExpr.backtrack.sepEndBy(parseToken(Token.Comma).backtrack)
    _    <- parseToken(Token.RParen)
  } yield CallExpr(name, args)
  private def parseVarRefExpr: P[VarRefExpr] = parseIdentifier.map(VarRefExpr(_, Type.Undef))
  private def parseUnaryExpr: P[UnaryExpr] =
    for {
      op <- parseToken(Token.Minus).map(_.map(_ => UnaryOp.Minus))
      e  <- parseExpr
    } yield UnaryExpr(op, e)
  private def parseNumLitExpr: P[NumLitExpr] = P
    .token(
      _ match
        case WithSpan(Token.Number(n), span) => Some(NumLitExpr(Type.Undef, WithSpan(n, span)))
        case _                               => None
      ,
      "number" :: Nil,
    )
  private def parseBoolLitExpr: P[BoolLitExpr] =
    (parseToken(Token.True).backtrack.map(ws => BoolLitExpr(ws.map(_ => true)))
      <|> parseToken(Token.False).map(ws => BoolLitExpr(ws.map(_ => false))))
      .expect("boolean literal" :: Nil)

  private def parseTerm: P[Expr] =
    parseCallExpr.backtrack
      <|> parseVarRefExpr.backtrack
      <|> parseUnaryExpr.backtrack
      <|> parseNumLitExpr.backtrack
      <|> parseBoolLitExpr.backtrack
      <|> (for {
        _ <- parseToken(Token.LParen)
        e <- parseExpr
        _ <- parseToken(Token.RParen)
      } yield e)

  private def parseBinExpr(opPrec: Precedence, lhs: Expr): P[Expr] =
    def parseBinOp: P[WithSpan[BinOp]] = P.token(
      (
          ws =>
            ws.value match
              case Token.Plus     => Some(ws.map(_ => BinOp.Plus))
              case Token.Minus    => Some(ws.map(_ => BinOp.Minus))
              case Token.Asterisk => Some(ws.map(_ => BinOp.Mul))
              case Token.Assign   => Some(ws.map(_ => BinOp.Assign))
              case Token.Eq       => Some(ws.map(_ => BinOp.Eq))
              case Token.Ne       => Some(ws.map(_ => BinOp.Ne))
              case Token.Le       => Some(ws.map(_ => BinOp.Le))
              case Token.Lt       => Some(ws.map(_ => BinOp.Lt))
              case Token.Gt       => Some(ws.map(_ => BinOp.Gt))
              case Token.Ge       => Some(ws.map(_ => BinOp.Ge))
              case _              => None
      ),
      "binary operator" :: Nil,
    )
    def getCurPrecedence: P[Precedence] = parseBinOp.lookahead.backtrack
      .map(_.value.precedence)
      .ignoreFailure(-1)

    getCurPrecedence.flatMap(curPrec => {
      if curPrec < opPrec
      then P.pure(lhs)
      else
        for {
          op       <- parseBinOp
          rhs      <- parseTerm
          nextPrec <- getCurPrecedence
          rhs <-
            if curPrec < nextPrec
            then parseBinExpr(curPrec + 1, rhs)
            else P.pure(rhs)
          e = BinExpr(op, lhs, rhs, Type.Undef)
          be <- parseBinExpr(opPrec, e)
        } yield be
    })

  private def parseExpr: P[Expr] =
    parseTerm
      .flatMap(lhs => parseBinExpr(0, lhs))
      .expect("expression" :: Nil)

  private def parseExprStmt: P[ExprStmt] = parseExpr.map(ExprStmt(_))
  private def parseIfStmt: P[IfStmt] =
    for {
      _    <- parseToken(Token.If)
      cond <- parseExpr
      tBr  <- parseBlock.map(BlockStmt(_))
      fBr <- (parseToken(Token.Else) *> parseBlock
        .map(BlockStmt(_))
        .orElse[IfStmt, IfStmt | BlockStmt](parseIfStmt))
        .map(Some(_))
        .backtrack
        .ignoreFailure(None)
    } yield IfStmt(cond, tBr, fBr)
  private def parseWhileStmt: P[WhileStmt] = for {
    _    <- parseToken(Token.While)
    cond <- parseExpr
    body <- parseBlock.map(BlockStmt(_))
  } yield WhileStmt(cond, body)
  private def parseDeclStmt: P[DeclStmt] = parseVarDecl.map(DeclStmt(_))
  private def parseRetStmt: P[RetStmt | UnitRetStmt] =
    for {
      WithSpan(_, span) <- parseToken(Token.Return)
      e                 <- parseExpr.backtrack.map(Some(_)).ignoreFailure(None)
    } yield e match
      case None    => UnitRetStmt(span)
      case Some(e) => RetStmt(e)

  private def parseStmt: P[Stmt] =
    ((parseExprStmt <* parseToken(Token.Semicolon)).backtrack
      <|> (parseRetStmt <* parseToken(Token.Semicolon)).backtrack
      <|> parseIfStmt.backtrack
      <|> parseWhileStmt.backtrack
      <|> (parseDeclStmt <* parseToken(Token.Semicolon)).backtrack
      <|> parseBlock.map(BlockStmt(_)))
      .expect("statement" :: Nil)

  private def parseBlock: P[Block] = parseBlockWithRBrace.map(_._1)

  private def parseBlockWithRBrace: P[(Block, Span)] =
    for {
      _ <- parseToken(Token.LBrace)
      b <- parseStmt.backtrack.sepBy(parseToken(Token.Semicolon).backtrack.many)
      r <- parseToken(Token.RBrace)
    } yield b -> r.span

  private def parseFnDecl: P[FnDecl] =
    def parseParam: P[(WithSpan[Name], WithSpan[Type])] =
      for {
        name <- parseIdentifier
        _    <- parseToken(Token.Colon)
        tp   <- parseType
      } yield name -> tp
    def parseParams: P[List[(WithSpan[Name], WithSpan[Type])]] =
      parseParam.backtrack.sepEndBy(parseToken(Token.Comma).backtrack)

    for {
      _      <- parseToken(Token.Fn)
      name   <- parseIdentifier.expect("function name" :: Nil)
      _      <- parseToken(Token.LParen)
      params <- parseParams
      _      <- parseToken(Token.RParen)
      rettype <- (parseType.backtrack <|> (parseToken(Token.LBrace).lookahead
        .map(_.map(_ => Type.Unit)))).expect("function return type" :: Nil)
      (body, rbraceSpan) <- parseBlockWithRBrace
        .map(p => BlockStmt(p._1) -> p._2)
      // .expect("function body" :: Nil)
    } yield
      // this is a dirty hack, should be fixed?
      val b =
        if rettype.value == Type.Unit then
          body.block.lastOption match
            case Some(u: UnitRetStmt) => body
            case _                    => BlockStmt(body.block :+ UnitRetStmt(rbraceSpan))
        else body
      FnDecl(name, params, rettype, b)

  private def parseVarDecl: P[VarDecl] =
    for {
      const <- P.token(
        _.value match
          case Token.Val => Some(true)
          case Token.Var => Some(false)
          case _         => None
        ,
        Token.Val :: Token.Var :: Nil,
      )
      name <- parseIdentifier
      tp <- (parseToken(Token.Colon) *> parseType).backtrack
        <|> P.pure(WithSpan(Type.Undef, name.span))
      value <- parseToken(Token.Assign) *> parseExpr
    } yield VarDecl(const, name, tp, value)

  private def parseDecl: P[Decl] =
    (parseFnDecl.map(_.asInstanceOf[Decl]).backtrack
      <|> (parseVarDecl <* parseToken(Token.Semicolon)))
      .expect("declaration" :: Nil)

  override def parse(t: Tokens): (List[Diagnostic], List[Decl]) =
    var tokens                  = t
    var diags: List[Diagnostic] = Nil
    var decls: List[Decl]       = Nil
    while !tokens.isEmpty do
      val (res, toks) = parseDecl.run(tokens)
      res match
        case Left(diag) =>
          diags :+= diag
          tokens = P
            .skipUntil(_ match
              case Token.Val | Token.Var | Token.Fn => true
              case _                                => false)
            .run(toks)
            ._2
        case Right(decl) =>
          decls :+= decl
          tokens = toks
    diags -> decls
