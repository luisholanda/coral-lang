module Language.Coral.Syntax.Parser.Parser where

import Data.Char (isLower, isUpper)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Coral.Syntax.AST
import Language.Coral.Syntax.Parser.Lexer
import Language.Coral.Syntax.Parser.Types
import Text.Parsec
import qualified Text.Parsec.Indent as I
import Text.ParserCombinators.Parsec (ParseError)

parseText :: Text -> Either ParseError Module
parseText = readOrThrow parseCoral

readOrThrow :: Parser a -> Text -> Either ParseError a
readOrThrow parser = I.runIndentParser parser () "coral"

parseCoral :: Parser Module
parseCoral = do
  suite <- many1 parseTopDeclr
  let pub = getStatements isPublic suite
      priv = getStatements (not . isPublic) suite
  pure $ Module pub priv
  where
    getStatements f = map extract . filter f

parseTopDeclr :: Parser (ParseTypes Statement)
parseTopDeclr = do
  pub <- optionMaybe (try pub')
  maybe Private (const Public) pub <$> parseStmt

parseStmt :: Parser Statement
parseStmt =
  choice $
  map
    try
    [ parseFunc
    , parseAssign
    , parseVar
    , parsePass
    , parseBreak
    , parseContinue
    , parseImport
    , parseAsyncFunc
    ]
  where
    parsePass = pass' *> pure Pass
    parseBreak = break' *> pure Break
    parseContinue = continue' *> pure Continue
    parseImport = import' *> commaSep1 parseImpItem <&> Import
      where
        parseImpItem = do
          itemName <- parseDotted
          asName <- optionMaybe (as' *> parseIdent)
          pure $ ImportItem {itemName, asName}
    parseFunc = do
      funcName <- def' *> parseIdent
      params <- parens parseParameters
      retType <- larrow' *> parseType
      body <- block' parseStmt
      pure $ Fun {funcName, params, retType, body}
    parseAsyncFunc = async' *> parseFunc <&> AsyncFun
    parseAssign = do
      left <- parseExpr
      assignType <-
        (try $ equals *> pure Assign) <|> (defEquals *> pure MutAssign)
      assignType left <$> parseExpr
    parseVar = do
      isMutable <- isJust <$> optionMaybe mut'
      varName <- parseIdent
      baseType <- colon *> parseType
      let varType =
            if isMutable
              then MutType baseType
              else baseType
      varValue <- equals *> parseExpr
      pure $ VarDef varName varType varValue

parseIdent :: Parser Ident
parseIdent = Ident <$> identifier

parseDotted :: Parser DottedName
parseDotted = parseIdent `sepBy` (char '.')

parseParameters :: Parser [Parameter]
parseParameters = fromMaybe [] <$> optionMaybe (many parseParameter)

parseParameter :: Parser Parameter
parseParameter = do
  paramName <- parseIdent
  paramType <- colon *> parseParamType
  paramDefault <- optionMaybe $ equals *> parseExpr
  pure $ Param {paramName, paramType, paramDefault}

parseParamType :: Parser Type
parseParamType = do
  mutable <- isJust <$> optionMaybe mut'
  pType <- parseType
  pure $
    if mutable
      then MutType pType
      else pType

parseType :: Parser Type
parseType =
  whiteSpace *> do
    generic <- optionMaybe $ char '\''
    ident <- identifier `sepBy1` char '.'
    if isJust generic
      -- This is a generic type, thus, the type identifier should be lowercase
      then if length ident > 1
             then unexpected "Generic Types should be simple strings."
             else let typeIdent = head ident
                   in if isLower (T.head typeIdent)
                        then pure . FreeType $ Ident typeIdent
                        else unexpected
                               "Generic Types should start with lower case."
      -- This is a concrete type, thus the type identifier should be uppercase.
      else let typeIdent = last ident
            in if isUpper (T.head typeIdent)
                 then pure . Type . map Ident $ ident
                 else unexpected "Concrete types should start with upper case."

parseArgsType :: Parser Type
parseArgsType = ArgsType <$> (char '*' *> parseType)

parseExpr :: Parser Expr
parseExpr = choice $ map try [Float <$> float, Int <$> integer, parseVar]
  where
    parseVar = Var <$> parseIdent
