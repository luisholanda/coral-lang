{-# LANGUAGE TemplateHaskell #-}
module Language.Coral.Parser.Monad where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Maybe                     ( listToMaybe )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String

import           Language.Coral.Lexer.Token
import           Language.Coral.Lexer.InputStream
import           Language.Coral.Parser.Error
import           Language.Coral.SrcSpan


internalError :: forall a s . Doc s -> P a
internalError =
  throwError . StrError . renderString . layoutPretty defaultLayoutOptions


spanError :: forall a b . Span a => a -> String -> P b
spanError x str = internalError $ pretty (getSpan x) <+> pretty str



type P a = StateT ParserState (Either ParserError) a


data ParserState =
  ParserState
  { _location :: !SrcLoc     -- ^ Position at current input location
  , _input :: !InputStream   -- ^ The current input
  , _previousToken :: !Token -- ^ The previous token
  , _startCodeStack :: [Int] -- ^ A stack of start codes for the state of the lexer
  , _indentStack :: [Int]    -- ^ A stack of source column positions of indenting levels
  , _parenStack :: [Token]   -- ^ A stack of parens and brackets for indentation handling
  , _lastEOL :: !SrcSpan     -- ^ Location of the last end-of-line encountered
  , _comments :: [Token]     -- ^ Accumulated comments
  }
  deriving Show
makeClassy ''ParserState


execParser :: forall a . P a -> ParserState -> Either ParserError a
execParser = evalStateT


execParserKeepComments
  :: forall a . P a -> ParserState -> Either ParserError (a, [Token])
execParserKeepComments parser = evalStateT $ do
  result <- parser
  coms   <- getComments
  pure (result, coms)


{-# INLINE runParser #-}
runParser :: forall a . P a -> ParserState -> Either ParserError (a, ParserState)
runParser = runStateT


{-# INLINE returnP #-}
returnP :: forall a . a -> P a
returnP = pure


{-# INLINE thenP #-}
thenP :: forall a b . P a -> (a -> P b) -> P b
thenP = (>>=)


initToken :: Token
initToken = TNewLine SpanEmpty


initialState :: SrcLoc -> InputStream -> [Int] -> ParserState
initialState initLoc inp scStack = ParserState
  { _location       = initLoc
  , _input          = inp
  , _previousToken  = initToken
  , _startCodeStack = scStack
  , _indentStack    = [0]
  , _parenStack     = []
  , _lastEOL        = SpanEmpty
  , _comments       = []
  }


pushStartCode :: Int -> P ()
pushStartCode code = startCodeStack %= (code :)


popStartCode :: P ()
popStartCode = use startCodeStack >>= \case
  [] -> internalError "fatal error in lexer: attempt to pop empty start code stack"
  _ : rest -> startCodeStack .= rest


getStartCode :: P Int
getStartCode = use startCodeStack >>= \case
  [] ->
    internalError "fatal error in lexer: start code stack empty on `getStartCode`"
  code : _ -> pure code


pushIndent :: Int -> P ()
pushIndent ind = indentStack %= (ind :)


popIndent :: P ()
popIndent = use indentStack >>= \case
  [] -> internalError "fatal error in lexer: attempt to pop empty indent stack"
  _ : rest -> indentStack .= rest


getIndent :: P Int
getIndent = use indentStack >>= \case
  []      -> internalError "fatal error in lexer: indent stack empty on `getIndent`"
  ind : _ -> pure ind


getIndentStackDepth :: P Int
getIndentStackDepth = indentStack `uses` length


pushParen :: Token -> P ()
pushParen symbol = parenStack %= (symbol :)


popParen :: P ()
popParen = use parenStack >>= \case
  []       -> internalError "fatal error in lexer: attempt to pop empty paren stack"
  _ : rest -> parenStack .= rest


getParen :: P (Maybe Token)
getParen = parenStack `uses` listToMaybe


getParenStackDepth :: P Int
getParenStackDepth = parenStack `uses` length


addComment :: Token -> P ()
addComment com = comments %= (com :)


getComments :: P [Token]
getComments = comments `uses` reverse
