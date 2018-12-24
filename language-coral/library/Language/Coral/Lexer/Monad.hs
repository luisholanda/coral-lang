{-# LANGUAGE TemplateHaskell #-}
module Language.Coral.Lexer.Monad where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Maybe                     ( listToMaybe )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String

import           Language.Coral.Data.Error
import           Language.Coral.Data.InputStream
import           Language.Coral.Data.SrcSpan
import           Language.Coral.Lexer.Token


internalError :: forall a s . Doc s -> L a
internalError =
  throwError . StrError . renderString . layoutPretty defaultLayoutOptions


spanError :: forall a b . Span a => a -> String -> L b
spanError x str = internalError $ pretty (getSpan x) <+> pretty str


type L a = StateT LexerState (Either CoralError) a


data LexerState =
  LexerState
  { _location       :: !SrcLoc
  -- ^ Position at current input location.
  , _input          :: !InputStream
  -- ^ The current input.
  , _previousToken  :: !Token
  -- ^ The previous token.
  , _startCodeStack :: [Int]
  -- ^ A stack of start codes for the state of the lexer.
  , _indentStack    :: [Int]
  -- ^ A stack of source column positions of indenting levels.
  , _parenStack     :: [Token]
  -- ^ A stack of parens and brackets for indentation handling.
  , _lastEOL        :: !SrcSpan
  -- ^ Location of the last end-of-line encountered.
  , _comments       :: [Token]
  -- ^ Accumulated comments.
  } deriving Show
makeClassy ''LexerState


execLexer :: forall a . L a -> LexerState -> Either CoralError a
execLexer = evalStateT
{-# INLINE execLexer #-}


execLexerKeepComments
  :: forall a . L a -> LexerState -> Either CoralError (a, [Token])
execLexerKeepComments parser = evalStateT $ do
  result <- parser
  coms   <- getComments
  pure (result, coms)


runLexer :: forall a . L a -> LexerState -> Either CoralError (a, LexerState)
runLexer = runStateT
{-# INLINE runLexer #-}


initToken :: Token
initToken = TNewLine SpanEmpty


initialState :: SrcLoc -> InputStream -> [Int] -> LexerState
initialState initLoc inp scStack = LexerState
  { _location       = initLoc
  , _input          = inp
  , _previousToken  = initToken
  , _startCodeStack = scStack
  , _indentStack    = [0]
  , _parenStack     = []
  , _lastEOL        = SpanEmpty
  , _comments       = []
  }


pushStartCode :: Int -> L ()
pushStartCode code = startCodeStack %= (code :)


popStartCode :: L ()
popStartCode = use startCodeStack >>= \case
  [] -> internalError "fatal error in lexer: attempt to pop empty start code stack"
  _ : rest -> startCodeStack .= rest


getStartCode :: L Int
getStartCode = use startCodeStack >>= \case
  [] ->
    internalError "fatal error in lexer: start code stack empty on `getStartCode`"
  code : _ -> pure code


pushIndent :: Int -> L ()
pushIndent ind = indentStack %= (ind :)


popIndent :: L ()
popIndent = use indentStack >>= \case
  [] -> internalError "fatal error in lexer: attempt to pop empty indent stack"
  _ : rest -> indentStack .= rest


getIndent :: L Int
getIndent = use indentStack >>= \case
  []      -> internalError "fatal error in lexer: indent stack empty on `getIndent`"
  ind : _ -> pure ind


getIndentStackDepth :: L Int
getIndentStackDepth = indentStack `uses` length


pushParen :: Token -> L ()
pushParen symbol = parenStack %= (symbol :)


popParen :: L ()
popParen = use parenStack >>= \case
  []       -> internalError "fatal error in lexer: attempt to pop empty paren stack"
  _ : rest -> parenStack .= rest


getParen :: L (Maybe Token)
getParen = parenStack `uses` listToMaybe


getParenStackDepth :: L Int
getParenStackDepth = parenStack `uses` length


addComment :: Token -> L ()
addComment com = comments %= (com :)


getComments :: L [Token]
getComments = comments `uses` reverse
