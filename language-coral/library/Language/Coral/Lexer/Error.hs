module Language.Coral.Lexer.Error where

import           Control.Exception
import           Data.Text.Prettyprint.Doc

import           Language.Coral.Data.SrcSpan
import           Language.Coral.Lexer.Token


data LexerError
  -- | An error from the lexer. Character found where it shouldn't be.
  = UnexpectedChar Char SrcLoc
  -- | A generic error containing the error string.
  | StrError String
  deriving (Eq, Ord, Show)


instance Exception LexerError


instance Pretty LexerError where
  pretty (UnexpectedChar c loc) = pretty loc <+>
                                  "unexpected character:" <+>
                                  pretty c
  pretty (StrError str) = pretty str
