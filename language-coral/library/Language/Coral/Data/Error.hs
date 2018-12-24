module Language.Coral.Data.Error where

import           Control.Exception
import           Data.Text.Prettyprint.Doc

import           Language.Coral.Data.SrcSpan
import           Language.Coral.Lexer.Token


data CoralError
  -- | An error from the lexer. Character found where it shouldn't be.
  = UnexpectedChar Char SrcLoc
  -- | An error from the parser. Token found where it shouldn't be.
  | UnexpectedToken Token
  -- | A generic error containing the error string.
  | StrError String
  deriving (Eq, Ord, Show)


instance Exception CoralError


instance Pretty CoralError where
  pretty (UnexpectedChar c loc) = pretty loc <+>
                                  "unexpected character:" <+>
                                  pretty c
  pretty (UnexpectedToken tok) = pretty (getSpan tok) <+>
                                  "unexpected token:" <+>
                                  pretty (show tok)
  pretty (StrError str) = pretty str
