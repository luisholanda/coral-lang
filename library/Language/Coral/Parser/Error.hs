module Language.Coral.Parser.Error where

import           Control.Exception
import           Data.Text.Prettyprint.Doc

import           Language.Coral.Lexer.Token
import           Language.Coral.SrcSpan


data ParserError
  -- | An error from the parser. Token found where it shouldn't be.
  = UnexpectedToken Token
  -- | An error from the lexer. Character found where it shouldn't be.
  | UnexpectedChar Char SrcLoc
  -- | A generic error containing the error string.
  | StrError String
  deriving (Eq, Ord, Show)


instance Exception ParserError


instance Pretty ParserError where
  pretty (UnexpectedToken t) = pretty (getSpan t) <+>
                               "unexpected token:" <+>
                               pretty (show t)
  pretty (UnexpectedChar c loc) = pretty loc <+>
                                  "unexpected character:" <+>
                                  pretty c
  pretty (StrError str) = pretty str
