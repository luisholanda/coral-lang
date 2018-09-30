module Language.Coral.Syntax.Exception where

import Control.Exception
import Text.Parsec.Error

data CoralException =
  ParserExcp ParseError
