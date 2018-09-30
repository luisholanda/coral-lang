module Language.Coral.Syntax.Parser.Types where

import Data.Text (Text)
import qualified Text.Parsec.Indent as I

type Parser a = I.IndentParser Text () a

data ParseTypes a
  = Private a
  | Public a
  deriving (Show)

isPublic (Public _) = True
isPublic _ = False

extract (Public a) = a
extract (Private a) = a
