module Language.Coral.Syntax.Parser
  ( parseInput
  , lexer
  ) where

import Language.Coral.Syntax.Parser.Layout as L
import Language.Coral.Syntax.Parser.ParGrammar as P

lexer = L.fixLayout True . P.myLexer

parseInput = P.pProgram . lexer
