{-# LANGUAGE UnicodeSyntax #-}
module Language.Coral.Syntax.Parser
  ( parseInput
  )
where

import Language.Coral.Syntax.AST               ( Module )
import Language.Coral.Syntax.Parser.ErrM       ( Err )
import Language.Coral.Syntax.Parser.Layout     ( fixLayout )
import Language.Coral.Syntax.Parser.ParGrammar ( myLexer, pProgram )
import Language.Coral.Syntax.Parser.Translate  ( translate )


parseInput ∷ String → Err Module
parseInput = (translate <$>) . pProgram . fixLayout True . myLexer
