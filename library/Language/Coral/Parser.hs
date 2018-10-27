{-# LANGUAGE UnicodeSyntax #-}
module Language.Coral.Parser
  ( parseInput
  )
where

import           Language.Coral.AST             ( Module )
import           Language.Coral.Parser.ErrM     ( Err )
import           Language.Coral.Parser.Layout   ( fixLayout )
import           Language.Coral.Parser.ParGrammar
                                                ( myLexer
                                                , pProgram
                                                )
import           Language.Coral.Parser.Translate
                                                ( translate )


parseInput :: String -> Err Module
parseInput = (translate <$>) . pProgram . fixLayout True . myLexer
