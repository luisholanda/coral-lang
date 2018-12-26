module Language.Coral.Parser
    ( Parse(..)
    )
where

import           Control.Monad

import           Language.Coral.Data.InputStream
import           Language.Coral.Lexer
import           Language.Coral.Lexer.Token
import           Language.Coral.Parser.Monad
import           Language.Coral.Parser.Parser
import           Language.Coral.Syntax.AST
import           Language.Coral.Syntax.Types




class Parse a where
    parser :: [Token] -> P a

    parse :: InputStream -> P a
    parse = parser <=< lexer


instance Parse (Module ()) where parser = parseModule
instance Parse (Definition ()) where parser = parseDef
instance Parse Type where parser = parseType
instance Parse (Def 'Sig ()) where parser = parseSign
instance Parse (Def 'Rec ()) where parser = parseRecrd
instance Parse (Statement ()) where parser = parseStmt
instance Parse (Expr ()) where parser = parseExpr
