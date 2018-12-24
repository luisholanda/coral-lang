module Language.Coral.Parser.Commons
  ( exprToMatch
  )
where

import           Language.Coral.Syntax.AST
import           Language.Coral.Syntax.Names


-- | Converts a expression to a match.
-- Used to define @Match@ in the parser.
exprToMatch :: forall a . Expr a -> Maybe (Match a)
exprToMatch = go
 where
  go ExpTry{}            = Nothing
  go ExpOpTry{}          = Nothing
  go ExpBoolExpr{}       = Nothing
  go ExpAccess{}         = Nothing
  go ExpCall{}           = Nothing
  go ExpIndex{}          = Nothing
  go ExpOp{}             = Nothing
  go ExpComp{}           = Nothing
  go (ExpListCons h t a) = toMatch =<< exprToMatch h
   where
    toMatch h' = do
      t' <- exprToMatch t
      (`MatList` a) . (h' :) <$> case t' of
        MatList ms _       -> pure ms
        MatIdent{}         -> pure [t']
        MatLit LitList{} _ -> pure [t']
        _                  -> Nothing
  go (ExpIdent (Id ident) a) = case ident of
    IdentName _ -> Just $ MatIdent ident a
    _           -> Nothing
  go (ExpLit lit a) = Just $ MatLit lit a
