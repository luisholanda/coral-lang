module Language.Coral.Parser.Monad where

import           Control.Monad.Except
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String

import           Language.Coral.Data.Error
import           Language.Coral.Data.SrcSpan
import           Language.Coral.Lexer.Token


internalError :: forall a s . Doc s -> P a
internalError =
    throwError . StrError . renderString . layoutPretty defaultLayoutOptions


spanError :: forall a b . Span a => a -> String -> P b
spanError x str = internalError $ pretty (getSpan x) <+> pretty str


tokenError :: forall a . [Token] -> P a
tokenError (t : _) = throwError $ UnexpectedToken t
tokenError []      = throwError $ StrError "Expecting token, found none."


type P a = (Either CoralError) a
