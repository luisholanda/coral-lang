module Language.Coral.Lexer
  ( lexer
  , lexerKeepComments
  )
where

import           Prelude

import           Language.Coral.Data.Error
import           Language.Coral.Data.InputStream
import           Language.Coral.Data.SrcSpan    ( initialSrcLoc )
import           Language.Coral.Lexer.Layout    ( fixLayout )
import           Language.Coral.Lexer.Lexer     ( lexToken
                                                , initStartCodeStack
                                                )
import           Language.Coral.Lexer.Token
import           Language.Coral.Lexer.Monad


initLexerState :: InputStream -> LexerState
initLexerState inp = initialState initialSrcLoc inp initStartCodeStack


lexer :: InputStream -> Either CoralError [Token]
lexer = lexWith execLexer


lexerKeepComments :: InputStream -> Either CoralError ([Token], [Token])
lexerKeepComments = lexWith execLexerKeepComments


lexWith
  :: forall a
   . (L [Token] -> LexerState -> Either CoralError a)
  -> InputStream
  -> Either CoralError a
lexWith exec = exec _lexer . initLexerState


_lexer :: L [Token]
_lexer = fixLayout <$> loop []
 where
  loop toks = do
    tok <- lexToken
    case tok of
      TEOF{} -> pure $ reverse toks
      _      -> loop (tok : toks)
