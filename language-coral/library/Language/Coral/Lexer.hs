module Language.Coral.Lexer
  ( lexer
  , lexerKeepComments
  )
where

import           Prelude

import           Language.Coral.Data.InputStream
import           Language.Coral.Data.SrcSpan    ( initialSrcLoc )
import           Language.Coral.Lexer.Layout    ( fixLayout )
import           Language.Coral.Lexer.Lexer     ( lexToken
                                                , initStartCodeStack
                                                )
import           Language.Coral.Lexer.Token
import           Language.Coral.Lexer.Error
import           Language.Coral.Lexer.Monad


initLexerState :: InputStream -> LexerState
initLexerState inp = initialState initialSrcLoc inp initStartCodeStack


lexer :: InputStream -> Either LexerError [Token]
lexer = lexWith execLexer


lexerKeepComments :: InputStream -> Either LexerError ([Token], [Token])
lexerKeepComments = lexWith execLexerKeepComments


lexWith :: forall a
         . (L [Token] -> LexerState -> Either LexerError a)
        -> InputStream
        -> Either LexerError a
lexWith exec = exec _lexer . initLexerState


_lexer :: L [Token]
_lexer = fixLayout <$> loop []
 where
  loop toks = do
    tok <- lexToken
    case tok of
      TEOF{} -> pure $ reverse toks
      _      -> loop (tok : toks)
