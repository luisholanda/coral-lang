module Language.Coral.Lexer
  ( lex
  , lexer
  )
where

import           Prelude                 hiding ( lex )

import           Language.Coral.Lexer.InputStream
import           Language.Coral.Lexer.Lexer     ( lexToken
                                                , initStartCodeStack
                                                )
import           Language.Coral.Lexer.Token
import           Language.Coral.SrcSpan         ( initialSrcLoc )
import           Language.Coral.Parser.Error
import           Language.Coral.Parser.Monad


initLexState :: InputStream -> ParserState
initLexState inp = initialState initialSrcLoc inp initStartCodeStack


lex :: InputStream -> Either ParserError [Token]
lex inp = execParser lexer $ initLexState inp


lexer :: P [Token]
lexer = loop []
 where
  loop toks = do
    tok <- lexToken
    case tok of
      TEOF{} -> pure $ reverse toks
      _      -> loop (tok : toks)
