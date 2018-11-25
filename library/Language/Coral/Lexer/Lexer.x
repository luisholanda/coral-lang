{
{-# LANGUAGE DataKinds #-}
module Language.Coral.Lexer.Lexer
  ( initStartCodeStack
  , lexToken
  , endOfFileToken
  , lexCont
  )
where

import           Control.Lens
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.UTF8             as BE
import           Data.ByteString.Read.Fractional
import           Data.ByteString.Read.Integral
import qualified Data.Map                         as Map
import           Data.Proxy

import           Language.Coral.Data.InputStream (peekBytes)
import           Language.Coral.Data.SrcSpan
import           Language.Coral.Lexer.Common
import           Language.Coral.Lexer.Token
import           Language.Coral.Parser.Monad
}

$lf              = \n
$cr              = \r
$eol             = [$lf $cr]
$not_eol         = ~$eol
$white           = [\ \n\r\f\v\t]
$white_no_eol    = $white # $eol
$dec             = 0-9
$non_zero_dec    = 1-9
$oct             = 0-7
$hex             = [$dec a-fA-F]
$bin             = 0-1
$string_char     = [^ $eol \" \\]
$bytestring_char = \0-\127 # [$eol \" \\]
$not_quote       = [. \n] # \"

@xid_start       = [a-z]
@xid_big_start   = [A-Z]
@xid_continue    = @xid_start | @xid_big_start | [$dec _ ']
@ident           = @xid_start @xid_continue*
@typename        = @xid_big_start @xid_continue*

@decimal = $non_zero_dec $dec*

@int_part         = $dec+
@expoent         = (e | E) (\+ | \-)? @int_part
@fraction        = \. @int_part

@float
    = (@int_part? @fraction)
    | @int_part \.
    | (@int_part \.? | @int_part? @fraction) @expoent

@eol_pattern    = $lf | $cr $lf | $lf $cr
@bs_prefix      = b | B
@rs_prefix      = r | R
@fs_prefix      = f | F
@rbs_prefix     = @bs_prefix @rs_prefix | @rs_prefix @bs_prefix
@fr_prefix      = @fs_prefix @rs_prefix | @rs_prefix @fs_prefix
@backslash_pair = \\ (\\ | \" | @eol_pattern | $bytestring_char)

@string         = \" ($string_char | @backslash_pair)* \"
@bytestring     = \" ($bytestring_char | @backslash_pair)* \"

@doc_limiter    = "---"
@documentation  = @doc_limiter [. $eol]* @doc_limiter

@comment        = "--" ($not_eol)*

tokens :-


<0> {
    @comment                     { token comment id }
    $white_no_eol+               ;
    @eol_pattern                 { bolEndOfLine lexToken bol }


    @float                       { token TFloat     (extract fractional) }
    @decimal                     { token TInteger   (extract $ integral' (Proxy :: Proxy 10)) }
    (@float | @int_part) (j | J) { token TImaginary (extract fractional . BS.init) }
    0 (o | O) $oct+              { token TInteger   (extract $ integral' (Proxy :: Proxy 8)) }
    0 (x | X) $hex+              { token TInteger   (extract $ integral' (Proxy :: Proxy 16)) }
    0 (b | B) $bin+              { token TInteger   (extract $ integral' (Proxy :: Proxy 2)) }


    @string                      { mkString stringToken }
    @rs_prefix @string           { mkString rawStringToken }
    @fs_prefix @string           { mkString formatStringToken }
    @fr_prefix @string           { mkString rawFormatStringToken }

    @bs_prefix @bytestring       { mkString byteStringToken }
    @rbs_prefix @bytestring      { mkString rawByteStringToken }


    @ident                       { \loc len str -> keywordOrIdent (peekBytes len str) loc }


    "("                          { openParen TLParen }
    ")"                          { closeParen TRParen }
    "["                          { openParen TLBrack }
    "]"                          { closeParen TRBrack }
    "{"                          { openParen TLCurly }
    "}"                          { closeParen TRCurly }

    "*"                          { symbol TMult }
    "/"                          { symbol TDiv }
    "//"                         { symbol TFDiv }
    "^"                          { symbol TPow }
    "%"                          { symbol TMod }
    "+"                          { symbol TAdd }
    "-"                          { symbol TMinus }
    "<<"                         { symbol TLShift }
    ">>"                         { symbol TRShift }
    "&"                          { symbol TBitAnd }
    "|"                          { symbol TBitOr }
    "->"                         { symbol TArrow }
    "=>"                         { symbol TFatArrow }
    "|>"                         { symbol TPipe }

    "not in"                     { symbol TNotIn }
    "is"                         { symbol TIs }
    "not"                        { symbol TNot }
    "<"                          { symbol TLt }
    "<="                         { symbol TLe }
    ">"                          { symbol TGt }
    ">="                         { symbol TGe }
    "!="                         { symbol TNe }
    "=="                         { symbol TEq }
    "and"                        { symbol TAnd }
    "or"                         { symbol TOr }

    "."                          { symbol TDot }
    ","                          { symbol TComma }
    ";"                          { symbol TSemiColon }
    ":"                          { symbol TColon }
    "="                          { symbol TAssign }
    ".="                         { symbol TMutAssign }
    ":="                         { symbol TDefine }
}


<dedent> ()                      { dedentation lexToken }


<bol> {
    @eol_pattern                 { endOfLine lexToken }
    ()                           { indentation lexToken dedent BeginLine }
}


<bof> {
    @eol_pattern                 { endOfLine lexToken }
    ()                           { indentation lexToken dedent BeginFile }
}


{
initStartCodeStack :: [Int]
initStartCodeStack = [0]


lexToken :: P Token
lexToken = do
  loc  <- use location
  inp  <- use input
  code <- getStartCode
  case alexScan (loc, inp) code of
    AlexError _ -> lexicalError
    AlexEOF -> use previousToken >>= \case
      TNewLine{} -> do
        depth <- getIndentStackDepth
        if depth <= 1
          then pure endOfFileToken
          else popIndent >> pure dedentToken
      _          -> previousToken <.= TNewLine (mkSrcSpan loc loc)
    AlexSkip (nextLoc, rest) _ -> do
      location .= nextLoc
      input .= rest
      lexToken
    AlexToken (nextLoc, rest) len action -> do
      location .= nextLoc
      input .= rest
      tok <- action (mkSrcSpan loc nextLoc) len inp
      previousToken <.= tok


lexCont :: (Token -> P a) -> P a
lexCont cont = lexLoop where
  lexLoop = do
    tok <- lexToken
    case tok of
      TComLine{}  -> addComment tok >> lexLoop
      TComLines{} -> addComment tok >> lexLoop
      _           -> cont tok


extract :: forall a
         . (ByteString -> Maybe (a, ByteString)) -- ^ Extractor function
        -> ByteString                            -- ^ Input
        -> a                                     -- ^ Result
extract f inp = case f inp of
  Just (val, _) -> val
  Nothing       -> error $ "internal error: failed to extract from " ++ show inp
{-# INLINE extract #-}


comment :: forall a . SrcSpan -> BS.ByteString -> a -> Token
comment loc lit _ = TComLine loc $ BE.drop 2 lit


keywordOrIdent :: BS.ByteString -> SrcSpan -> P Token
keywordOrIdent str loc = pure $ case Map.lookup str keywords of
  Just sym -> sym loc
  Nothing  -> TIdentifier loc str


keywords :: Map.Map BS.ByteString (SrcSpan -> Token)
keywords = Map.fromList [ ("False", TFalse)
                        , ("True", TTrue)
                        , ("None", TNone)
                        , ("module", TModule)
                        , ("exports", TExports)
                        , ("use", TUse)
                        , ("as", TAs)
                        , ("if", TIf)
                        , ("unless", TUnless)
                        , ("elif", TElif)
                        , ("else", TElse)
                        , ("while", TWhile)
                        , ("until", TUntil)
                        , ("for", TFor)
                        , ("in", TIn)
                        , ("try", TTry)
                        , ("catch", TCatch)
                        , ("finally", TFinally)
                        , ("with", TWith)
                        , ("type", TType)
                        , ("alias", TAlias)
                        , ("effect", TEffect)
                        , ("handler", THandler)
                        , ("forall", TForall)
                        , ("∀", TForall)
                        , ("mut",TMut)
                        , ("break", TBreak)
                        , ("continue", TContinue)
                        ]

}
