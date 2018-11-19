module Language.Coral.Lexer.Common where

import           Control.Lens
import           Control.Exception              ( throw )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Internal      as B

import           Language.Coral.Data.InputStream
import           Language.Coral.Data.SrcSpan
import           Language.Coral.Lexer.Token
import           Language.Coral.Parser.Error
import           Language.Coral.Parser.Monad


data BeginOf = BeginFile | BeginLine deriving Eq

type Action = SrcSpan -> Int -> InputStream -> P Token


endOfLine :: P Token -> Action
endOfLine lexToken s _len _str = do
  lastEOL .= spanStartPoint s
  lexToken


bolEndOfLine :: P Token -> Int -> Action
bolEndOfLine lexToken bol s len inp = do
  pushStartCode bol
  endOfLine lexToken s len inp


dedentation :: P Token -> Action
dedentation lexToken s _len _str = do
  top <- getIndent
  case startCol s `compare` top of
    EQ -> popStartCode >> lexToken
    LT -> popIndent >> pure dedentToken
    GT -> spanError s "indentation error"


-- | Hold's the logic behind the emission of indentation and new line tokens.
indentation :: P Token -> Int -> BeginOf -> Action
-- Check if we are at EOF. If yes, we need to generate a
-- newline in case we came here from BeginLine.
indentation lexToken _dedent bo _loc _len bs | inputStreamEmpty bs =
  popStartCode >> case bo of
    BeginLine -> newlineToken
    BeginFile -> lexToken
indentation lexToken dedent bo loc _len _bs = do
  popStartCode
  parenDepth <- getParenStackDepth
  if parenDepth > 0
    then lexToken
    else do
      top <- getIndent
      case startCol loc `compare` top of
        EQ -> case bo of
          BeginLine -> newlineToken
          BeginFile -> lexToken
        LT -> pushStartCode dedent >> newlineToken
        GT -> pushIndent (startCol loc) >> pure indentToken
  where indentToken = TIndent loc


symbol :: (SrcSpan -> Token) -> Action
symbol mkToken loc _ _ = pure $ mkToken loc


-- | Emits a token that hold the literal source and a parsed value.
--   This is usually used to emit tokens for literal, like numbers.
token
  :: forall a
   . (SrcSpan -> BS.ByteString -> a -> Token) -- ^ Token constructor
  -> (BS.ByteString -> a)                     -- ^ Bytestring parser
  -> Action
token mkToken r loc len str = pure $ mkToken loc literal (r literal)
  where literal = peekBytes len str


endOfFileToken, dedentToken :: Token
endOfFileToken = TEOF SpanEmpty
dedentToken = TDedent SpanEmpty


newlineToken :: P Token
newlineToken = lastEOL `uses` TNewLine

atEOLorEOF :: forall a . a -> AlexInput -> Int -> AlexInput -> Bool
atEOLorEOF _user _inputBefore _tokenLength (_loc, inputAfter) =
  inputStreamEmpty inputAfter || nextChar == '\n' || nextChar == '\r'
  where nextChar = fst $ takeChar inputAfter


notEOF :: forall a . a -> AlexInput -> Int -> AlexInput -> Bool
notEOF _user _inputBefore _tokenLength (_loc, inputAfter) =
  not $ inputStreamEmpty inputAfter


mkString :: (SrcSpan -> BS.ByteString -> Token) -> Action
mkString toToken loc len = pure . toToken loc . peekBytes len


stringToken :: SrcSpan -> BS.ByteString -> Token
stringToken = TString

rawStringToken :: SrcSpan -> BS.ByteString -> Token
rawStringToken = TRawString

byteStringToken :: SrcSpan -> BS.ByteString -> Token
byteStringToken = TByteString

formatStringToken :: SrcSpan -> BS.ByteString -> Token
formatStringToken = TFmtString

rawFormatStringToken :: SrcSpan -> BS.ByteString -> Token
rawFormatStringToken = TRawFmtString

rawByteStringToken :: SrcSpan -> BS.ByteString -> Token
rawByteStringToken = TRawByteString


openParen :: (SrcSpan -> Token) -> Action
openParen mkToken loc _len _str = do
  let tok = mkToken loc
  pushParen tok
  pure tok


closeParen :: (SrcSpan -> Token) -> Action
closeParen mkToken loc _len _str =
  let tok = mkToken loc
  in  getParen >>= \case
        Nothing -> spanError loc err
        Just open ->
          if matchParen open tok then popParen >> pure tok else spanError loc err
  where err = "lexical error: unmatched close paren"


matchParen :: Token -> Token -> Bool
matchParen TLCurly{} TRCurly{} = True
matchParen TLBrack{} TRBrack{} = True
matchParen TLParen{} TRParen{} = True
matchParen _         _         = False


{-| = Functionality required by Alex -}

type AlexInput = (SrcLoc,      -- ^ Current position
                  InputStream) -- ^ Current input stream


alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = error "alexInputPrevChar is not used."


alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (pos, inp)
  | inputStreamEmpty inp
  = Nothing
  | otherwise
  = let (!c, inp') = takeChar inp
        !pos'      = alexMove c pos
    in  Just (B.c2w c, (pos', inp'))


alexMove :: Char -> SrcLoc -> SrcLoc
alexMove '\n' = incLine 1
alexMove '\r' = id
alexMove _    = incColumn 1


lexicalError :: P a
lexicalError = do
  loc <- use location
  c   <- use $ input . to takeChar . _1
  throw $ UnexpectedChar c loc
