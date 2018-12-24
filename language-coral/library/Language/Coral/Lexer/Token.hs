{-# OPTIONS_GHC -Wno-partial-fields #-}
module Language.Coral.Lexer.Token
  ( Token(..)
  , hasLiteral
  , TokenClass(..)
  , classifyToken
  )
where

import           Data.ByteString                ( ByteString )
import           Data.Data

import           Language.Coral.Data.SrcSpan


data Token
  -- Whitespaces
  = TIndent        { tSpan :: !SrcSpan } -- ^ Indentation: increase
  | TDedent        { tSpan :: !SrcSpan } -- ^ Indentation: decrease
  | TNewLine       { tSpan :: !SrcSpan } -- ^ New line

  -- Comments
  | TComLine       { tSpan :: !SrcSpan, literal :: !ByteString } -- ^ Single line comment
  | TComLines      { tSpan :: !SrcSpan, literal :: !ByteString } -- ^ Multi line comment

  -- Identifiers
  | TIdentifier    { tSpan :: !SrcSpan, literal :: !ByteString } -- ^ Identifier
  | TTypeName      { tSpan :: !SrcSpan, literal :: !ByteString } -- ^ Concrete type name
  | TQualIdent     { tSpan :: !SrcSpan, lits  :: ![ByteString] } -- ^ A qualified identifier
  | TQualType      { tSpan :: !SrcSpan, lits  :: ![ByteString] } -- ^ A qualified type

  -- Literals
  | TString        { tSpan :: !SrcSpan, literal :: !ByteString } -- ^ Literal string
  | TRawString     { tSpan :: !SrcSpan, literal :: !ByteString } -- ^ Literal raw string
  | TByteString    { tSpan :: !SrcSpan, literal :: !ByteString } -- ^ Literal bytestring
  | TFmtString     { tSpan :: !SrcSpan, literal :: !ByteString } -- ^ Literal format string
  | TRawFmtString  { tSpan :: !SrcSpan, literal :: !ByteString } -- ^ Literal raw format string
  | TRawByteString { tSpan :: !SrcSpan, literal :: !ByteString } -- ^ Literal raw bytestring
  | TInteger       { tSpan :: !SrcSpan, literal :: !ByteString, integer :: !Integer } -- ^ Literal integer
  | TFloat         { tSpan :: !SrcSpan, literal :: !ByteString, float :: {-# UNPACK #-} !Double } -- ^ Literal float
  | TImaginary     { tSpan :: !SrcSpan, literal :: !ByteString, imaginary :: {-# UNPACK #-} !Double } -- ^ Literal imaginary
  | TTrue          { tSpan :: !SrcSpan } -- ^ Literal @True@
  | TFalse         { tSpan :: !SrcSpan } -- ^ Literal @False@
  | TNone          { tSpan :: !SrcSpan } -- ^ Literal @None@
  | TEff           { tSpan :: !SrcSpan } -- ^ Literal @Eff@

  -- Keywords
  | TModule        { tSpan :: !SrcSpan } -- ^ Keyword \"module\"
  | TExports       { tSpan :: !SrcSpan } -- ^ Keyword \"exports\"
  | TUse           { tSpan :: !SrcSpan } -- ^ Keyword \"use\"
  | TAs            { tSpan :: !SrcSpan } -- ^ Keyword \"as\"
  | TIf            { tSpan :: !SrcSpan } -- ^ Keyword \"if\"
  | TUnless        { tSpan :: !SrcSpan } -- ^  Keyword \"unless\"
  | TElse          { tSpan :: !SrcSpan } -- ^ Keyword \"else\"
  | TWhile         { tSpan :: !SrcSpan } -- ^ Keyword \"while\"
  | TUntil         { tSpan :: !SrcSpan } -- ^ Keyword \"until\"
  | TFor           { tSpan :: !SrcSpan } -- ^ Keyword \"for\"
  | TIn            { tSpan :: !SrcSpan } -- ^ Keyword \"in\"
  | TTry           { tSpan :: !SrcSpan } -- ^ Keyword \"try\"
  | TCatch         { tSpan :: !SrcSpan } -- ^ Keyword \"catch\"
  | TFinally       { tSpan :: !SrcSpan } -- ^ Keyword \"finally\"
  | TWith          { tSpan :: !SrcSpan } -- ^ Keyword \"with\"
  | TType          { tSpan :: !SrcSpan } -- ^ Keyword \"type\"
  | TAlias         { tSpan :: !SrcSpan } -- ^ Keyword \"alias\"
  | TRecord        { tSpan :: !SrcSpan } -- ^ Keyword \"record\"
  | TEffect        { tSpan :: !SrcSpan } -- ^ Keyword \"effect\"
  | THandler       { tSpan :: !SrcSpan } -- ^  Keyword \"handler\"
  | TForall        { tSpan :: !SrcSpan } -- ^ Keyword \"forall\" or \"∀\"
  | TMut           { tSpan :: !SrcSpan } -- ^ Keyword \"mut\"
  | TBreak         { tSpan :: !SrcSpan } -- ^ Keyword \"break\"
  | TContinue      { tSpan :: !SrcSpan } -- ^ Keyword \"continue\"
  | TReturn        { tSpan :: !SrcSpan } -- ^ Keyword \"return\"

  -- Operators
  | TMult          { tSpan :: !SrcSpan } -- ^ Operator \"*\"
  | TDiv           { tSpan :: !SrcSpan } -- ^ Operator \"/\"
  | TFDiv          { tSpan :: !SrcSpan } -- ^ Operator \"//\"
  | TPow           { tSpan :: !SrcSpan } -- ^ Operator \"^\"
  | TMod           { tSpan :: !SrcSpan } -- ^ Operator \"%\"
  | TAdd           { tSpan :: !SrcSpan } -- ^ Operator \"+\"
  | TMinus         { tSpan :: !SrcSpan } -- ^ Operator \"-\"
  | TLShift        { tSpan :: !SrcSpan } -- ^ Operator \"<<\"
  | TRShift        { tSpan :: !SrcSpan } -- ^ Operator \">>\"
  | TBitAnd        { tSpan :: !SrcSpan } -- ^ Operator \"&\"
  | TBitOr         { tSpan :: !SrcSpan } -- ^ Operator \"|\"
  | TArrow         { tSpan :: !SrcSpan } -- ^ Operator \"->\"
  | TPipe          { tSpan :: !SrcSpan } -- ^ Operator \"|>\"
  | TCons          { tSpan :: !SrcSpan } -- ^ Operator \"::\"

  -- Boolean Operators
  | TNotIn         { tSpan :: !SrcSpan } -- ^ Operator \"not in\"
  | TNot           { tSpan :: !SrcSpan } -- ^ Operator \"not\"
  | TAnd           { tSpan :: !SrcSpan } -- ^ Operator \"and\"
  | TOr            { tSpan :: !SrcSpan } -- ^ Operator \"or\"
  | TLt            { tSpan :: !SrcSpan } -- ^ Operator \"<\"
  | TLe            { tSpan :: !SrcSpan } -- ^ Operator \"<=\"
  | TGt            { tSpan :: !SrcSpan } -- ^ Operator \">\"
  | TGe            { tSpan :: !SrcSpan } -- ^ Operator \">=\"
  | TNe            { tSpan :: !SrcSpan } -- ^ Operator \"!=\"
  | TEq            { tSpan :: !SrcSpan } -- ^ Operator \"==\"

  -- Delimiters
  | TLParen        { tSpan :: !SrcSpan } -- ^ Delimiter \"(\"
  | TRParen        { tSpan :: !SrcSpan } -- ^ Delimiter \")\"
  | TLBrack        { tSpan :: !SrcSpan } -- ^ Delimiter \"[\"
  | TRBrack        { tSpan :: !SrcSpan } -- ^ Delimiter \"]\"
  | TLCurly        { tSpan :: !SrcSpan } -- ^ Delimiter \"{\"
  | TRCurly        { tSpan :: !SrcSpan } -- ^ Delimiter \"}\"
  | TDot           { tSpan :: !SrcSpan } -- ^ Delimiter \".\"
  | TComma         { tSpan :: !SrcSpan } -- ^ Delimiter \",\"
  | TSemiColon     { tSpan :: !SrcSpan } -- ^ Delimiter \";\"
  | TColon         { tSpan :: !SrcSpan } -- ^ Delimiter \":\"
  | TMutAssign     { tSpan :: !SrcSpan } -- ^ Delimiter \"=\"
  | TDefine        { tSpan :: !SrcSpan } -- ^ Delimiter \":=\"
  | TFatArrow      { tSpan :: !SrcSpan } -- ^ Delimiter \"=>\"
  | TLArrow        { tSpan :: !SrcSpan } -- ^ Delimiter \"->\"

  -- Special Cases
  | TEOF           { tSpan :: !SrcSpan } -- ^ End of File
  deriving (Show, Eq, Ord, Data)


instance Span Token where
  getSpan = tSpan


-- | Test if a token has its literal source content
hasLiteral :: Token -> Bool
hasLiteral = \case
  TComLine{}    -> True
  TComLines{}   -> True
  TIdentifier{} -> True
  TTypeName{}   -> True
  TString{}     -> True
  TInteger{}    -> True
  TFloat{}      -> True
  TImaginary{}  -> True
  _             -> False


data TokenClass
  = Comment
  | Number
  | Boolean
  | Identifier
  | Punctuation
  | Bracket
  | Layout
  | Keyword
  | String
  | Operator
  | Assigment
  deriving (Show, Eq, Ord)


classifyToken :: Token -> TokenClass
classifyToken = \case
  -- Whitespaces
  TIndent{}        -> Layout
  TDedent{}        -> Layout
  TNewLine{}       -> Layout

  -- Comments
  TComLine{}       -> Comment
  TComLines{}      -> Comment

  -- Identifiers
  TIdentifier{}    -> Identifier
  TTypeName{}      -> Identifier
  TQualIdent{}     -> Identifier
  TQualType{}      -> Identifier

  -- Literals
  TString{}        -> String
  TByteString{}    -> String
  TFmtString{}     -> String
  TRawString{}     -> String
  TRawByteString{} -> String
  TRawFmtString{}  -> String
  TInteger{}       -> Number
  TFloat{}         -> Number
  TImaginary{}     -> Number
  TTrue{}          -> Boolean
  TFalse{}         -> Boolean
  TNone{}          -> Identifier
  TEff{}           -> Identifier

  -- Keywords
  TModule{}        -> Keyword
  TExports{}       -> Keyword
  TUse{}           -> Keyword
  TAs{}            -> Keyword
  TIf{}            -> Keyword
  TUnless{}        -> Keyword
  TElse{}          -> Keyword
  TWhile{}         -> Keyword
  TUntil{}         -> Keyword
  TFor{}           -> Keyword
  TIn{}            -> Keyword
  TTry{}           -> Keyword
  TCatch{}         -> Keyword
  TFinally{}       -> Keyword
  TWith{}          -> Keyword
  TType{}          -> Keyword
  TAlias{}         -> Keyword
  TRecord{}        -> Keyword
  TEffect{}        -> Keyword
  THandler{}       -> Keyword
  TForall{}        -> Keyword
  TMut{}           -> Keyword
  TBreak{}         -> Keyword
  TContinue{}      -> Keyword
  TReturn{}        -> Keyword

  -- Operators
  TMult{}          -> Operator
  TDiv{}           -> Operator
  TFDiv{}          -> Operator
  TPow{}           -> Operator
  TMod{}           -> Operator
  TAdd{}           -> Operator
  TMinus{}         -> Operator
  TLShift{}        -> Operator
  TRShift{}        -> Operator
  TBitAnd{}        -> Operator
  TArrow{}         -> Operator
  TPipe{}          -> Operator
  TCons{}          -> Operator

  -- Boolean Operators
  TNotIn{}         -> Operator
  TNot{}           -> Operator
  TAnd{}           -> Operator
  TOr{}            -> Operator
  TLt{}            -> Operator
  TLe{}            -> Operator
  TGt{}            -> Operator
  TGe{}            -> Operator
  TNe{}            -> Operator
  TEq{}            -> Operator

  -- Delimiters
  TLParen{}        -> Bracket
  TRParen{}        -> Bracket
  TLBrack{}        -> Bracket
  TRBrack{}        -> Bracket
  TLCurly{}        -> Bracket
  TRCurly{}        -> Bracket
  TDot{}           -> Punctuation
  TComma{}         -> Punctuation
  TSemiColon{}     -> Punctuation
  TColon{}         -> Punctuation
  TFatArrow{}      -> Punctuation
  TLArrow{}        -> Punctuation
  TBitOr{}         -> Punctuation
  TMutAssign{}     -> Assigment
  TDefine{}        -> Assigment

  -- Special Cases
  TEOF{}           -> Layout
