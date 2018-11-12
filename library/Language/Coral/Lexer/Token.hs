module Language.Coral.Lexer.Token where

import           Data.ByteString                ( ByteString )
import           Data.Data

import           Language.Coral.SrcSpan



data Token
  -- Whitespaces
  = TIndent { tSpan :: !SrcSpan } -- ^ Indentation: increase
  | TDedent { tSpan :: !SrcSpan } -- ^ Indentation: decrease
  | TNewLine { tSpan :: !SrcSpan } -- ^ New line

  -- Comments
  | TComLine { tSpan :: !SrcSpan, literal :: !ByteString } -- ^ Single line comment
  | TComLines { tSpan :: !SrcSpan, literal :: !ByteString } -- ^ Multi line comment

  -- Identifiers
  | TIdentifier { tSpan :: !SrcSpan, literal :: !ByteString } -- ^ Identifier
  | TTypeName { tSpan :: !SrcSpan, literal :: !ByteString } -- ^ Concrete type name

  -- Literals
  | TString { tSpan :: !SrcSpan, literal :: !ByteString } -- ^ Literal string
  | TRawString { tSpan :: !SrcSpan, literal :: !ByteString } -- ^ Literal raw string
  | TByteString { tSpan :: !SrcSpan, literal :: !ByteString } -- ^ Literal bytestring
  | TFmtString { tSpan :: !SrcSpan, literal :: !ByteString } -- ^ Literal format string
  | TRawFmtString { tSpan :: !SrcSpan, literal :: !ByteString } -- ^ Literal raw format string
  | TRawByteString { tSpan :: !SrcSpan, literal :: !ByteString } -- ^ Literal raw bytestring
  | TInteger { tSpan :: !SrcSpan, literal :: !ByteString, integer :: !Integer } -- ^ Literal integer
  | TFloat { tSpan :: !SrcSpan, literal :: !ByteString, float :: !Double } -- ^ Literal float
  | TImaginary { tSpan :: !SrcSpan, literal :: !ByteString, imaginary :: !Double } -- ^ Literal imaginary
  | TTrue { tSpan :: !SrcSpan }-- ^ Literal @True@
  | TFalse { tSpan :: !SrcSpan } -- ^ Literal @False@
  | TNone { tSpan :: !SrcSpan }-- ^ Literal @None@

  -- Keywords
  | TModule { tSpan :: !SrcSpan } -- ^ Keyword \"module\"
  | TExports { tSpan :: !SrcSpan } -- ^ Keyword \"exports\"
  | TUse { tSpan :: !SrcSpan } -- ^ Keyword \"use\"
  | TAs { tSpan :: !SrcSpan } -- ^ Keyword \"as\"
  | TAsync { tSpan :: !SrcSpan } -- ^ Keyword \"async\"
  | TAwait { tSpan :: !SrcSpan } -- ^ Keyword \"await\"
  | TDef { tSpan :: !SrcSpan } -- ^ Keyword \"def\"
  | TIf { tSpan :: !SrcSpan } -- ^ Keyword \"if\"
  | TElif { tSpan :: !SrcSpan } -- ^ Keyword \"elif\"
  | TElse { tSpan :: !SrcSpan } -- ^ Keyword \"else\"
  | TWhile { tSpan :: !SrcSpan } -- ^ Keyword \"while\"
  | TFor { tSpan :: !SrcSpan } -- ^ Keyword \"for\"
  | TIn { tSpan :: !SrcSpan } -- ^ Keyword \"in\"
  | TTry { tSpan :: !SrcSpan } -- ^ Keyword \"try\"
  | TExcept { tSpan :: !SrcSpan } -- ^ Keyword \"except\"
  | TFinally { tSpan :: !SrcSpan } -- ^ Keyword \"finally\"
  | TWith { tSpan :: !SrcSpan } -- ^ Keyword \"with\"
  | TType { tSpan :: !SrcSpan } -- ^ Keyword \"type\"
  | TMut { tSpan :: !SrcSpan } -- ^ Keyword \"mut\"
  | TBreak { tSpan :: !SrcSpan } -- ^ Keyword \"break\"
  | TPass { tSpan :: !SrcSpan } -- ^ Keyword \"pass\"
  | TContinue { tSpan :: !SrcSpan } -- ^ Keyword \"continue\"

  -- Operators
  | TMult { tSpan :: !SrcSpan } -- ^ Operator \"*\"
  | TMatMult { tSpan :: !SrcSpan } -- ^ Operator \"@\"
  | TDiv { tSpan :: !SrcSpan } -- ^ Operator \"/\"
  | TFDiv { tSpan :: !SrcSpan } -- ^ Operator \"//\"
  | TPow { tSpan :: !SrcSpan } -- ^ Operator \"**\"
  | TMod { tSpan :: !SrcSpan } -- ^ Operator \"%\"
  | TAdd { tSpan :: !SrcSpan } -- ^ Operator \"+\"
  | TMinus { tSpan :: !SrcSpan } -- ^ Operator \"-\"
  | TLShift { tSpan :: !SrcSpan } -- ^ Operator \"<<\"
  | TRShift { tSpan :: !SrcSpan } -- ^ Operator \">>\"
  | TBitAnd { tSpan :: !SrcSpan } -- ^ Operator \"&\"
  | TBitOr { tSpan :: !SrcSpan } -- ^ Operator \"|\"
  | TBitXor { tSpan :: !SrcSpan } -- ^ Operator \"^\"
  | TArrow { tSpan :: !SrcSpan } -- ^ Operator \"->\"
  | TPipe { tSpan :: !SrcSpan } -- ^ Operator \"|>\"

  -- Boolean Operators
  | TNotIn { tSpan :: !SrcSpan } -- ^ Operator \"not in\"
  | TIs { tSpan :: !SrcSpan } -- ^ Operator \"is\"
  | TNot { tSpan :: !SrcSpan } -- ^ Operator \"not\"
  | TLt { tSpan :: !SrcSpan } -- ^ Operator \"<\"
  | TLe { tSpan :: !SrcSpan } -- ^ Operator \"<=\"
  | TGt { tSpan :: !SrcSpan } -- ^ Operator \">\"
  | TGe { tSpan :: !SrcSpan } -- ^ Operator \">=\"
  | TNe { tSpan :: !SrcSpan } -- ^ Operator \"!=\"
  | TEq { tSpan :: !SrcSpan } -- ^ Operator \"==\"
  | TAnd { tSpan :: !SrcSpan } -- ^ Operator \"and\"
  | TOr { tSpan :: !SrcSpan } -- ^ Operator \"or\"

  -- Delimiters
  | TLParen { tSpan :: !SrcSpan } -- ^ Delimiter \"(\"
  | TRParen { tSpan :: !SrcSpan } -- ^ Delimiter \")\"
  | TLBrack { tSpan :: !SrcSpan } -- ^ Delimiter \"[\"
  | TRBrack { tSpan :: !SrcSpan } -- ^ Delimiter \"]\"
  | TLCurly { tSpan :: !SrcSpan } -- ^ Delimiter \"{\"
  | TRCurly { tSpan :: !SrcSpan } -- ^ Delimiter \"}\"
  | TDot { tSpan :: !SrcSpan } -- ^ Delimiter \".\"
  | TComma { tSpan :: !SrcSpan } -- ^ Delimiter \",\"
  | TSemiColon { tSpan :: !SrcSpan } -- ^ Delimiter \";\"
  | TColon { tSpan :: !SrcSpan } -- ^ Delimiter \":\"
  | TAssign { tSpan :: !SrcSpan } -- ^ Delimiter \"=\"
  | TMutAssign { tSpan :: !SrcSpan } -- ^ Delimiter \".=\"

  -- Special Cases
  | TEOF { tSpan :: !SrcSpan } -- ^ End of File
  deriving (Show, Eq, Ord, Typeable, Data)


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

  -- Keywords
  TModule{}        -> Keyword
  TExports{}       -> Keyword
  TUse{}           -> Keyword
  TAs{}            -> Keyword
  TAsync{}         -> Keyword
  TAwait{}         -> Keyword
  TDef{}           -> Keyword
  TIf{}            -> Keyword
  TElif{}          -> Keyword
  TElse{}          -> Keyword
  TWhile{}         -> Keyword
  TFor{}           -> Keyword
  TIn{}            -> Keyword
  TTry{}           -> Keyword
  TExcept{}        -> Keyword
  TFinally{}       -> Keyword
  TWith{}          -> Keyword
  TType{}          -> Keyword
  TMut{}           -> Keyword
  TBreak{}         -> Keyword
  TPass{}          -> Keyword
  TContinue{}      -> Keyword

  -- Operators
  TMult{}          -> Operator
  TMatMult{}       -> Operator
  TDiv{}           -> Operator
  TFDiv{}          -> Operator
  TPow{}           -> Operator
  TMod{}           -> Operator
  TAdd{}           -> Operator
  TMinus{}         -> Operator
  TLShift{}        -> Operator
  TRShift{}        -> Operator
  TBitAnd{}        -> Operator
  TBitOr{}         -> Operator
  TBitXor{}        -> Operator
  TArrow{}         -> Operator
  TPipe{}          -> Operator

  -- Boolean Operators
  TNotIn{}         -> Operator
  TIs{}            -> Operator
  TNot{}           -> Operator
  TLt{}            -> Operator
  TLe{}            -> Operator
  TGt{}            -> Operator
  TGe{}            -> Operator
  TNe{}            -> Operator
  TEq{}            -> Operator
  TAnd{}           -> Operator
  TOr{}            -> Operator

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
  TAssign{}        -> Assigment
  TMutAssign{}     -> Assigment

  -- Special Cases
  TEOF{}           -> Layout