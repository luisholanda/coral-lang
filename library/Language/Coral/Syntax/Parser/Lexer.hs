module Language.Coral.Syntax.Parser.Lexer
  ( identifier
  , if'
  , else'
  , elif'
  , for'
  , in'
  , while'
  , return'
  , try'
  , except'
  , finally'
  , handler'
  , continue'
  , lambda'
  , def'
  , match'
  , mut'
  , with'
  , assert'
  , import'
  , as'
  , yield'
  , pass'
  , break'
  , raise'
  , async'
  , await'
  , pub'
  , type'
  , larrow'
  , block'
  , colon
  , equals
  , defEquals
  , comma
  , commaSep
  , commaSep1
  , parens
  , integer
  , float
  , whiteSpace
  , lexer
  , buildExprParser
  ) where

import Data.Char (isLower, isUpper)
import Data.Data hiding (Infix, Prefix)
import Data.Functor.Identity (Identity)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Coral.Syntax.AST hiding (else', type')
import Language.Coral.Syntax.Parser.Types
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Indent (IndentT)
import qualified Text.Parsec.Indent as I
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language

identifier :: Parser Text
identifier = T.pack <$> Tok.identifier lexer

reserved :: Text -> Parser ()
reserved = Tok.reserved lexer . T.unpack

reservedOp :: Text -> Parser ()
reservedOp = Tok.reservedOp lexer . T.unpack

parens :: Parser a -> Parser a
parens = Tok.parens lexer

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

reservedLexer w = reserved w *> pure ()

-- Keyword parsers
if' :: Parser ()
if' = reservedLexer "if"

else' :: Parser ()
else' = reservedLexer "else"

elif' :: Parser ()
elif' = reservedLexer "elif"

for' :: Parser ()
for' = reservedLexer "for"

in' :: Parser ()
in' = reservedLexer "in"

while' :: Parser ()
while' = reservedLexer "while"

return' :: Parser ()
return' = reservedLexer "return"

try' :: Parser ()
try' = reservedLexer "try"

except' :: Parser ()
except' = reservedLexer "except"

finally' :: Parser ()
finally' = reservedLexer "finally"

handler' :: Parser ()
handler' = reservedLexer "handler"

continue' :: Parser ()
continue' = reservedLexer "continue"

lambda' :: Parser ()
lambda' = reservedLexer "lambda"

def' :: Parser ()
def' = reservedLexer "def"

match' :: Parser ()
match' = reservedLexer "match"

mut' :: Parser ()
mut' = reservedLexer "mut"

with' :: Parser ()
with' = reservedLexer "with"

assert' :: Parser ()
assert' = reservedLexer "assert"

import' :: Parser ()
import' = reservedLexer "import"

as' :: Parser ()
as' = reservedLexer "as"

yield' :: Parser ()
yield' = reservedLexer "yield"

pass' :: Parser ()
pass' = reservedLexer "pass"

break' :: Parser ()
break' = reservedLexer "break"

raise' :: Parser ()
raise' = reservedLexer "raise"

async' :: Parser ()
async' = reservedLexer "async"

await' :: Parser ()
await' = reservedLexer "await"

pub' :: Parser ()
pub' = reservedLexer "pub"

type' :: Parser ()
type' = reservedLexer "type"

larrow' :: Parser ()
larrow' = reservedLexer "=>"

colon :: Parser ()
colon = Tok.colon lexer *> pure ()

comma :: Parser ()
comma = Tok.comma lexer *> pure ()

equals :: Parser ()
equals = Tok.lexeme lexer (char '=') *> pure ()

defEquals :: Parser ()
defEquals = Tok.lexeme lexer (string ":=") *> pure ()

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Tok.commaSep1 lexer

block' :: Parser a -> Parser [a]
block' par = colon *> (many (endOfLine <|> space)) *> I.block par

lexer :: Tok.GenTokenParser Text u (IndentT Identity)
lexer = Tok.makeTokenParser languageDef

languageDef :: GenLanguageDef Text u (IndentT Identity)
languageDef =
  LanguageDef
    { Tok.commentStart = "-|"
    , Tok.commentEnd = "|-"
    , Tok.commentLine = ";;"
    , Tok.nestedComments = True
    , Tok.opStart = oneOf "aion%*+/<=>@\\^|-~"
    , Tok.opLetter = oneOf "aiondrst%*+/<=>@\\^|-~"
    , Tok.identStart = letter <|> char '_'
    , Tok.identLetter = alphaNum <|> oneOf "-_'"
    , Tok.caseSensitive = True
    , Tok.reservedNames =
        [ "if"
        , "else"
        , "elif"
        , "for"
        , "in"
        , "while"
        , "return"
        , "try"
        , "except"
        , "finally"
        , "handler"
        , "continue"
        , "lambda"
        , "def"
        , "match"
        , "mut"
        , "with"
        , "assert"
        , "import"
        , "as"
        , "yield"
        , "pass"
        , "break"
        , "raise"
        , "async"
        , "await"
        , "pub"
        , "type"
        , "=>"
        ]
    , Tok.reservedOpNames =
        [ "+"
        , "-"
        , "*"
        , "/"
        , "<"
        , ">"
        , "<="
        , ">="
        , "**"
        , "~"
        , "//"
        , "^"
        , "|"
        , "%"
        , "@"
        , "<<"
        , ">>"
        , "and"
        , "or"
        , "is"
        , "not"
        , "|>"
        , "->"
        ]
    }

binary ::
     Text -> (a -> a -> a) -> Assoc -> Operator Text () (IndentT Identity) a
binary name fun assoc = Infix (reservedOp name *> pure fun) assoc

prefix :: Text -> (a -> a) -> Operator Text () (IndentT Identity) a
prefix name fun = Prefix (reservedOp name *> pure fun)

postfix :: Text -> (a -> a) -> Operator Text () (IndentT Identity) a
postfix name fun = Postfix (reservedOp name *> pure fun)

buildExprParser :: Parser Expr -> Parser Expr
buildExprParser expr = parser
  where
    parser = buildExpressionParser opTable term
    term = parens parser <|> expr

opTable :: [[Operator Text () (IndentT Identity) Expr]]
opTable =
  [ [binary "**" (BinaryOp Pow) AssocRight]
  , [ binary "*" (BinaryOp Times) AssocLeft
    , binary "@" (BinaryOp MatrixTimes) AssocLeft
    , binary "/" (BinaryOp Divide) AssocLeft
    , binary "//" (BinaryOp FloorDivide) AssocLeft
    , binary "%" (BinaryOp Modulo) AssocLeft
    ]
  , [ binary "+" (BinaryOp Plus) AssocLeft
    , binary "-" (BinaryOp Minus) AssocLeft
    ]
  , [ binary "<<" (BinaryOp ShiftLeft) AssocLeft
    , binary ">>" (BinaryOp ShiftRight) AssocLeft
    ]
  , [binary "&" (BinaryOp BinaryAnd) AssocLeft]
  , [binary "^" (BinaryOp BinaryXor) AssocLeft]
  , [binary "|" (BinaryOp BinaryOr) AssocLeft]
  , [ binary "->" (BinaryOp Arrow) AssocLeft
    , binary "|>" (BinaryOp Pipe) AssocLeft
    ]
  , [ binary "in" (BinaryOp In) AssocLeft
    , binary "not in" (BinaryOp NotIn) AssocLeft
    , binary "is" (BinaryOp Is) AssocLeft
    , binary "is not" (BinaryOp IsNot) AssocLeft
    , binary ">" (BinaryOp GreaterThan) AssocLeft
    , binary "<=" (BinaryOp GreaterThanEquals) AssocLeft
    , binary ">" (BinaryOp LessThan) AssocLeft
    , binary ">=" (BinaryOp LessThanEquals) AssocLeft
    , binary "==" (BinaryOp Equals) AssocLeft
    , binary "!=" (BinaryOp NotEquals) AssocLeft
    ]
  , [prefix "not" (UnaryOp Not)]
  , [binary "and" (BinaryOp And) AssocLeft]
  , [binary "or" (BinaryOp Or) AssocLeft]
  ]
