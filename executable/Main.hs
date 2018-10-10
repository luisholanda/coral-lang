-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import qualified Language.Coral.Syntax.AST as A
import Language.Coral.Syntax.Parser
import Language.Coral.Syntax.Parser.ErrM
import Text.Pretty.Simple (pPrint)

testCode :: IO (String)
testCode = readFile "../test.cor"

main :: IO ()
main = do
  example <- testCode
  pPrint $ lexer example
  case parseInput example of
    Ok p -> pPrint p
    Bad e -> pPrint e
