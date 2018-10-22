{-# LANGUAGE UnicodeSyntax #-}
-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Language.Coral.Syntax.Parser
import Language.Coral.Syntax.Parser.ErrM
import Text.Pretty.Simple                ( pPrint )

testCode ∷ IO String
testCode = readFile "/Users/luiscm/Projects/coral/test.cor"

main ∷ IO ()
main = do
  example <- testCode
  case parseInput example of
    Ok  p -> pPrint p
    Bad e -> pPrint e
