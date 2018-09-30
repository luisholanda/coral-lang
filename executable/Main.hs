-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Data.Text as T
import Data.Text.IO as TIO
import qualified Language.Coral.Syntax.AST as A
import qualified Language.Coral.Syntax.Parser.Parser as P

main :: IO ()
main = do
  let example =
        T.unlines
          [ "async def test'foo(a: mut Int = -1) => Void:"
          , "    break"
          , "    x := 1"
          , "    mut x: Int = 0"
          , "    continue"
          ]
  case P.parseText example of
    Left e -> print e >> fail "parse error"
    Right r -> print r >> pure ()

parseFile :: String -> IO A.Module
parseFile file = do
  program <- TIO.readFile file
  case P.parseText program of
    Left e -> print e >> fail "parse error"
    Right r -> pure r
