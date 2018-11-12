-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
module Main where


testCode :: IO String
testCode = readFile "/Users/luiscm/Projects/coral/test.cor"

main :: IO ()
main = do
  code <- testCode
  putStrLn code
