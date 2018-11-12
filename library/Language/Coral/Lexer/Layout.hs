{-|
    Fixes the layout of the stream of tokens emited by Alex
to account for the following rules:

  * Any line finishing with either an operator or a dot starts
    an implicit line continuation, e.g:

@
    example = [1, 2, 3].
      map(_ + 1).
      reduce(_ + _)
@

  * Any indented line starting with either an operator or a dot
    starts an implicit line continuation.

@
    example = [1, 2, 3]
      .map(_ + 1)
      .reduce(_ + _)
@

   As stated in @'Language.Coral.Lexer.Common.indentation'@ function,
the first rule can be easily resolved by holding the last code-only
token emitted, i.e. not accounting comments or layout tokens, and
making the check when we would emit the indentation token.
   We prefer to put the logic for this rule here to not increase more
the size of the parser's state record. As we're already making a pass
over the token's stream to solve the second rule, this will not be a
problem in terms of performance.

   The second one is basicaly check if the first token of a indented
line is a operator or a dot. This check, again, should be made when
we encounter a @'TNewLine'@ followed by @'TIndent'@, which represents
a indented line.
-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.Coral.Lexer.Layout (fixLayout) where

import           Language.Coral.Lexer.Token
import           Language.Coral.SrcSpan


-- | Checks if a Token is a code token.
-- A code token is any token that is not a comment
-- or a layout logic.
isCodeToken :: Token -> Bool
isCodeToken (classifyToken -> Layout ) = False
isCodeToken (classifyToken -> Comment) = False
isCodeToken (classifyToken -> _      ) = True


-- | Checks if a @'Token'@ starts a implicit line continuation.
createsImplicit :: Token -> Bool
createsImplicit token = case classifyToken token of
  Operator    -> True
  Punctuation -> case token of
    TDot{} -> True
    _      -> False
  _ -> False


-- | Matches an indented line.
-- Returns the first token of the line and the rest of the stream.
pattern IndentedLine :: Token -> [Token] -> [Token]
pattern IndentedLine t ts <- TNewLine{} : TIndent{} : t : ts


-- | Matches a new line.
-- Returns the rest of the stream after the new line token.
pattern NewLine :: [Token] -> [Token]
pattern NewLine ts <- TNewLine{} : ts


-- | Fixes the layout of a stream of tokens.
--
-- This operation is indepotent.
-- prop> fixLayout . fixLayout = fixLayout
fixLayout :: [Token] -> [Token]
fixLayout = reverse . go [] (TNewLine SpanEmpty)
 where
  go :: [Token] -> Token -> [Token] -> [Token]
  -- First rule
  go rs l (NewLine ts)        | createsImplicit l = go rs l ts
  -- Second rule
  go rs l (IndentedLine t ts) | createsImplicit t = go (t : rs) l ts
  -- Continuation
  go rs l (t : ts) | isCodeToken t = go (t : rs) t ts
                   | otherwise     = go (t : rs) l ts
  -- We're done
  go rs _ []                       = rs
