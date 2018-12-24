{-|
    Fixes the layout of the stream of tokens emited by Alex
to account for the following rules:

  1. Any line finishing with either an operator or a dot starts
     an implicit line continuation, e.g:

@
    example = [1, 2, 3].
      map(_ + 1).
      reduce(_ + _)
@

  2. Any indented line starting with either an operator or a dot
     starts an implicit line continuation.

@
    example = [1, 2, 3]
      .map(_ + 1)
      .reduce(_ + _)
@

    While we could solve the first rule in the Alex lexer, we prefer
to put the logic for this rule here to not increase more the size of
the parser's state record. As we're already making a pass over the
token's stream to solve the second rule, this will not be a problem
in terms of performance.

    The second one is basicaly check if the first token of a indented
line is a operator or a dot. This check, again, should be made when
we encounter a @'TNewLine'@ followed by @'TIndent'@, which represents
a indented line.

== Coral Layout Rules

    As Coral is a indentation based language, we should define the rules
to the layout of the language. Theses rules afect both the definition
of new blocks and the end of an expression.
    A block is any set of lines that have the same indentation.
    The end of an expression is more complicated to define, as an expression
can span over multiple lines.

=== Implicit Lines

    The rules for implicit lines continuation are the following:

    1. Any lines between matching brackets are considered to be of the same
logical line.

    2. If a line ends (ignoring comments) with an operator or a dot, the
next line should be indented and is considered of the same logical line.

    3. If a line is indented relative to the previous one /and/ starts with
an operator or a dot, it should be considered of the same logical line.
-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
module Language.Coral.Lexer.Layout (fixLayout) where

import Language.Coral.Data.SrcSpan
import Language.Coral.Lexer.Token


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
    TDot{}   -> True
    TComma{} -> True
    _        -> False
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
