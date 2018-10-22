module Language.Coral.Syntax.Parser.Layout
  ( fixLayout
  , getIndentLine
  ) where

import Control.Monad.State.Lazy
import Data.Maybe (fromJust, isJust, isNothing)
import Language.Coral.Syntax.Parser.ErrM
import Language.Coral.Syntax.Parser.LayoutGrammar
import Language.Coral.Syntax.Parser.LexGrammar
import Language.Coral.Syntax.Parser.ParGrammar (pIndentLine)

-- | Fix the layout of the Token stream received from the Alex lexer.
-- The idea is to insert '{' at the end of a indent line and a '}' when
-- the column of the tokens decrease. We also have to insert ';' at the
-- end (or start) of lines.
fixLayout :: Bool -> [Token] -> [Token]
fixLayout tp =
  res
    Nothing
    [ if tl
        then Implicit 1
        else Explicit
    ]
  where
    tl = tp && topLayout
    res ::
         Maybe Token -- ^ The previous token, if any.
      -> [Block] -- ^ A stack of layout blocks
      -> [Token] -- ^ Rest of tokens
      -> [Token]
    -- The stack should never be empty.
    res _ [] ts = error $ "Layout error: stack empty. Tokens: " ++ show ts
    res _ st (t0:ts)
      -- We found an open brace in the input,
      -- put an explicit layout block on the stack.
      -- This is done even if there was no layout word,
      -- to keep opening and closing braces.
      | isLayoutOpen t0 = moveAlong (Explicit : st) [t0] ts
    -- We are in an implicit layout block
    res pt st@(Implicit n:ns) rest@(t0:ts)
      | newLine pt t0 && column t0 < n
            -- Insert a closing brace after the previous token.
       =
        let b:ts' = addToken (afterPrev pt) layoutClose rest
                -- Repeat, with the current block removed from the stack.
         in moveAlong ns [b] ts'
      | newLine pt t0 &&
          column t0 == n &&
          not (isNothing pt ||
                isTokenIn [layoutSep, layoutOpen] (fromJust pt) ||
                isTokenIn ["except", "else", "elif"] t0)
            -- If we are in a implicit block, we must add
            -- a semicolon to the start of the line, this
            -- will end the previous statament.
       =
        let b':_ts'' = addToken (afterPrev pt) layoutSep rest
         in moveAlong st [b'] _ts''
    res pt st rest@(t0:ts)
        -- If we encounter a closing brace, exit the first explicit layout block.
      | isLayoutClose t0 =
        let st' = drop 1 (dropWhile isImplicit st)
         in if null st'
              then layoutErrorAt t0
              else moveAlong st' [t0] ts
      | isStartIndentLine t0
        -- `t0` starts a indent line. Thus, we must check if the line
        -- is indeed a indentline and inserta open brace after it.
       =
        let (ln, ts') = getIndentLine rest
         in case ln of
              Nothing -> if isTokenIn ["try"] t0
                            then moveAlong st [t0] ts
                            else syntaxErrorAt (head ts')
              Just (i0, indLn) ->
                let col
                      | null ts' = column i0
                      | otherwise = column (head ts')
                        -- insert an open brance after the indent line.
                    b:ts'' = addToken (nextPos i0) layoutOpen ts'
                        -- save the start column
                    st' = Implicit col : st
                        -- We already processed `indLn`
                 in moveAlong st' (indLn ++ [b]) ts''
    res pt st@(Implicit n:ns) rest@(t0:ts)
          -- Encounted a new line in an implicit layout block.
      | newLine pt t0 && column t0 == n
        -- Insert a semicolon after the previous token,
        -- unless we are the beginning of the file,
        -- or the previous token is a semicolon or open brace.
       =
        if isNothing pt || isTokenIn [layoutSep, layoutOpen] (fromJust pt) || isTokenIn ["except"] t0
          then moveAlong st [t0] ts
          else let b:t0':ts' = addToken (afterPrev pt) layoutSep rest
                in moveAlong st [b, t0'] ts'
    res _ st (t:ts) = moveAlong st [t] ts
    res (Just t) (Explicit:bs) []
      | null bs = []
      | otherwise = res (Just t) bs []
    res (Just t) (Implicit _n:bs) [] =
      let c = addToken (nextPos t) layoutClose []
       in moveAlong bs c []
    res Nothing _st [] = []
    moveAlong ::
         [Block] -- ^ The layout stack.
      -> [Token] -- ^ Any tokens just processed.
      -> [Token] -- ^ The rest of the tokens.
      -> [Token]
    moveAlong _ [] _ = error "Layout error: moveLaong got [] as old tokens"
    moveAlong st ot ts = ot ++ res (Just $ last ot) st ts
    newLine :: Maybe Token -> Token -> Bool
    newLine pt t0 = maybe True ((/= line t0) . line) pt
    syntaxErrorAt :: Token -> a
    syntaxErrorAt token =
      error $
      "Syntax error at (" ++ show lin ++ "," ++ show col ++ "): " ++ show token
      where
        col = column token
        lin = line token
    layoutErrorAt :: Token -> a
    layoutErrorAt t =
      error $
      "Layout error: Found " ++
      layoutClose ++
      " at (" ++
      show lin ++ "," ++ show col ++ ") without an explicit layout block."
      where
        col = column t
        lin = line t

data IndentLine = IndentLine
  { count :: Int
  , toks :: [Token]
  } deriving (Show)

-- | Gets the current complete indenetation start line.
-- When the first token of the stream start a indentation line,
-- we return the last position of the line in conjunction with
-- the line. When not, we return Nothing.
--
-- A indentation line is a line that starts a new indentaion
-- scope when it ends. The grammar for indentation lines are
-- defined in the `IndenttLine` grammar rule.
getIndentLine :: [Token] -> (Maybe (Token, [Token]), [Token])
getIndentLine ts =
  case runState (go ts) (IndentLine 0 []) of
    (Just (i0, r), s) -> (Just (i0, reverse $ toks s), r)
    (Nothing, s) -> (Nothing, toks s)
  where
    addToLine :: Token -> State IndentLine ()
    addToLine t =
      modify $ \IndentLine {count = c, toks} ->
        let c'
              | isOpenParen t = succ c
              | isCloseParen t = pred c
              | otherwise = c
         in IndentLine {count = c', toks = t : toks}
    go :: [Token] -> State IndentLine (Maybe (Token, [Token]))
    go [] = pure Nothing
    go rest@(r:rs') = do
      (c, tks) <- (,) <$> gets count <*> gets toks
      let tk = head tks
      if null tks || not (isIndentTok tk && c == 0)
        then addToLine r *> go rs'
        else pure $
             case pIndentLine (reverse tks) of
               Ok _ -> Just (tk, rest)
               Bad e -> Nothing

isIndentTok :: Token -> Bool
isIndentTok = isTS ":"

isOpenParen :: Token -> Bool
isOpenParen = isTS "("

isCloseParen :: Token -> Bool
isCloseParen = isTS ")"

isTS :: String -> Token -> Bool
isTS c t = strOf t == c
isTS _ _ = False

strOf :: Token -> String
strOf (PT _ (TS x _)) = x
strOf _ = ""

isStartIndentLine :: Token -> Bool
isStartIndentLine a = or $ map (`isTS` a) [ "def"
                                          , "if"
                                          , "elif"
                                          , "else"
                                          , "for"
                                          , "while"
                                          , "try"
                                          , "except"
                                          , "finally"
                                          , "type"
                                          , "data"
                                          ]
