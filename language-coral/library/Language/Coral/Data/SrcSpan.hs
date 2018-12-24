{-# OPTIONS_GHC -Wno-partial-fields -Wno-incomplete-record-updates #-}
module Language.Coral.Data.SrcSpan where

import           Data.Data
import           Data.Text.Prettyprint.Doc


-- | Type which have a span
class Span a where
  getSpan :: a -> SrcSpan
  getSpan _ = SpanEmpty


instance forall a . Span a => Span [a] where
  -- | Assumes that the list is ordered by it source location.
  -- While this instance is not general enough, it's enough
  -- for the application in an AST, where the definitions satisfy
  -- the above assumption.
  getSpan []     = SpanEmpty
  getSpan [x]    = getSpan x
  getSpan (x:xs) = x <-> last xs


instance forall a . Span a => Span (Maybe a) where
  getSpan = maybe SpanEmpty getSpan


instance forall a b . (Span a, Span b) => Span (Either a b) where
  getSpan = either getSpan getSpan


instance forall a b . (Span a, Span b) => Span (a, b) where
  getSpan = uncurry spanning


-- | @spanning x y@ creates the smaller @SrcSpan@ over @x@ and @y@
spanning :: forall a b . (Span a, Span b) => a -> b -> SrcSpan
spanning x y = getSpan x <> getSpan y
{-# INLINE spanning #-}

-- | Operator version of @spanning@
(<->) :: forall a b . (Span a, Span b) => a -> b -> SrcSpan
(<->) = spanning
{-# INLINE (<->) #-}


data SrcLoc
  = SrcLoc { slocRow :: {-# UNPACK #-} !Int
           , slocCol :: {-# UNPACK #-} !Int
           }
  | NoLoc
  deriving (Eq, Ord, Show, Data)


instance Span SrcLoc where
  getSpan NoLoc = SpanEmpty
  getSpan loc   = SpanPoint loc


instance Pretty SrcLoc where
  pretty = pretty . getSpan


initialSrcLoc :: SrcLoc
initialSrcLoc = SrcLoc { slocRow = 1, slocCol = 0 }


-- | Decrease the column of a @SrcLoc@
decColumn :: Int -> SrcLoc -> SrcLoc
decColumn _ NoLoc = NoLoc
decColumn n loc | n < col   = loc { slocCol = col - n }
                | otherwise = loc
  where col = slocCol loc


-- | Increase the column of a @SrcLoc@
incColumn :: Int -> SrcLoc -> SrcLoc
incColumn _ NoLoc = error "internal error: try incColumn with NoLoc"
incColumn n loc@SrcLoc { slocCol = col } = loc { slocCol = col + n }


-- | Increases the line number of a @SrcLoc@ by one
incLine :: Int -> SrcLoc -> SrcLoc
incLine _ NoLoc                    = error "internal error: try incLine with NoLoc"
incLine n SrcLoc { slocRow = row } = SrcLoc { slocCol = 1, slocRow = row + n }


-- | Source location spanning a contiguous section of a file
data SrcSpan
  -- | A span which starts and ends on the same line.
  = SpanCoLin { spanRow      :: {-# UNPACK #-} !Int
              -- ^ Span's line
              , spanStartCol :: {-# UNPACK #-} !Int
              -- ^ Span's Start column
              , spanEndCol   :: {-# UNPACK #-} !Int
              -- ^ Span's end column
              }
  -- | A span which starts and ends on different lines
  | SpanMult { start :: !SrcLoc
             , end   :: !SrcLoc }
  -- | A span that is just one point in the file
  | SpanPoint { loc :: !SrcLoc }
  -- | No span information
  | SpanEmpty
  deriving (Eq, Ord, Show, Data)


instance Semigroup SrcSpan where
  (<>) = combineSrcSpans


instance Monoid SrcSpan where
  mempty = SpanEmpty


instance Span SrcSpan where
  getSpan = id


instance Pretty SrcSpan where
  pretty s@SpanCoLin{} = prettyMultiSpan s
  pretty s@SpanMult{} = prettyMultiSpan s
  pretty s@SpanPoint{} = parens $ pretty (startRow s) <> colon <>
                                  pretty (endCol s)
  pretty SpanEmpty = emptyDoc


prettyMultiSpan :: forall a . SrcSpan -> Doc a
prettyMultiSpan s = parens $ start <> "-" <> end
 where
  start = parens $ pretty (startRow s) <> colon <> pretty (startCol s)
  end   = parens $ pretty (endRow s) <> colon <> pretty (endCol s)


spanStartPoint :: SrcSpan -> SrcSpan
spanStartPoint SpanEmpty = SpanEmpty
spanStartPoint span'     = SpanPoint $ SrcLoc (startRow span') (startCol span')


-- | Makes a span point from the start of the span
spanStart :: SrcSpan -> SrcSpan
spanStart SpanEmpty          = SpanEmpty
spanStart s@SpanPoint{}      = s
spanStart SpanMult { start } = SpanPoint start
spanStart s = SpanPoint $ SrcLoc { slocRow = startRow s, slocCol = startCol s }

-- | Makes a span point from the end of the span
spanEnd :: SrcSpan -> SrcSpan
spanEnd SpanEmpty        = SpanEmpty
spanEnd s@SpanPoint{}    = s
spanEnd SpanMult { end } = SpanPoint end
spanEnd s = SpanPoint $ SrcLoc { slocRow = endRow s, slocCol = endCol s }


startLoc :: SrcSpan -> SrcLoc
startLoc SpanEmpty = NoLoc
startLoc x         = loc $ spanStart x


endLoc :: SrcSpan -> SrcLoc
endLoc SpanEmpty = NoLoc
endLoc x         = loc $ spanEnd x


-- | Makes a span from two locations.
-- Assumptions: either the location are the same, or
-- the left one preceeds the right one.
mkSrcSpan :: SrcLoc -> SrcLoc -> SrcSpan
mkSrcSpan NoLoc _     = SpanEmpty
mkSrcSpan _     NoLoc = SpanEmpty
mkSrcSpan loc1 loc2
  | line1 == line2 = if col2 <= col1
    then SpanPoint $ SrcLoc line1 col1
    else SpanCoLin line1 col1 col2
  | otherwise = SpanMult loc1 loc2
 where
  line1 = slocRow loc1
  col1  = slocCol loc1
  line2 = slocRow loc2
  col2  = slocCol loc2


-- | Combine two @SrcSpan@ into the smaller one that
-- spans over all the characters within both.
combineSrcSpans :: SrcSpan -> SrcSpan -> SrcSpan
combineSrcSpans SpanEmpty r         = r
combineSrcSpans l         SpanEmpty = l
combineSrcSpans start     end       = case row1 `compare` row2 of
  LT -> SpanMult (startLoc start) (endLoc end)
  GT -> SpanMult (startLoc end) (endLoc start)
  EQ -> case col1 `compare` col2 of
    EQ -> spanStart start
    LT -> SpanCoLin row1 col1 col2
    GT -> SpanCoLin row1 col2 col1
 where
  row1 = startRow start
  col1 = startCol start
  row2 = endRow end
  col2 = endCol end


startRow :: SrcSpan -> Int
startRow SpanCoLin { spanRow = row } = row
startRow SpanMult { start }          = slocRow start
startRow (SpanPoint loc)             = slocRow loc
startRow SpanEmpty                   = error "startRow called on empty span"


endRow :: SrcSpan -> Int
endRow SpanCoLin { spanRow = row } = row
endRow SpanMult { end }            = slocRow end
endRow (SpanPoint loc)             = slocRow loc
endRow SpanEmpty                   = error "endRow called on empty span"


startCol :: SrcSpan -> Int
startCol SpanCoLin { spanStartCol = col } = col
startCol SpanMult { start }               = slocCol start
startCol (SpanPoint loc)                  = slocCol loc
startCol SpanEmpty                        = error "startCol called on empty span"


endCol :: SrcSpan -> Int
endCol SpanCoLin { spanEndCol = col } = col
endCol SpanMult { end }               = slocCol end
endCol (SpanPoint loc)                = slocCol loc
endCol SpanEmpty                      = error "endLoc called on empty span"
