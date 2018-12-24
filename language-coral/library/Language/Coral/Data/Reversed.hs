{-| Provides a reversed list-like structure for use in the parser.

The structure provides a O(1) append operation, with also
providing a O(log(min(m, n))) concatenation operation.
-}
module Language.Coral.Data.Reversed
    ( Reversed(..)
    , rsingleton
    , snoc
    , toList
    ) where

import Data.Foldable ( foldr', toList )
import Data.Sequence

import Language.Coral.Data.SrcSpan


-- | Fast reverse list-like structure.
newtype Reversed a = Rev { unReversed :: Seq a }
    deriving (Functor, Applicative, Monad)


instance Semigroup (Reversed a) where
    (Rev r1) <> (Rev r2) = Rev $ r2 >< r1


instance Monoid (Reversed a) where
    mempty = Rev empty


instance Foldable Reversed where
    foldr f z (Rev rs) = foldl (flip f) z rs


instance forall a . Span a => Span (Reversed a) where
    getSpan (Rev Empty)              = SpanEmpty
    getSpan (Rev (a :<| Empty))      = getSpan a
    getSpan (Rev (a :<| (_ :|> a'))) = getSpan a <-> getSpan a'


-- | Reverse append.
snoc :: forall a . Reversed a -> a -> Reversed a
snoc (Rev as) a = Rev $ as :|> a


-- | Creates a singleton @Reversed@.
rsingleton :: forall a . a -> Reversed a
rsingleton = Rev . singleton
