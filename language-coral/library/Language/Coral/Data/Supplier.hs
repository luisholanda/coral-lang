{-| Provides a name generator that can be used when
generating new identifiers in analysis. -}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Coral.Data.Supplier where

import Control.Monad.State
import Data.ByteString.Char8 ( singleton )

import Language.Coral.Data.Ident


{-| Hability of supllying names. -}
class Monad m => MonadSupply m where
  {-# MINIMAL getIdent #-}
  getIdent :: m Ident

  getIdentFor :: Char -> m Ident
  getIdentFor c = (baseIdent <>) <$> getIdent
    where baseIdent = mkIdent $ singleton c


newtype SupplyT m a = SupplyT { unSupply :: StateT Supplier m a }
  deriving (Functor, Applicative, Monad, MonadState Supplier)

newName :: Monad m => SupplyT m Int
newName = do
  (name, gen) <- nextName <$> get
  put gen
  pure name

instance Monad m => MonadSupply (SupplyT m) where
  getIdent = mkIdentS <$> newName

  getIdentFor c = mkIdentStr . (c :) . show <$> newName


newtype Supplier = Supplier { names :: [Int] }


nameGenerator :: Supplier
nameGenerator = Supplier [0 ..]


nextName :: Supplier -> (Int, Supplier)
nextName (Supplier []      ) = error "Called nextName with exausted supplier."
nextName (Supplier (x : xs)) = (x, Supplier xs)
