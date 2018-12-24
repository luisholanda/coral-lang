{-|
    Defines the type that will hold any identifier in the
language's types. Providing fast comparision and other utilities.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Language.Coral.Data.Ident
  ( Ident
  , mkIdent
  , mkIdentS
  , mkIdentStr
  )
where

import Data.ByteString       hiding ( pack )
import Data.ByteString.Char8 ( pack )
import Data.Data             ( Data )
import Data.Hashable
import Data.String           ( IsString (..) )
import GHC.Generics          ( Generic )


-- | An identifier
data Ident = Ident
  { name  :: {-# UNPACK #-} !ByteString -- ^ Content of the identifier
  , _hash :: {-# UNPACK #-} !Int -- ^ Hash of the content, used for hash comparisions
  } deriving (Data, Generic)


instance Show Ident where
  show = show . name


instance IsString Ident where
  fromString = mkIdent . fromString


instance Eq Ident where
  i1 == i2 = hash i1 == hash i2 && name i1 == name i2
  i1 /= i2 = hash i1 /= hash i2 || name i1 /= name i2


instance Ord Ident where
  i1 `compare` i2 = case hash i1 `compare` hash i2 of
    EQ  -> name i1 `compare` name i2
    cmp -> cmp


instance Semigroup Ident where
  i1 <> i2 = mkIdent $ name i1 <> name i2
  {-# INLINE (<>) #-}


instance Monoid Ident where
  mempty = mkIdent ""


instance Hashable Ident where
  hash = _hash
  {-# INLINE hash #-}


mkIdent :: ByteString -> Ident
mkIdent bs = Ident bs (hash bs)
{-# INLINE mkIdent #-}

mkIdentStr :: String -> Ident
mkIdentStr = mkIdent . pack
{-# INLINE mkIdentStr #-}

mkIdentS :: Show s => s -> Ident
mkIdentS = mkIdentStr . show
{-# INLINE mkIdentS #-}
