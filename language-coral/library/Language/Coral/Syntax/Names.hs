module Language.Coral.Syntax.Names
  ( NameTy(..)
  , Name(..)
  , AName(..)
  , IsIdentifier
  , Identifier(..)
  , IsTypeIdentifier
  , TypeIdentifier(..)
  , Qualified(..)
  )
where

import           Data.Data                      ( Data )

import           Language.Coral.Data.Ident


-- | Types of available names.
data NameTy
  -- | An identifier
  = Identifier
  -- | A module
  | Mod
  -- | A qualified identifier
  | QualIdent
  -- | A type name
  | Type
  -- | A qualified type name
  | QualType
  deriving (Eq, Show, Ord, Data)


-- | Holds the name of identifiers, modules and types.
-- Parametrized by the type of name that it holds.
data Name (ty :: NameTy) where
  IdentName     :: Ident                         -> Name 'Identifier
  TypeName      :: Ident                         -> Name 'Type
  ModName       :: [Ident]                       -> Name 'Mod
  QualIdentName :: Name 'Mod -> Name 'Identifier -> Name 'QualIdent
  QualTypeName  :: Name 'Mod -> Name 'Type       -> Name 'QualType
deriving instance Show (Name ty)
deriving instance Eq   (Name ty)


-- | Homogeneous name type.
-- To be used in lists of heterogeneous names.
data AName = forall (ty :: NameTy) . AName { unAName :: Name ty }
deriving instance Show AName


-- | States that a @Name ty@ is an identifier.
class IsIdentifier (ty :: NameTy) where
instance IsIdentifier 'Identifier
instance IsIdentifier 'QualIdent


-- | Homogeneous identifier type.
-- To be used in lists of heterogeneous identifiers.
data Identifier = forall ty . IsIdentifier ty
               => Id { unIdentifier :: Name ty }
deriving instance Show Identifier


-- | States that a @Name ty@ is a type identifier.
class IsTypeIdentifier (ty :: NameTy) where
instance IsTypeIdentifier 'Type
instance IsTypeIdentifier 'QualType


data TypeIdentifier = forall ty . IsTypeIdentifier ty
                   => TI { unTypeIdentifier ::  Name ty }
deriving instance Show TypeIdentifier


-- | Type class for qualified names
-- @a@ is the qualified name, @b@ is the type of name produced
-- when we disqualify a name of type @a@.
class Qualified a b | a -> b, b -> a where
  -- | Qualify a name, given a module name.
  qualify :: Name 'Mod -> Name b -> Name a

  -- | Disqualify a name.
  disqualify :: Name a -> Name b

  -- | Get the module name of a qualified name.
  module' :: Name a -> Name 'Mod

  -- | Checks if a qualified name is qualified with a given module.
  isQualifiedWith :: Name a -> Name 'Mod -> Bool
  isQualifiedWith name mod'' = module' name == mod''


instance Qualified 'QualIdent 'Identifier where
  qualify = QualIdentName
  module' (QualIdentName mod' _) = mod'
  disqualify (QualIdentName _ name) = name


instance Qualified 'QualType 'Type where
  qualify = QualTypeName
  module' (QualTypeName mod' _) = mod'
  disqualify (QualTypeName _ name) = name
