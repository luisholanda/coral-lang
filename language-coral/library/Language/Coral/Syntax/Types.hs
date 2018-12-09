module Language.Coral.Syntax.Types where

import           Data.Data


import Language.Coral.Data.Ident
import Language.Coral.Data.SrcSpan


data Type
  -- | A unification variable.
  = TyUnknown     SrcSpan Int
  -- | A named type variable.
  | TyVar         SrcSpan Ident
  -- | A type constructor.
  | TyConstructor SrcSpan Ident
  -- | A type application.
  | TyApp         SrcSpan Type Type
  -- | A function type.
  -- A special case is needed (instead of using @TyApp@)
  -- as in Coral we explicit name the arguments of a function
  -- in its type.
  | TyFun         SrcSpan (Ident, Type) Type
  -- | A universal quantification.
  | TyForAll      SrcSpan Ident Type
  -- | A constrained type.
  | TyConstraint  SrcSpan [Constraint] Type
  -- | A type with kind annotation.
  | TyKinded      SrcSpan Type Kind
  -- | A row type, used in @Eff@ types.
  | TyRow         SrcSpan [Type] Ident
  deriving (Eq, Show, Data)


-- A typeclass constraint
data Constraint = Constraint
  { constraintClass :: !Ident
  -- ^ Constraint class name
  , constraintArgs :: [Type]
  -- ^ Type arguments
  } deriving (Eq, Show, Data)


-- | Kinds
data Kind
  -- | Unification kind variable
  = KndUnknown Int
  -- | Function kind
  | KndFunc Kind Kind
  -- | A named kind
  | KndNamed Ident
  deriving (Eq, Show, Data)