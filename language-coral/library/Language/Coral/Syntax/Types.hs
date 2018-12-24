module Language.Coral.Syntax.Types where

import           Language.Coral.Syntax.Names


data Type
  -- | A unification variable.
  -- Used during type checking
  = TyUnknown     !Int
  -- | A named type variable.
  -- Bounded by @forall@.
  -- e.g.
  -- > forall a . IO a
  -- @a@ in @IO a@ is a @TyVar@.
  | TyVar         !(Name 'Identifier)
  -- | A type constructor.
  -- A constructor of types,
  -- e.g. @Int@ or @IO@ in @forall a . IO a@.
  | TyConstructor !(Name 'Type)
  -- | A type application.
  -- @TyApp a b@ is equivalent to @a b@.
  | TyApp         !Type !Type
  -- | A function type.
  -- A special case is needed (instead of using @TyApp@)
  -- as in Coral we explicit name the arguments of a function
  -- in it's type.
  -- e.g.
  -- > (x: Int) -> Int.
  | TyFun         !(Name 'Identifier, Type) !Type
  -- | A universal quantification.
  -- Used to introduce new type variables.
  -- e.g.
  -- > forall a, b . (f : a -> a) -> (x: b) -> b
  | TyForAll      ![Name 'Identifier] !Type
  -- | A constrained type.
  -- e.g.
  -- > forall a r . Show a => a -> Eff (IO | r) ()
  | TyConstraint  ![Constraint] !Type
  -- | A type with kind annotation.
  | TyKinded      !Type !Kind
  -- | A @Eff@ type.
  --
  -- Holds a type list, representing the list of permited
  -- effects, the returning type of the computation and
  -- maybe a row identifier, representing a polymorphic
  -- effect list.
  -- e.g.
  -- > forall a r . Eff (IO | r) a
  | TyEff         ![Type] !Type !(Maybe (Name 'Identifier))
  -- | A row type.
  -- Represents the type of records.
  --
  -- Holds the list of (label, type) representing the record,
  -- and maybe a row identifier, representing the polymorphic
  -- part of the record.
  -- e.g.
  -- > forall r . { x: Int, y: String | r }
  | TyRow         ![(Name 'Identifier, Type)] !(Maybe (Name 'Identifier))
  -- | A type wrappend inside parenthesis.
  | TyParens      !Type
  deriving (Eq, Show)


-- A typeclass constraint
data Constraint = Constraint
  { constraintClass :: !(Name 'Type)
  -- ^ Constraint class name
  , constraintArgs  :: ![Type]
  -- ^ Type arguments
  } deriving (Eq, Show)


-- | Kinds
data Kind
  -- | Unification kind variable
  = KndUnknown Int
  -- | Function kind
  | KndFunc Kind Kind
  -- | A named kind
  | KndNamed !(Name 'Identifier)
  deriving (Eq, Show)
