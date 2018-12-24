module Language.Coral.Syntax.Types where

import           Data.Bifunctor
import           Data.Text.Prettyprint.Doc

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


-- | A typeclass constraint
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


-- | Pretty instances

instance Pretty Type where
  pretty (TyUnknown i) = "t" <> pretty i
  pretty (TyVar typ) = pretty typ
  pretty (TyConstructor typ) = pretty typ
  pretty (TyApp typ1 typ2) = pretty typ1 <+> pretty typ2
  pretty (TyFun (IdentName param, ptyp) ret) =
    if param == mempty
      then prettyFun (pretty ptyp) (pretty ret)
      else prettyFun (parens $ pretty param <+> colon <+> pretty ptyp)
                     (pretty ret)
    where
      prettyFun p r = align $ sep [p, "->" <+> r]
  pretty (TyForAll as typ) = align $ sep [ "forall" <+> concatWith (surround comma) (map pretty as)
                                         , "."
                                         , pretty typ ]
  pretty (TyConstraint cs typ) =
    case cs of
      [c] -> align $ sep [pretty c, "=>" <+> pretty typ]
      _   -> align $ sep [tupled $ map pretty cs, "=>" <+> pretty typ]
  pretty (TyKinded typ kind) = pretty typ <+> ":" <+> pretty kind
  pretty (TyEff effs ret row) = "Eff" <+> prettyEffs <+> pretty ret
    where
      prettyEffs = case row of
        Just r -> parens $ pEff <+> pipe <+> pretty r
        Nothing -> case effs of
          [_] -> parens pEff
          _   -> pEff
      pEff = case effs of
        [e] -> pretty e
        _   -> concatWith (surround comma) $ map pretty effs
  pretty (TyRow rs row) = braces $ case row of
    Just r -> cat $ pLabels ++ [pipe <+> pretty r]
    Nothing -> cat pLabels
    where
      pLabels = head pL : map (", " <>) (tail pL)
      pL = map (uncurry (surround colon) . bimap pretty pretty) rs
  pretty (TyParens typ) = parens $ pretty typ


instance Pretty Constraint where
  pretty Constraint{constraintClass, constraintArgs} =
    hsep $ pretty constraintClass : map pretty constraintArgs


instance Pretty Kind where
  pretty (KndUnknown i) = "k" <> pretty i
  pretty (KndFunc k1 k2) = align $ sep [pretty k1, "->" <+> pretty k2]
  pretty (KndNamed kind) = pretty kind
