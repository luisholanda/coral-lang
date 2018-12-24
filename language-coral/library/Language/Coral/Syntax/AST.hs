module Language.Coral.Syntax.AST
  ( Module(..)
  , Import(..)
  , Block
  , DefType(..)
  , Def(..)
  , Definition(..)
  , Argument(..)
  , Constructor(..)
  , Statement(..)
  , If(..)
  , Catch(..)
  , Expr(..)
  , BoolExpr(..)
  , CompOp(..)
  , Op(..)
  , Parameter(..)
  , Comprehension(..)
  , CompTest(..)
  , Lit(..)
  , StrTy(..)
  , Match(..)
  )
where

import           Data.ByteString                ( ByteString )
import           Data.Data                      ( Data )
import           Data.Text.Prettyprint.Doc

import           Language.Coral.Syntax.Names
import           Language.Coral.Syntax.Types


data Module = Module
  { moduleName  :: !(Name 'Mod)
  -- ^ Name of the module.
  , exports     :: !(Maybe [Identifier])
  -- ^ List of exported simbols.
  -- We put it inside a @Maybe@ to differentiate a missing
  -- export list to a empty export list.
  , imports     :: ![Import]
  , definitions :: ![Definition]
  -- ^ Definitions of the module.
  } deriving Show


data Import
  = ImpMod !(Name 'Mod)
  | ImpAli !(Name 'Mod) !(Name 'Identifier)
  deriving Show


-- | A block of statements
type Block a = [Statement a]


data DefType = Sig | Fun | Typ | Ali | Rec | Eff | Han
  deriving (Show, Data)


-- | Top level definitions
-- Parametrized by the type of the definition.
data Def (t :: DefType)  a  where
  -- | A signature
  --
  -- e.g.
  -- @
  --  map: ∀ f a b . Functor f => (fn: a -> b) -> (func: f a) -> f b
  -- @
  DefSig :: Name 'Identifier -> Type -> a -> Def 'Sig a
  -- | A function
  --
  -- e.g.
  -- @
  -- foo(a, b) :=
  --   return a + b
  -- @
  DefFun :: Name 'Identifier -> [Argument a] -> Block a -> a -> Def 'Fun a
  -- | A type
  --
  -- e.g.
  -- @
  -- type Maybe a := Nothing | Just a
  -- @
  DefTyp :: Type -> [Constructor a] -> a -> Def 'Typ a
  -- | An alias
  --
  -- e.g.
  -- @
  -- alias IntList = [Int]
  -- @
  DefAli :: Type -> Type -> a -> Def 'Ali a
  -- | A record
  --
  -- e.g.
  -- @
  -- record Row a b :=
  --   foo: [a]
  --   bar: b
  -- @
  DefRec :: Type -> [Def 'Sig a] -> a -> Def 'Rec a
  -- | An effect
  DefEff :: Type -> [Def 'Sig a] -> a -> Def 'Eff a
  -- | A handler
  DefHan :: Type -> [Def 'Sig a] -> a -> Def 'Han a
deriving instance Functor (Def t)
deriving instance Show a => Show (Def t a)


data Definition = forall a (dk:: DefType) . Show a => D { def :: Def dk a }
deriving instance Show Definition


data Argument a
  = Arg (Name 'Identifier)             -- ^ A common argument
  | ArgDef (Name 'Identifier) (Expr a) -- ^ A argument with default value
  deriving (Functor, Show)


data Constructor a
  -- | A value constructor, e.g. @Nil@
  = ConsValue (Name 'Type) a
  -- | A product constructor, e.g. @Cons(a, List a)@
  | ConsProd (Name 'Type) [Type] a
  deriving (Eq, Show, Functor)


-- | Block statements
data Statement a
  -- | A @continue@ statement
  = StContinue  a
  -- | A @break@ statement
  | StBreak     a
  -- | A @return@ statement
  | StReturn    (Expr a) a
  -- | A assignment statement
  | StAssign    (Match a) (Expr a)  a
  -- | A mutable variable definition
  | StMutDef    (Name 'Identifier) (Expr a) a
  -- | A mutable assignment statement
  | StMutAssign (Name 'Identifier) (Expr a) a
  -- | A expression statement
  | StExpr      (Expr a) a
  -- | A @if@ statement
  | StIf        [If a] (Maybe (Block a)) a
  -- | A @unless@ statement
  | StUnless    [If a] (Maybe (Block a)) a
  -- | A @try/except@ statement
  | StTry       [Identifier] (Block a) [Catch a] (Maybe (Block a)) a
  -- | A @for@ statement
  | StFor       (Match a) (Expr a) (Block a) a
  -- | A @while@ statement
  | StWhile     (BoolExpr a) (Block a) a
  -- | A @until@ statement
  | StUntil     (BoolExpr a) (Block a) a
  -- | A local function defeinition
  | StFunDef    (Def 'Fun a) a
  -- | A local signature
  | StSig       (Def 'Sig a) a
  deriving (Functor, Show)


-- | A if block
data If a = If (BoolExpr a) -- ^ Test of the block.
               (Block a)    -- ^ Statements that for mthe block.
               a
  deriving (Functor, Show)


data Catch a = Catch Identifier                 -- ^ Exception been catch.
                     (Maybe (Name 'Identifier)) -- ^ The alias to the exception.
                     (Block a)                  -- ^ The block of the catch.
                     a
  deriving (Functor, Show)


data BoolExpr a
  -- | @Expr@ that returns a boolean.
  = BlExpr (Expr a) a
  -- | A @not@ boolean expression.
  | BlNot  (BoolExpr a) a
  -- | A @and@ boolean expression.
  | BlAnd  (BoolExpr a) (BoolExpr a) a
  -- | A @or@ boolean expression.
  | BlOr   (BoolExpr a) (BoolExpr a) a
  -- | A comparison between two expressions.
  | BlComp CompOp (Expr a) (Expr a) a
  deriving (Functor, Show)


-- | Comparison operators
data CompOp
  = CmpEq   -- ^ Equality operator (@==@)
  | CmpNeq  -- ^ Inequality operator (@!=@)
  | CmpLt   -- ^ Less than operator (@<@)
  | CmpGt   -- ^ Greater than operator (@>@)
  | CmpLe   -- ^ Less or equals operator (@<=@)
  | CmpGe   -- ^ Greater or equals operator (@>=@)
  | CmpNin  -- ^ Not in operator (@not in@)
  | CmpIn   -- ^  In operator (@in@)
  deriving (Eq, Show, Data)


-- | Expressions
data Expr a
  -- | A boolean expression.
  = ExpBoolExpr (BoolExpr a) a
  -- | A identifier expression.
  | ExpIdent    Identifier a
  -- | A literal expression.
  | ExpLit      (Lit a) a
  -- | A operator expression.
  | ExpOp       Op (Expr a) (Expr a) a
  -- | A function call expression.
  | ExpCall     (Expr a) [Parameter a] a
  -- | A field acess expression.
  | ExpAccess   (Expr a) (Name 'Identifier) a
  -- | A indexing expression.
  | ExpIndex    (Expr a) (Expr a) a
  -- | A comprehension expression.
  | ExpComp     (Comprehension a) a
  -- | A @try@ expression.
  | ExpTry      (Expr a) a
  -- | A @try?@ expression.
  | ExpOpTry    (Expr a) a
  -- | A list construction.
  -- e.g. @(x::y)@
  | ExpListCons (Expr a) (Expr a) a
  -- | A expression wrapped inside parenthesis.
  | ExpParens (Expr a)
  deriving (Functor, Show)


-- Expressions Operators.
data Op
  = OpPlus  -- ^ Plus operator (@+@)
  | OpMinus -- ^ Minus operator (@-@)
  | OpMult  -- ^ Multiplier operator (@*@)
  | OpDiv   -- ^ Division operator (@/@)
  | OpFlDiv -- ^ FloorDiv operator (@//@)
  | OpPow   -- ^ Power operator (@^@)
  | OpMod   -- ^ Modulus operator (@%@)
  | OpLSft  -- ^ Left shift operator (@>>@)
  | OpRSft  -- ^ Right shift operator (@<<@)
  | OpBAnd  -- ^ Bitwise and operator (@&@)
  | OpArrow -- ^ Arrow operatator (@->@)
  | OpPipe  -- ^ Pipe operator (@|>@)
  deriving (Eq, Show, Data)


-- Function parameters.
data Parameter a
  = ParCommon (Expr a) a
  | ParNamed  (Name 'Identifier) (Expr a) a
  deriving (Functor, Show)


-- | A list comprehension
-- e.g. @[x + 1 | x <- foo(2), x % 3 != 0]@
data Comprehension a
  = Comprehension (Expr a)     -- ^ Comprehension computation
                  [CompTest a] -- ^ Comprehension tests.
                  a
  deriving (Functor, Show)


data CompTest a
  -- | A comprehension iteration.
  -- e.g. @x <- foo@.
  = CompIter (Name 'Identifier) (Expr a) a
  -- | A comprehension filter.
  -- e.g. @isJust(x)@.
  | CompFil  (BoolExpr a) a
  deriving (Functor, Show)


-- | Literal expressions
data Lit a
  = LitInt   !Integer ByteString a
  | LitFlt   {-# UNPACK #-} !Double ByteString a
  | LitImg   {-# UNPACK #-} !Double ByteString a
  | LitStr   !StrTy {-# UNPACK #-} !ByteString a
  | LitList  [Expr a] a
  | LitTuple [Expr a] a
  | LitSum   !TypeIdentifier a
  | LitBool  !Bool a
  | LitNone  a
  deriving (Functor, Show)


-- The types of literal strings
data StrTy = Str | Raw | Fmt | Byte | RawByte | RawFmt
  deriving (Eq, Show, Data, Ord)


-- | Pattern matching
data Match a
  -- | Creates a new identifier.
  = MatIdent (Name 'Identifier) a
  -- | Matches a list.
  | MatList  [Match a] a
  -- | Matches a tuple.
  | MatTupl  [Match a] a
  -- | Matches a product type constructor.
  | MatProd  TypeIdentifier [Match a] a
  -- | Matches a literal value.
  | MatLit   (Lit a) a
  deriving (Functor, Show)


-- * Pretty Instances

indent' :: forall a . Doc a -> Doc a
indent' = indent 2

lines' :: forall a . [Doc a] -> Doc a
lines' = align . concatWith (surround hardline)

indline' :: forall a . Doc a
indline' = flatAlt mempty (indent' hardline)


prettyBlock :: forall a ann . Block a -> Doc ann
prettyBlock = fuse Shallow . (hardline <>) . indent' . lines' . map pretty


instance Pretty Module where
  pretty Module{moduleName, exports, imports, definitions} = fuse Shallow $
    "module" <+> pretty moduleName <> (case exports of
      Just cs -> space <> "exports" <+> tupled (map pretty cs)
      Nothing -> mempty) <> hardline <> pImports <> pDefs
    where
      pImports = fuse Shallow . lines' $ map pretty imports
      pDefs = fuse Shallow . concatWith (surround $ hardline <> hardline) $ map pretty definitions


instance Pretty Import where
  pretty (ImpMod mod') = "use" <+> pretty mod'
  pretty (ImpAli mod' alias) = fuse Shallow $ "use" <+> pretty mod' <+> "as" <+> pretty alias


instance Pretty (Def ty a) where
  pretty (DefAli ali typ _) = fuse Shallow $ "alias" <+> pretty ali <+> align (":=" <+> pretty typ)
  pretty DefEff{} = "not implemented"
  pretty (DefFun fun args block _) = fuse Shallow $
    pretty fun <> tupled (map pretty args) <+> case block of
      [e] -> indline' <> ":=" <+> pretty e
      _ -> ":=" <> hardline <> fuse Shallow (indent' . vcat $ map pretty block)
  pretty DefHan{} = "not implemented"
  pretty (DefRec rec sigs _) = fuse Shallow . lines' $
    ("record" <+> pretty rec <+> ":=") : map (indent' . pretty) sigs
  pretty (DefTyp typ cons _) = fuse Shallow $
    "type" <+> pretty typ <+> case cons of
      [c] -> indline' <> ":=" <+> pretty c
      _   -> indline' <> align (":=" <+> fuse Shallow (concatWith (surround pipe) $ map pretty cons))
  pretty (DefSig name typ _) = fuse Shallow $ pretty name <> align (sep [" :", pretty typ])


instance Pretty Definition where
  pretty (D a) = pretty a


instance Pretty (Argument a) where
  pretty (Arg name) = pretty name
  pretty (ArgDef name val) = pretty name <> "=" <> pretty val


instance Pretty (Constructor a) where
  pretty (ConsValue name _) = pretty name
  pretty (ConsProd name fields _) = pretty name <> tupled (map pretty fields)


instance Pretty (Statement a) where
  pretty (StContinue _) = "continue"
  pretty (StBreak _) = "break"
  pretty (StReturn rets _) = "return" <+> pretty rets
  pretty (StAssign mat e _) = pretty mat <+> ":=" <+> pretty e
  pretty (StMutDef name val _) = "mut" <+> pretty name <+> ":=" <+> pretty val
  pretty (StMutAssign name val _) = pretty name <+> "=" <+> pretty val
  pretty (StExpr e _) = pretty e
  pretty (StIf blocks else' _) =
    let ifs = fuse Shallow . align . concatWith (surround $ hardline <> "else ") $ map pretty blocks
        pElse' = maybe emptyDoc (fuse Shallow) $ ("else" <+>) . prettyBlock <$> else'
     in ifs <> hardline <> pElse'
  pretty (StUnless [] _ _) = error "empty if list in StUnless."
  pretty (StUnless (If be b _:blocks) else' _) =
    let first = "unless" <+> pretty be <> prettyBlock b
        ifs = fuse Shallow . align . concatWith (surround $ hardline <> "else ") $ map pretty blocks
        pElse' = maybe emptyDoc (fuse Shallow) $ ("else" <+>) . prettyBlock <$> else'
     in first <> hardline <> "else" <> ifs <> hardline <> pElse'
  pretty (StTry hs b cs f _) = fuse Shallow $
    "try" <+> pWith <> prettyBlock b <> pCatchs <> pFin
    where
      pWith = case hs of
        []  -> emptyDoc
        [h] -> "with" <+> pretty h
        _   -> "with" <+> tupled (map pretty hs)
      pCatchs = fuse Shallow . lines' $ map pretty cs
      pFin = case f of
        Just b' -> "finally" <> prettyBlock b'
        Nothing -> emptyDoc
  pretty (StFor m e b _) = fuse Shallow $
    "for" <+> pretty m <+> "in" <+> pretty e <> prettyBlock b
  pretty (StWhile be b _) = fuse Shallow $
    "while" <+> pretty be <> prettyBlock b
  pretty (StUntil be b _) = fuse Shallow $
    "until" <+> pretty be <> prettyBlock b
  pretty (StFunDef d _) = pretty d
  pretty (StSig s _) = pretty s


instance Pretty (If a) where
  pretty (If be b _) = fuse Shallow $ "if" <+> pretty be <> prettyBlock b


instance Pretty (Catch a) where
  pretty (Catch i a b _) =
    let pAlias = maybe emptyDoc (enclose space space . pretty) a
     in fuse Shallow $ "catch" <+> pretty i <> pAlias <> prettyBlock b


instance Pretty (BoolExpr a) where
  pretty (BlExpr e _) = pretty e
  pretty (BlNot be _) = "not" <+> pretty be
  pretty (BlAnd be1 be2 _) = pretty be1 <+> indline' <> "and" <+> pretty be2
  pretty (BlOr be1 be2 _) = pretty be1 <+> indline' <> "or" <+> pretty be2
  pretty (BlComp c e1 e2 _) = pretty e1 <+> indline' <> pretty c <+> pretty e2


instance Pretty CompOp where
  pretty CmpEq   = "=="
  pretty CmpNeq  = "!="
  pretty CmpLt   = "<"
  pretty CmpGt   = ">"
  pretty CmpLe   = "<="
  pretty CmpGe   = ">="
  pretty CmpNin  = "not in"
  pretty CmpIn   = "in"


instance Pretty (Expr a) where
  pretty (ExpBoolExpr be _) = pretty be
  pretty (ExpIdent i _) = pretty i
  pretty (ExpLit lit _) = pretty lit
  pretty (ExpOp op e1 e2 _) = pretty e1 <+> indline' <> pretty op <+> pretty e2
  pretty (ExpCall fun ps _) = pretty fun <> tupled (map pretty ps)
  pretty (ExpAccess e field _) = pretty e <> dot <> pretty field
  pretty (ExpIndex e ix _) = pretty e <> brackets (pretty ix)
  pretty (ExpComp comp _) = pretty comp
  pretty (ExpTry e _) = "try" <+> pretty e
  pretty (ExpOpTry e _) = "try?" <+> pretty e
  pretty (ExpListCons h t _) = pretty h <+> indline' <> "::" <+> pretty t
  pretty (ExpParens e) = parens $ pretty e


instance Pretty Op where
  pretty OpPlus  = "+"
  pretty OpMinus = "-"
  pretty OpMult  = "*"
  pretty OpDiv   = "/"
  pretty OpFlDiv = "//"
  pretty OpPow   = "^"
  pretty OpMod   = "%"
  pretty OpLSft  = ">>"
  pretty OpRSft  = "<<"
  pretty OpBAnd  = "&"
  pretty OpArrow = "->"
  pretty OpPipe  = "|>"


instance Pretty (Parameter a) where
  pretty (ParCommon e _) = pretty e
  pretty (ParNamed n e _) = pretty n <> "=" <> pretty e


instance Pretty (Comprehension a) where
  pretty (Comprehension e ts _) = fuse Shallow . brackets
                                $ pretty e <+> pipe
                              <+> concatWith (surround comma) (map pretty ts)


instance Pretty (CompTest a) where
  pretty (CompIter n e _) = pretty n <+> "<-" <+> pretty e
  pretty (CompFil be _) = pretty be


instance Pretty (Lit a) where
  pretty (LitInt i _ _) = pretty i
  pretty (LitFlt d _ _) = pretty d
  pretty (LitImg d _ _) = "i" <> pretty d
  pretty (LitStr p b _) = pretty p <> pretty (show b)
  pretty (LitList ts _) = prettyList ts
  pretty (LitTuple ts _) = tupled . map pretty $ ts
  pretty (LitSum i _) = pretty i
  pretty (LitBool b _) = pretty b
  pretty (LitNone _) = "None"


instance Pretty StrTy where
  pretty Str = emptyDoc
  pretty Raw = "r"
  pretty Fmt = "f"
  pretty Byte = "b"
  pretty RawByte = "rb"
  pretty RawFmt = "rf"


instance Pretty (Match a) where
  pretty (MatIdent i _) = pretty i
  pretty (MatList ts _) = prettyList ts
  pretty (MatTupl ts _) = tupled $ map pretty ts
  pretty (MatProd typ as _) = pretty typ <> tupled (map pretty as)
  pretty (MatLit lit _) = pretty lit
