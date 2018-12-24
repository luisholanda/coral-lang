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
  | StMutAssign (Match a) (Expr a) a
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
