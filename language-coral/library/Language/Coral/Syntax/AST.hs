module Language.Coral.Syntax.AST where

import Data.ByteString ( ByteString )
import Data.Data       ( Data )

import Language.Coral.Data.Ident
import Language.Coral.Data.SrcSpan
import Language.Coral.Syntax.Types


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
  --  map: ∀ f a b . Funcor f => (fn: a -> b) -> (func: f a) -> f b
  -- @
  DefSig :: SrcSpan -> Ident  -> Type -> a -> Def 'Sig a
  -- | A function
  --
  -- e.g.
  -- @
  -- foo(a, b) :=
  --   return a + b
  -- @
  DefFun :: SrcSpan -> Ident  -> [Argument a] -> Block a -> a -> Def 'Fun a
  -- | A type
  --
  -- e.g.
  -- @
  -- type Maybe a := Nothing | Just a
  -- @
  DefTyp :: SrcSpan -> Type -> [Constructor a] -> a -> Def 'Typ a
  -- | An alias
  --
  -- e.g.
  -- @
  -- alias IntList = [Int]
  -- @
  DefAli :: SrcSpan -> Type -> Type -> a -> Def 'Ali a
  -- | A record
  --
  -- e.g.
  -- @
  -- record Row a b :=
  --   foo: [a]
  --   bar: b
  -- @
  DefRec :: SrcSpan -> Type -> [Def 'Sig a] -> a -> Def 'Rec a
  -- | An effect
  DefEff :: SrcSpan -> Type -> [Def 'Sig a] -> a -> Def 'Eff a
  -- | A handler
  DefHan :: SrcSpan -> Type -> [Def 'Sig a] -> a -> Def 'Han a
deriving instance (Show a) => Show (Def t a)
deriving instance Functor (Def t)


instance Span (Def dt a) where
  getSpan (DefSig s _ _ _)   = s
  getSpan (DefFun s _ _ _ _) = s
  getSpan (DefTyp s _ _ _)   = s
  getSpan (DefAli s _ _ _)   = s
  getSpan (DefRec s _ _ _)   = s
  getSpan (DefEff s _ _ _)   = s
  getSpan (DefHan s _ _ _)   = s


newtype Definition a = D { def :: forall (dk :: DefType) . Def dk a }


data Argument a
  = Arg SrcSpan Ident             -- ^ A common argument
  | ArgDef SrcSpan Ident (Expr a) -- ^ A argument with default value
  deriving (Eq, Show, Data, Functor)


instance Span (Argument a) where
  getSpan (Arg s _)      = s
  getSpan (ArgDef s _ _) = s


data Constructor a
  = ConsValue SrcSpan Ident a   -- ^ A value constructor, e.g. @Nil@
  | ConsProd SrcSpan [Type] a -- ^ A product constructor, e.g. @Cons(a, List a)@
  deriving (Eq, Show, Data, Functor)


instance Span (Constructor a) where
  getSpan (ConsValue s _ _) = s
  getSpan (ConsProd  s _ _) = s


-- | Block statements
data Statement a
  -- | A @continue@ statement
  = StContinue  SrcSpan a
  -- | A @break@ statement
  | StBreak     SrcSpan a
  -- | A @return@ statement
  | StReturn    SrcSpan (Expr a) a
  -- | A assignment statement
  | StAssign    SrcSpan (Match a) (Expr a)  a
  -- | A mutable assignment statement
  | StMutAssign SrcSpan (Match a) (Expr a) a
  -- | A expression statement
  | StExpr      SrcSpan (Expr a) a
  -- | A @if@ statement
  | StIf        SrcSpan [If a] (Maybe (Block a)) a
  -- | A @unless@ statement
  | StUnless    SrcSpan [If a] (Maybe (Block a)) a
  -- | A @try/except@ statement
  | StTry       SrcSpan [Ident] (Block a) [Catch a] (Maybe (Block a)) a
  -- | A @for@ statement
  | StFor       SrcSpan (Match a) (Expr a) (Block a) a
  -- | A @while@ statement
  | StWhile     SrcSpan (BoolExpr a) (Block a) a
  -- | A @until@ statement
  | StUntil     SrcSpan (BoolExpr a) (Block a) a
  -- | A local function defeinition
  | StFunDef    SrcSpan (Def 'Fun a) a
  -- | A local signature
  | StSig       SrcSpan (Def 'Sig a) a
  deriving (Show, Functor)


instance Span (Statement a) where
  getSpan (StContinue s _)      = s
  getSpan (StBreak s _)         = s
  getSpan (StReturn s _ _)      = s
  getSpan (StAssign s _ _ _)    = s
  getSpan (StMutAssign s _ _ _) = s
  getSpan (StExpr s _ _)        = s
  getSpan (StIf s _ _ _)        = s
  getSpan (StUnless s _ _ _)    = s
  getSpan (StTry s _ _ _ _ _)   = s
  getSpan (StFor s _ _ _ _)     = s
  getSpan (StWhile s _ _ _)     = s
  getSpan (StUntil s _ _ _)     = s
  getSpan (StFunDef s _ _)      = s
  getSpan (StSig s _ _)         = s


-- | A if block
data If a = If SrcSpan      -- ^ span of the block.
               (BoolExpr a) -- ^ Test of the block.
               (Block a)    -- ^ Statements that for mthe block.
               a
  deriving (Show, Functor)


instance Span (If a) where
  getSpan (If s _ _ _) = s


data Catch a = Catch SrcSpan Ident (Maybe Ident) (Block a) a
  deriving (Show, Functor)


instance Span (Catch a) where
  getSpan (Catch s _ _ _ _) = s


data BoolExpr a
  -- | @Expr@ that returns a boolean.
  = BlExpr SrcSpan (Expr a) a
  -- | A @not@ boolean expression.
  | BlNot  SrcSpan (BoolExpr a) a
  -- | A @and@ boolean expression.
  | BlAnd  SrcSpan (BoolExpr a) (BoolExpr a) a
  -- | A @or@ boolean expression.
  | BlOr   SrcSpan (BoolExpr a) (BoolExpr a) a
  -- | A comparison between two expressions.
  | BlComp SrcSpan CompOp (Expr a) (Expr a) a
  deriving (Eq, Show, Data, Functor)


instance Span (BoolExpr a) where
  getSpan (BlExpr s _ _)     = s
  getSpan (BlNot s _ _)      = s
  getSpan (BlAnd s _ _ _)    = s
  getSpan (BlOr s _ _ _)     = s
  getSpan (BlComp s _ _ _ _) = s


-- | Comparison operators
data CompOp
  = CmpEq    -- ^ Equality operator (@==@)
  | CmpLt    -- ^ Less than operator (@<@)
  | CmpGt    -- ^ Greater than operator (@>@)
  | CmpLe    -- ^ Less or equals operator (@<=@)
  | CmpGe    -- ^ Greater or equals operator (@>=@)
  | CmpIs    -- ^ Is operator (@is@)
  | CmpIsNo  -- ^ Is not operator (@is not@)
  | CmpNotIn -- ^ Not in operator (@not in@)
  | CmpIn    -- ^  In operator (@in@)
  deriving (Eq, Show, Data)


-- | Expressions
data Expr a
  -- | A boolean expression.
  = ExpBoolExpr SrcSpan (BoolExpr a) a
  -- | A identifier expression.
  | ExpIdent    SrcSpan Ident a
  -- | A literal expression.
  | ExpLit      SrcSpan (Lit a) a
  -- | A operator expression.
  | ExpOp       SrcSpan Op (Expr a) (Expr a) a
  -- | A function call expression.
  | ExpCall     SrcSpan (Expr a) [Parameter a] a
  -- | A field acess expression.
  | ExpAcess    SrcSpan (Expr a) Ident a
  -- | A indexing expression.
  | ExpIndex    SrcSpan (Expr a) (Expr a) a
  -- | A comprehension expression.
  | ExpComp     SrcSpan (Comprehension a) a
  -- | A @try@ expression.
  | ExpTry      SrcSpan (Expr a) a
  -- | A @try?@ expression.
  | ExpOpTry    SrcSpan (Expr a) a
  deriving (Eq, Show, Data, Functor)


instance Span (Expr a) where
  getSpan (ExpBoolExpr s _ _) = s
  getSpan (ExpIdent s _ _)    = s
  getSpan (ExpLit s _ _ )     = s
  getSpan (ExpOp s _ _ _ _)   = s
  getSpan (ExpCall s _ _ _)   = s
  getSpan (ExpAcess s _ _ _)  = s
  getSpan (ExpIndex s _ _ _)  = s
  getSpan (ExpComp s _ _)     = s
  getSpan (ExpTry s _ _)      = s
  getSpan (ExpOpTry s _ _)    = s


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
  | OpBOr   -- ^ Bitwise or operator (@|@)
  | OpArrow -- ^ Arrow operatator (@->@)
  | OpPipe  -- ^ Pipe operator (@|>@)
  deriving (Eq, Show, Data)


-- Function parameters.
data Parameter a
  = ParCommon SrcSpan (Expr a) a
  | ParNamed SrcSpan Ident (Expr a) a
  deriving (Eq, Show, Data, Functor)


instance Span (Parameter a) where
  getSpan (ParCommon s _ _)  = s
  getSpan (ParNamed s _ _ _) = s


-- | A list comprehension
-- e.g. @[x + 1 | x <- foo(2), x % 3 != 0]@
data Comprehension a
  = Comprehension SrcSpan      -- ^ Source span
                  (Expr a)     -- ^ Comprehension computation
                  [CompTest a] -- ^ Comprehension tests.
                  a
  deriving (Eq, Show, Data, Functor)


instance Span (Comprehension a) where
  getSpan (Comprehension s _ _ _) = s


data CompTest a
  -- | A comprehension iteration.
  -- e.g. @x <- foo@.
  = CompIter SrcSpan Ident (Expr a) a
  -- | A comprehension filter.
  -- e.g. @isJust x@.
  | CompFil  SrcSpan (BoolExpr a) a
  deriving (Eq, Show, Data, Functor)


instance Span (CompTest a) where
  getSpan (CompIter s _ _ _) = s
  getSpan (CompFil s _ _)    = s


data Lit a
  = LitInt SrcSpan !Integer ByteString a
  | LitFlt SrcSpan {-# UNPACK #-} !Double ByteString a
  | LitStr SrcSpan {-# UNPACK #-} !ByteString a
  | LitList SrcSpan [Expr a] a
  | LitTuple SrcSpan [Expr a] a
  deriving (Eq, Show, Data, Functor)


instance Span (Lit a) where
  getSpan (LitInt s _ _ _) = s
  getSpan (LitFlt s _ _ _) = s
  getSpan (LitStr s _ _) = s
  getSpan (LitList s _ _) = s
  getSpan (LitTuple s _ _) = s


-- | Pattern matching
data Match a
  = MatIdent SrcSpan Ident a
  | MatList SrcSpan [Match a] a
  | MatTupl SrcSpan [Match a] a
  | MatProd SrcSpan Ident [Match a] a
  | MatLit SrcSpan (Lit a) a
  deriving (Eq, Show, Data, Functor)
