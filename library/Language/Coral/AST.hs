{-# LANGUAGE UnicodeSyntax #-}
module Language.Coral.Syntax.AST where

import Data.Data
import Data.Text as T

newtype Ident = Ident
  { name :: T.Text
  } deriving (Eq, Ord, Show, Typeable, Data)

data Module =
  Module { private :: Suite -- private declarations
         , public  :: Suite -- exposed declarations
         }
  deriving (Eq, Ord, Show, Typeable, Data)

type Suite = [Statement]

type DottedName = [Ident]

data ImportItem = ImportItem
  { itemName :: DottedName
  , asName   :: Maybe Ident
  } deriving (Eq, Ord, Show, Typeable, Data)

data FromItems
  = ImportAll
  | OnlyItems [Ident]
  deriving (Eq, Ord, Show, Typeable, Data)

data TypeDef
  = ADT { typeName     :: Type
        , constructors :: [Constructor]}
  | Record { typeName :: Type
           , fields   :: [Field]}
  deriving (Eq, Ord, Show, Typeable, Data)

data Constructor
   = ConstRecord { constName   :: Ident
                 , constFields :: [Field]}
   | Product { constName  :: Ident
             , constTyped :: [Type]}
   | Term { constName :: Ident }
   deriving (Eq, Ord, Show, Typeable, Data)

data Field = Field { fieldName :: Ident
                   , fieldType :: Type }
    deriving (Eq, Ord, Show, Typeable, Data)

data DataDef = DataDef { dataName   :: Type
                       , dataFields :: [DataField]}
  deriving (Eq, Ord, Show, Typeable, Data)

data DataField
  = Method Statement
  | Prop Ident Type
  deriving (Eq, Ord, Show, Typeable, Data)

data Statement
  = Import { importedItems :: [ImportItem] }
  | TypeD TypeDef
  | DataD DataDef
  | While { cond  :: Expr
          , body  :: Suite
          , else' :: Maybe Suite }
  | For { targets   :: [Expr]
        , generator :: [Expr]
        , body      :: Suite
        , else'     :: Maybe Suite }
  | AsyncFor Statement
  | Fun { funcName :: Ident
        , params   :: [Parameter]
        , retType  :: Type
        , body     :: Suite }
  | AsyncFun Statement
  | Conditional { guards :: [(Expr, Suite)]
                , else'  :: Maybe Suite }
  | Declaration { name' :: Ident
                , type' :: Type
                , value :: Maybe Expr }
  | MutDeclaration { name' :: Ident
                   , type' :: Type
                   , value :: Maybe Expr }
  | Assign { to   :: Expr
           , expr :: Expr }
  | MutAssign { to   :: Expr
              , expr :: Expr }
  | Return { returns :: [Expr] }
  | Try { body     :: Suite
        , excepts  :: [Handler]
        , handlers :: [Expr]
        , else'    :: Maybe Suite
        , finally  :: Maybe Suite }
  | Raise { expr :: Expr }
  | With { context :: [(Expr, Maybe Expr)]
         , body    :: Suite }
  | AsyncWith Statement
  | Pass
  | Break
  | Continue
  | Delete [Expr]
  | Expression Expr
  | Assert Expr
  deriving (Eq, Ord, Show, Typeable, Data)

data Decorator = Decorator
  { decName :: DottedName
  , args    :: [Argument]
  } deriving (Eq, Ord, Show, Typeable, Data)

data Parameter
  = Param { paramName    :: Ident
          , paramType    :: Type
          , paramDefault :: Maybe Expr }
  | VarParam { paramName :: Ident
             , paramType :: Type }
  | EndPositional
  deriving (Eq, Ord, Show, Typeable, Data)

data Argument
  = Arg Expr
  | VarArg Expr
  | KeywordArg Ident
               Expr
  deriving (Eq, Ord, Show, Typeable, Data)

data Handler = Handler
  { clause  :: ExceptClause
  , hanBody :: Suite
  } deriving (Eq, Ord, Show, Typeable, Data)

data ExceptClause
  = CatchAll
  | Catch [(Type, Maybe Ident)]
  deriving (Eq, Ord, Show, Typeable, Data)

data Comprehension = Comprehension
  { compExpr :: CompExpr
  , compFor  :: CompFor
  } deriving (Eq, Ord, Show, Typeable, Data)

data CompExpr
  = CompExpr Expr
  | CompDict DictKeyDatumList
  deriving (Eq, Ord, Show, Typeable, Data)

data CompFor = CompFor
  { async  :: Bool
  , exprs  :: [Expr]
  , inExpr :: Expr
  , iters  :: Maybe CompIter
  } deriving (Eq, Ord, Show, Typeable, Data)

data CompIf = CompIf
  { ifExpr  :: Expr
  , ifIters :: Maybe CompIter
  } deriving (Eq, Ord, Show, Typeable, Data)

data CompIter
  = IterFor CompFor
  | IterIf CompIf
  deriving (Eq, Ord, Show, Typeable, Data)

data Str
  = Str !T.Text
  | RawStr Str
  | FormatStr Str
  | ByteStr Str
  | SplitStr Str Str
  deriving (Eq, Ord, Show, Typeable, Data)

data Number
  = Int Integer
  | Float Double
  | Imaginary Double
  deriving (Eq, Ord, Show, Typeable, Data)

data Expr
  = Var Ident
  | Num Number
  | Bool Bool
  | None
  | String Str
  | Call { func     :: Expr
         , callArgs :: [Argument] }
  | Indexing Expr -- Indexed
             Expr -- Index
  | Cond { trueBranch  :: Expr
         , condition   :: Expr
         , falseBranch :: Expr }
  | BinaryOp Op
             Expr -- Left
             Expr -- Right
  | UnaryOp Op
            Expr
  -- Field access
  | Dot Expr
        Ident
  | Lambda [Parameter]
           Expr
  | Tuple [Expr]
  | Yield (Maybe YieldArg)
  | Slice (Maybe Expr) (Maybe Expr) (Maybe Expr)
  | Generator Comprehension
  | Await Expr
  | TryE Expr
  | ListComp Comprehension
  | List [Expr]
  | Dict [DictKeyDatumList]
  | DictComp Comprehension
  | Set [Expr]
  | SetComp Comprehension
  | Starred Expr
  | Paren Expr
  deriving (Eq, Ord, Show, Typeable, Data)

data YieldArg
  = YieldFrom Expr
  | YieldExpr Expr
  deriving (Eq, Ord, Show, Typeable, Data)

data DictKeyDatumList
  = DictMappingPair Expr
                    Expr
  | DictUnpacking Expr
  deriving (Eq, Ord, Show, Typeable, Data)

data Type
  = Type DottedName
  | GenType DottedName [Type]
  | FunType [Type] Type
  | ArgsType Type
  | FreeType Ident
  | MutType Type
  | ImplicitType
  deriving (Eq, Ord, Show, Typeable, Data)

data Op
  = And
  | Or
  | Not
  | Pow
  | LessThan
  | GreaterThan
  | Equals
  | GreaterThanEquals
  | LessThanEquals
  | NotEquals
  | In
  | Is
  | IsNot
  | NotIn
  | BinaryOr
  | BinaryAnd
  | BinaryXor
  | ShiftLeft
  | ShiftRight
  | Times
  | Plus
  | Minus
  | Divide
  | FloorDivide
  | Invert
  | Modulo
  | MatrixTimes
  | Pipe
  | Arrow
  deriving (Eq, Ord, Show, Typeable, Data)