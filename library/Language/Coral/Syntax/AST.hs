module Language.Coral.Syntax.AST where

import Data.Data
import Data.Text as T

data Ident = Ident
  { name :: !T.Text
  } deriving (Eq, Ord, Show, Typeable, Data)

data Module =
  Module Suite -- private declarations
         Suite -- exposed declarations
  deriving (Eq, Ord, Show, Typeable, Data)

type Suite = [Statement]

type DottedName = [Ident]

data ImportItem = ImportItem
  { itemName :: DottedName
  , asName :: Maybe Ident
  } deriving (Eq, Ord, Show, Typeable, Data)

data FromItems
  = ImportAll
  | OnlyItems [Ident]
  deriving (Eq, Ord, Show, Typeable, Data)

data Statement
  = Import { importedItems :: [ImportItem] }
  | While { cond :: Expr
          , body :: Suite
          , else' :: Suite }
  | For { targets :: [Expr]
        , generator :: Expr
        , body :: Suite
        , else' :: Suite }
  | AsyncFor Statement
  | Fun { funcName :: Ident
        , params :: [Parameter]
        , retType :: Type
        , body :: Suite }
  | AsyncFun Statement
  | Conditional { guards :: [(Expr, Suite)]
                , else' :: Suite }
  | Assign { to :: Expr
           , expr :: Expr }
  | MutAssign { to :: Expr
              , expr :: Expr }
  | Decorated { decorators :: [Decorator]
              , decoraterd :: Statement }
  | Return { retExpr :: Maybe Expr }
  | Try { body :: Suite
        , excepts :: [Handler]
        , handlers :: [DottedName]
        , else' :: Suite
        , finally :: Suite }
  | Raise { expr :: Expr }
  | With { context :: [(Expr, Maybe Expr)]
         , body :: Suite }
  | AsyncWith Statement
  | Pass
  | Break
  | Continue
  | Delete [Expr]
  | VarDef { varName :: Ident
           , varType :: Type
           , varValue :: Expr }
  | Expression Expr
  | Assert Expr
  deriving (Eq, Ord, Show, Typeable, Data)

data Decorator = Decorator
  { decName :: DottedName
  , args :: [Argument]
  } deriving (Eq, Ord, Show, Typeable, Data)

data Parameter
  = Param { paramName :: Ident
          , paramType :: Type
          , paramDefault :: Maybe Expr }
  | VarArg { paramName :: Ident
           , paramType :: Type }
  | EndPositional
  deriving (Eq, Ord, Show, Typeable, Data)

data Argument
  = ArgExpr Expr
  | ArgVarArgs Expr
  | ArgKeyword Ident
               Expr
  deriving (Eq, Ord, Show, Typeable, Data)

data Handler = Handler
  { clause :: ExceptClause
  , hanBody :: Suite
  } deriving (Eq, Ord, Show, Typeable, Data)

data ExceptClause =
  ExceptClause (Maybe (Expr, Maybe (Expr)))
  deriving (Eq, Ord, Show, Typeable, Data)

data Comprehension = Comprehension
  { compExpr :: CompExpr
  , compFor :: CompFor
  } deriving (Eq, Ord, Show, Typeable, Data)

data CompExpr
  = CompExpr Expr
  | CompDict DictKeyDatumList
  deriving (Eq, Ord, Show, Typeable, Data)

data CompFor = CompFor
  { async :: Bool
  , exprs :: [Expr]
  , inExpr :: Expr
  , iters :: Maybe CompIter
  } deriving (Eq, Ord, Show, Typeable, Data)

data CompIf = CompIf
  { ifExpr :: Expr
  , ifIters :: Maybe CompIter
  } deriving (Eq, Ord, Show, Typeable, Data)

data CompIter
  = IterFor CompFor
  | IterIf CompIf
  deriving (Eq, Ord, Show, Typeable, Data)

data Expr
  = Var Ident
  | Int Integer
  | Float Double
  | Imagary Double
  | Bool Bool
  | None
  | ByteString !T.Text
  | String !T.Text
  | Call { func :: Expr
         , callArgs :: [Argument] }
  | Indexing Expr -- Indexed
             Expr -- Index
  | Cond { trueBranch :: Expr
         , condition :: Expr
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
  | Generator Comprehension
  | Await Expr
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
  | Xor
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
