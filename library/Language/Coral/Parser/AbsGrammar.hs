{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.Coral.Syntax.Parser.AbsGrammar where

-- Haskell module generated by the BNF converter


import Data.Data (Data,Typeable)
import GHC.Generics (Generic)
newtype Con = Con String
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
newtype Gen = Gen String
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
newtype NAME = NAME String
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
newtype Hexadecimal = Hexadecimal String
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
newtype Octal = Octal String
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
data Program a = Prog a [Definition a]
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor Program where
    fmap f x = case x of
        Prog a definitions -> Prog (f a) (map (fmap f) definitions)
data NUMBER a
    = ImagF a Double
    | ImagI a Integer
    | Zero a
    | Int a Integer
    | Hex a Hexadecimal
    | Float a Double
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor NUMBER where
    fmap f x = case x of
        ImagF a double -> ImagF (f a) double
        ImagI a integer -> ImagI (f a) integer
        Zero a -> Zero (f a)
        Int a integer -> Int (f a) integer
        Hex a hexadecimal -> Hex (f a) hexadecimal
        Float a double -> Float (f a) double
data STRING a
    = Str a String
    | RawStr a (STRING a)
    | FmtStr a (STRING a)
    | ByteStr a (STRING a)
    | SplitStr a (STRING a) (STRING a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor STRING where
    fmap f x = case x of
        Str a string -> Str (f a) string
        RawStr a string -> RawStr (f a) (fmap f string)
        FmtStr a string -> FmtStr (f a) (fmap f string)
        ByteStr a string -> ByteStr (f a) (fmap f string)
        SplitStr a string1 string2 -> SplitStr (f a) (fmap f string1) (fmap f string2)
data Definition a
    = Definition1 a (AsyncDef a)
    | Definition2 a (FuncDef a)
    | Definition3 a (TypeDef a)
    | Definition4 a (DataDef a)
    | DefinitionAsyncDef a (AsyncDef a)
    | DefinitionFuncDef a (FuncDef a)
    | DefinitionTypeDef a (TypeDef a)
    | DefinitionDataDef a (DataDef a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor Definition where
    fmap f x = case x of
        Definition1 a asyncdef -> Definition1 (f a) (fmap f asyncdef)
        Definition2 a funcdef -> Definition2 (f a) (fmap f funcdef)
        Definition3 a typedef -> Definition3 (f a) (fmap f typedef)
        Definition4 a datadef -> Definition4 (f a) (fmap f datadef)
        DefinitionAsyncDef a asyncdef -> DefinitionAsyncDef (f a) (fmap f asyncdef)
        DefinitionFuncDef a funcdef -> DefinitionFuncDef (f a) (fmap f funcdef)
        DefinitionTypeDef a typedef -> DefinitionTypeDef (f a) (fmap f typedef)
        DefinitionDataDef a datadef -> DefinitionDataDef (f a) (fmap f datadef)
data Suite a = Suite a [Statement a]
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor Suite where
    fmap f x = case x of
        Suite a statements -> Suite (f a) (map (fmap f) statements)
data AsyncDef a = AsyncDef a (FuncDef a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor AsyncDef where
    fmap f x = case x of
        AsyncDef a funcdef -> AsyncDef (f a) (fmap f funcdef)
data FuncDef a = FuncDef a NAME [Parameter a] (Type a) (Suite a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor FuncDef where
    fmap f x = case x of
        FuncDef a name parameters type_ suite -> FuncDef (f a) name (map (fmap f) parameters) (fmap f type_) (fmap f suite)
data IndentLine a
    = IndentLine1 a NAME [Parameter a] (Type a)
    | IndentLine2 a (BoolExpr a)
    | IndentLine3 a (BoolExpr a)
    | IndentLine4 a
    | IndentLine5 a (BoolExpr a)
    | IndentLine6 a [Test a] [Test a]
    | IndentLine7 a
    | IndentLine8 a
    | IndentLine9 a
    | IndentLine10 a [Test a]
    | IndentLine11 a [ExceptClause a]
    | IndentLine12 a (TypeName a)
    | IndentLine13 a (TypeName a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor IndentLine where
    fmap f x = case x of
        IndentLine1 a name parameters type_ -> IndentLine1 (f a) name (map (fmap f) parameters) (fmap f type_)
        IndentLine2 a boolexpr -> IndentLine2 (f a) (fmap f boolexpr)
        IndentLine3 a boolexpr -> IndentLine3 (f a) (fmap f boolexpr)
        IndentLine4 a -> IndentLine4 (f a)
        IndentLine5 a boolexpr -> IndentLine5 (f a) (fmap f boolexpr)
        IndentLine6 a tests1 tests2 -> IndentLine6 (f a) (map (fmap f) tests1) (map (fmap f) tests2)
        IndentLine7 a -> IndentLine7 (f a)
        IndentLine8 a -> IndentLine8 (f a)
        IndentLine9 a -> IndentLine9 (f a)
        IndentLine10 a tests -> IndentLine10 (f a) (map (fmap f) tests)
        IndentLine11 a exceptclauses -> IndentLine11 (f a) (map (fmap f) exceptclauses)
        IndentLine12 a typename -> IndentLine12 (f a) (fmap f typename)
        IndentLine13 a typename -> IndentLine13 (f a) (fmap f typename)
data Type a
    = MutType a (Type a)
    | GenericType a Con [Type a]
    | ConcreteType a Con
    | ParamType a Gen
    | FunType a [Type a] (Type a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor Type where
    fmap f x = case x of
        MutType a type_ -> MutType (f a) (fmap f type_)
        GenericType a con types -> GenericType (f a) con (map (fmap f) types)
        ConcreteType a con -> ConcreteType (f a) con
        ParamType a gen -> ParamType (f a) gen
        FunType a types type_ -> FunType (f a) (map (fmap f) types) (fmap f type_)
data ArgType a = ArgTypeType a (Type a) | ArgType1 a (Type a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor ArgType where
    fmap f x = case x of
        ArgTypeType a type_ -> ArgTypeType (f a) (fmap f type_)
        ArgType1 a type_ -> ArgType1 (f a) (fmap f type_)
data VarType a = IType a | EType a (Type a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor VarType where
    fmap f x = case x of
        IType a -> IType (f a)
        EType a type_ -> EType (f a) (fmap f type_)
data Parameter a
    = Parameter1 a NAME (ArgType a)
    | Parameter2 a NAME (ArgType a) (Test a)
    | Parameter3 a NAME (ArgType a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor Parameter where
    fmap f x = case x of
        Parameter1 a name argtype -> Parameter1 (f a) name (fmap f argtype)
        Parameter2 a name argtype test -> Parameter2 (f a) name (fmap f argtype) (fmap f test)
        Parameter3 a name argtype -> Parameter3 (f a) name (fmap f argtype)
data TypeDef a
    = ADTDef a (TypeName a) [Constructor a]
    | RecDef a (TypeName a) [Field a]
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor TypeDef where
    fmap f x = case x of
        ADTDef a typename constructors -> ADTDef (f a) (fmap f typename) (map (fmap f) constructors)
        RecDef a typename fields -> RecDef (f a) (fmap f typename) (map (fmap f) fields)
data TypeName a = GenName a Con [Type a] | Name a Con
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor TypeName where
    fmap f x = case x of
        GenName a con types -> GenName (f a) con (map (fmap f) types)
        Name a con -> Name (f a) con
data Constructor a = Product a Con [CField a] | Term a Con
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor Constructor where
    fmap f x = case x of
        Product a con cfields -> Product (f a) con (map (fmap f) cfields)
        Term a con -> Term (f a) con
data Field a = Field a NAME (Type a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor Field where
    fmap f x = case x of
        Field a name type_ -> Field (f a) name (fmap f type_)
data CField a = CFieldType a (Type a) | CFieldField a (Field a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor CField where
    fmap f x = case x of
        CFieldType a type_ -> CFieldType (f a) (fmap f type_)
        CFieldField a field -> CFieldField (f a) (fmap f field)
data DataDef a = DataDef a (TypeName a) [DataField a]
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor DataDef where
    fmap f x = case x of
        DataDef a typename datafields -> DataDef (f a) (fmap f typename) (map (fmap f) datafields)
data DataField a
    = DataFieldProp a (Prop a) | DataFieldFuncDef a (FuncDef a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor DataField where
    fmap f x = case x of
        DataFieldProp a prop -> DataFieldProp (f a) (fmap f prop)
        DataFieldFuncDef a funcdef -> DataFieldFuncDef (f a) (fmap f funcdef)
data Prop a = Prop a NAME (Type a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor Prop where
    fmap f x = case x of
        Prop a name type_ -> Prop (f a) name (fmap f type_)
data Statement a
    = Expr a (Expr a)
    | SFuncDef a (FuncDef a)
    | SAFuncDef a (AsyncDef a)
    | Assign a (Test a) (Expr a)
    | MutAssign a (Test a) (Expr a)
    | DefVal a NAME (VarType a) (Expr a)
    | Def a NAME (VarType a)
    | MutDefVal a NAME (VarType a) (Expr a)
    | MutDef a NAME (VarType a)
    | Return a [TestStarExpr a]
    | WhileElse a (BoolExpr a) (Suite a) (Suite a)
    | While a (BoolExpr a) (Suite a)
    | ForElse a [Test a] [Test a] (Suite a) (Suite a)
    | For a [Test a] [Test a] (Suite a)
    | IfElse a (BoolExpr a) (Suite a) [Elif a] (Suite a)
    | IfElif a (BoolExpr a) (Suite a) [Elif a]
    | If a (BoolExpr a) (Suite a)
    | TryEFin a (Suite a) [Except a] (Suite a) (Suite a)
    | TryWEFin a [Test a] (Suite a) [Except a] (Suite a) (Suite a)
    | TryElse a (Suite a) [Except a] (Suite a)
    | TryWElse a [Test a] (Suite a) [Except a] (Suite a)
    | Try a (Suite a) [Except a]
    | TryWith a [Test a] (Suite a) [Except a]
    | Break a
    | Continue a
    | Pass a
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor Statement where
    fmap f x = case x of
        Expr a expr -> Expr (f a) (fmap f expr)
        SFuncDef a funcdef -> SFuncDef (f a) (fmap f funcdef)
        SAFuncDef a asyncdef -> SAFuncDef (f a) (fmap f asyncdef)
        Assign a test expr -> Assign (f a) (fmap f test) (fmap f expr)
        MutAssign a test expr -> MutAssign (f a) (fmap f test) (fmap f expr)
        DefVal a name vartype expr -> DefVal (f a) name (fmap f vartype) (fmap f expr)
        Def a name vartype -> Def (f a) name (fmap f vartype)
        MutDefVal a name vartype expr -> MutDefVal (f a) name (fmap f vartype) (fmap f expr)
        MutDef a name vartype -> MutDef (f a) name (fmap f vartype)
        Return a teststarexprs -> Return (f a) (map (fmap f) teststarexprs)
        WhileElse a boolexpr suite1 suite2 -> WhileElse (f a) (fmap f boolexpr) (fmap f suite1) (fmap f suite2)
        While a boolexpr suite -> While (f a) (fmap f boolexpr) (fmap f suite)
        ForElse a tests1 tests2 suite1 suite2 -> ForElse (f a) (map (fmap f) tests1) (map (fmap f) tests2) (fmap f suite1) (fmap f suite2)
        For a tests1 tests2 suite -> For (f a) (map (fmap f) tests1) (map (fmap f) tests2) (fmap f suite)
        IfElse a boolexpr suite1 elifs suite2 -> IfElse (f a) (fmap f boolexpr) (fmap f suite1) (map (fmap f) elifs) (fmap f suite2)
        IfElif a boolexpr suite elifs -> IfElif (f a) (fmap f boolexpr) (fmap f suite) (map (fmap f) elifs)
        If a boolexpr suite -> If (f a) (fmap f boolexpr) (fmap f suite)
        TryEFin a suite1 excepts suite2 suite3 -> TryEFin (f a) (fmap f suite1) (map (fmap f) excepts) (fmap f suite2) (fmap f suite3)
        TryWEFin a tests suite1 excepts suite2 suite3 -> TryWEFin (f a) (map (fmap f) tests) (fmap f suite1) (map (fmap f) excepts) (fmap f suite2) (fmap f suite3)
        TryElse a suite1 excepts suite2 -> TryElse (f a) (fmap f suite1) (map (fmap f) excepts) (fmap f suite2)
        TryWElse a tests suite1 excepts suite2 -> TryWElse (f a) (map (fmap f) tests) (fmap f suite1) (map (fmap f) excepts) (fmap f suite2)
        Try a suite excepts -> Try (f a) (fmap f suite) (map (fmap f) excepts)
        TryWith a tests suite excepts -> TryWith (f a) (map (fmap f) tests) (fmap f suite) (map (fmap f) excepts)
        Break a -> Break (f a)
        Continue a -> Continue (f a)
        Pass a -> Pass (f a)
data Elif a = Elif a (BoolExpr a) (Suite a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor Elif where
    fmap f x = case x of
        Elif a boolexpr suite -> Elif (f a) (fmap f boolexpr) (fmap f suite)
data Except a
    = EExcp a (Suite a) | Excp a [ExceptClause a] (Suite a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor Except where
    fmap f x = case x of
        EExcp a suite -> EExcp (f a) (fmap f suite)
        Excp a exceptclauses suite -> Excp (f a) (map (fmap f) exceptclauses) (fmap f suite)
data ExceptClause a
    = EClauseAs a (TypeName a) NAME | EClause a (TypeName a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor ExceptClause where
    fmap f x = case x of
        EClauseAs a typename name -> EClauseAs (f a) (fmap f typename) name
        EClause a typename -> EClause (f a) (fmap f typename)
data BoolExpr a = BoolExpr a (Expr a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor BoolExpr where
    fmap f x = case x of
        BoolExpr a expr -> BoolExpr (f a) (fmap f expr)
data BoolOp a = And a | Or a
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor BoolOp where
    fmap f x = case x of
        And a -> And (f a)
        Or a -> Or (f a)
data CompOp a
    = In a | NotIn a | Is a | Lt a | Le a | Gt a | Ge a | Ne a | Eq a
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor CompOp where
    fmap f x = case x of
        In a -> In (f a)
        NotIn a -> NotIn (f a)
        Is a -> Is (f a)
        Lt a -> Lt (f a)
        Le a -> Le (f a)
        Gt a -> Gt (f a)
        Ge a -> Ge (f a)
        Ne a -> Ne (f a)
        Eq a -> Eq (f a)
data StarExpr a = Star a (Expr a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor StarExpr where
    fmap f x = case x of
        Star a expr -> Star (f a) (fmap f expr)
data Expr a
    = TryExpr a (Expr a)
    | AwaitExpr a (Expr a)
    | OpExpr a (Expr a) (Op a) (Expr a)
    | NotExpr a (Expr a)
    | BOpExpr a (Expr a) (BoolOp a) (Expr a)
    | COpExpr a (Expr a) (CompOp a) (Expr a)
    | LTrue a
    | LFalse a
    | TestExpr a (Test a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor Expr where
    fmap f x = case x of
        TryExpr a expr -> TryExpr (f a) (fmap f expr)
        AwaitExpr a expr -> AwaitExpr (f a) (fmap f expr)
        OpExpr a expr1 op expr2 -> OpExpr (f a) (fmap f expr1) (fmap f op) (fmap f expr2)
        NotExpr a expr -> NotExpr (f a) (fmap f expr)
        BOpExpr a expr1 boolop expr2 -> BOpExpr (f a) (fmap f expr1) (fmap f boolop) (fmap f expr2)
        COpExpr a expr1 compop expr2 -> COpExpr (f a) (fmap f expr1) (fmap f compop) (fmap f expr2)
        LTrue a -> LTrue (f a)
        LFalse a -> LFalse (f a)
        TestExpr a test -> TestExpr (f a) (fmap f test)
data Op a
    = Mul a
    | MMul a
    | Div a
    | FDiv a
    | Mod a
    | Add a
    | Minus a
    | LShift a
    | RShift a
    | BitAnd a
    | BitXor a
    | BitOr a
    | APipe a
    | Pipe a
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor Op where
    fmap f x = case x of
        Mul a -> Mul (f a)
        MMul a -> MMul (f a)
        Div a -> Div (f a)
        FDiv a -> FDiv (f a)
        Mod a -> Mod (f a)
        Add a -> Add (f a)
        Minus a -> Minus (f a)
        LShift a -> LShift (f a)
        RShift a -> RShift (f a)
        BitAnd a -> BitAnd (f a)
        BitXor a -> BitXor (f a)
        BitOr a -> BitOr (f a)
        APipe a -> APipe (f a)
        Pipe a -> Pipe (f a)
data Test a
    = Test1 a NAME [Trailer a]
    | TestNAME a NAME
    | TestNUMBER a (NUMBER a)
    | TestSTRING a (STRING a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor Test where
    fmap f x = case x of
        Test1 a name trailers -> Test1 (f a) name (map (fmap f) trailers)
        TestNAME a name -> TestNAME (f a) name
        TestNUMBER a number -> TestNUMBER (f a) (fmap f number)
        TestSTRING a string -> TestSTRING (f a) (fmap f string)
data Trailer a
    = Trailer1 a [Arg a] | Trailer2 a (Subscript a) | Trailer3 a NAME
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor Trailer where
    fmap f x = case x of
        Trailer1 a args -> Trailer1 (f a) (map (fmap f) args)
        Trailer2 a subscript -> Trailer2 (f a) (fmap f subscript)
        Trailer3 a name -> Trailer3 (f a) name
data Subscript a
    = SubscriptTest a (Test a)
    | Subscript1 a (Test a) (Test a) (Test a)
    | Subscript2 a (Test a) (Test a)
    | Subscript3 a (Test a) (Test a)
    | Subscript4 a (Test a) (Test a)
    | Subscript5 a (Test a)
    | Subscript6 a (Test a)
    | Subscript7 a (Test a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor Subscript where
    fmap f x = case x of
        SubscriptTest a test -> SubscriptTest (f a) (fmap f test)
        Subscript1 a test1 test2 test3 -> Subscript1 (f a) (fmap f test1) (fmap f test2) (fmap f test3)
        Subscript2 a test1 test2 -> Subscript2 (f a) (fmap f test1) (fmap f test2)
        Subscript3 a test1 test2 -> Subscript3 (f a) (fmap f test1) (fmap f test2)
        Subscript4 a test1 test2 -> Subscript4 (f a) (fmap f test1) (fmap f test2)
        Subscript5 a test -> Subscript5 (f a) (fmap f test)
        Subscript6 a test -> Subscript6 (f a) (fmap f test)
        Subscript7 a test -> Subscript7 (f a) (fmap f test)
data Arg a
    = ArgExpr a (Expr a) | Arg1 a NAME (Test a) | Arg2 a (Test a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor Arg where
    fmap f x = case x of
        ArgExpr a expr -> ArgExpr (f a) (fmap f expr)
        Arg1 a name test -> Arg1 (f a) name (fmap f test)
        Arg2 a test -> Arg2 (f a) (fmap f test)
data TestStarExpr a
    = TestStarExprTest a (Test a) | TestStarExprStarExpr a (StarExpr a)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Functor TestStarExpr where
    fmap f x = case x of
        TestStarExprTest a test -> TestStarExprTest (f a) (fmap f test)
        TestStarExprStarExpr a starexpr -> TestStarExprStarExpr (f a) (fmap f starexpr)
