{-
  Translates the abstract grammar from the parser to the internal
representation used by the rest of the code.

  This give us the power to change the parser AST as we want without
the need to change the internal representation of the grammar.

  All the functions follow the pattern `translX`, where `X` defines
the type that that function will translate.
-}
module Language.Coral.Parser.Translate
  ( translate
  )
where

import           Data.Functor                   ( (<&>) )
import           Data.List                      ( foldl'
                                                , partition
                                                )
import qualified Data.Text                     as T
import qualified Language.Coral.AST            as AST
import qualified Language.Coral.Parser.AbsGrammar
                                               as AG
import           Numeric

translate :: forall a . AG.Program a -> AST.Module
translate = translProgram

translProgram :: forall a . AG.Program a -> AST.Module
translProgram (AG.Prog _ defs) = AST.Module (map translDef priv) (map translDef pub)
 where
  isPublic :: AG.Definition a -> Bool
  isPublic (AG.Definition1 _ _) = True
  isPublic (AG.Definition2 _ _) = True
  isPublic (AG.Definition3 _ _) = True
  isPublic (AG.Definition4 _ _) = True
  isPublic _                    = False

  (pub, priv) = partition isPublic defs


translDef :: forall a . AG.Definition a -> AST.Statement
translDef (AG.Definition1 _ (AG.AsyncDef _ funcDef)) =
  AST.AsyncFun $ translFunc funcDef
translDef (AG.Definition2 _ funcDef) = translFunc funcDef
translDef (AG.Definition3 _ typeDef) = AST.TypeD $ translTypeDef typeDef
translDef (AG.Definition4 _ dataDef) = AST.DataD $ translDataDef dataDef
translDef (AG.DefinitionAsyncDef _ (AG.AsyncDef _ funcDef)) =
  AST.AsyncFun $ translFunc funcDef
translDef (AG.DefinitionFuncDef _ funcDef) = translFunc funcDef
translDef (AG.DefinitionTypeDef _ typeDef) = AST.TypeD $ translTypeDef typeDef
translDef (AG.DefinitionDataDef _ dataDef) = AST.DataD $ translDataDef dataDef


translFunc :: forall a . AG.FuncDef a -> AST.Statement
translFunc (AG.FuncDef _ name params retType suite) = AST.Fun
  (translName name)
  (map translParam params)
  (translType retType)
  (translSuite suite)


translTypeDef :: forall a . AG.TypeDef a -> AST.TypeDef
translTypeDef (AG.RecDef _ typeName fields) =
  AST.Record (translTypeName typeName) (map translField fields)
translTypeDef (AG.ADTDef _ typeName cons) = AST.ADT (translTypeName typeName)
                                                    (map translCons cons)
 where
  translCons :: AG.Constructor a -> AST.Constructor
  translCons (AG.Term _ con) = AST.Term $ translCon con
  translCons (AG.Product _ con fields) =
    let constName = translCon con
    in  if isProduct fields
          then AST.Product constName (map translCFieldToType fields)
          else AST.ConstRecord constName (map translCFieldToField fields)

  isProduct :: [AG.CField a] -> Bool
  isProduct = all isField
   where
    isField (AG.CFieldType _ _) = True
    isField _                   = False

  translCFieldToField :: AG.CField a -> AST.Field
  translCFieldToField (AG.CFieldField _ field) = translField field
  translCFieldToField _ = error "Impossible input in translation!"

  translCFieldToType :: AG.CField a -> AST.Type
  translCFieldToType (AG.CFieldType _ type') = translType type'
  translCFieldToType _                       = error "Impossible input in translation!"


translField :: forall a . AG.Field a -> AST.Field
translField (AG.Field _ name type') = AST.Field (translName name) (translType type')


translTypeName :: forall a . AG.TypeName a -> AST.Type
translTypeName (AG.Name _ con      ) = AST.Type [translCon con]
translTypeName (AG.GenName _ con ts) = AST.GenType [translCon con] (map translType ts)


translDataDef :: forall a . AG.DataDef a -> AST.DataDef
translDataDef (AG.DataDef _ typeName dfs) = AST.DataDef (translTypeName typeName)
                                                        (map translDataField dfs)
 where
  translDataField :: AG.DataField a -> AST.DataField
  translDataField (AG.DataFieldProp _ (AG.Prop _ name type')) =
    AST.Prop (translName name) (translType type')
  translDataField (AG.DataFieldFuncDef _ funcDef) = AST.Method $ translFunc funcDef


translParam :: forall a . AG.Parameter a -> AST.Parameter
translParam (AG.Parameter1 _ name argtype) =
  AST.Param (translName name) (translArgType argtype) Nothing
translParam (AG.Parameter2 _ name argtype def) =
  AST.Param (translName name) (translArgType argtype) (Just (translTest def))
translParam (AG.Parameter3 _ name argtype) =
  AST.VarParam (translName name) (translArgType argtype)


translArgType :: forall a . AG.ArgType a -> AST.Type
translArgType (AG.ArgTypeType _ type') = translType type'
translArgType (AG.ArgType1    _ type') = AST.ArgsType $ translType type'


translType :: forall a . AG.Type a -> AST.Type
translType (AG.MutType _ type') = AST.MutType $ translType type'
translType (AG.GenericType _ type' args) =
  AST.GenType [translCon type'] (map translType args)
translType (AG.ConcreteType _ type') = AST.Type [translCon type']
translType (AG.ParamType _ type') = AST.FreeType $ translGen type'
translType (AG.FunType _ args ret) = AST.FunType (map translType args) (translType ret)


translSuite :: forall a . AG.Suite a -> AST.Suite
translSuite (AG.Suite _ stmts) = map translStmt stmts


translStmt :: forall a . AG.Statement a -> AST.Statement
translStmt (AG.Expr      _ expr                   ) = AST.Expression $ translExpr expr
translStmt (AG.SFuncDef  _ funcDef                ) = translFunc funcDef
translStmt (AG.SAFuncDef _ (AG.AsyncDef _ funcDef)) = AST.AsyncFun $ translFunc funcDef
translStmt (AG.Assign _ test expr) = AST.Assign (translTest test) (translExpr expr)
translStmt (AG.MutAssign _ test expr) =
  AST.MutAssign (translTest test) (translExpr expr)
translStmt (AG.DefVal _ name varType expr) =
  AST.Declaration (translName name) (translVarType varType) (Just $ translExpr expr)
translStmt (AG.Def _ name varType) =
  AST.Declaration (translName name) (translVarType varType) Nothing
translStmt (AG.MutDefVal _ name varType e) =
  AST.MutDeclaration (translName name) (translVarType varType) (Just $ translExpr e)
translStmt (AG.MutDef _ name varType) =
  AST.MutDeclaration (translName name) (translVarType varType) Nothing
translStmt (AG.Return _ testOrStar) = AST.Return $ map translTestStar testOrStar
 where
  translTestStar (AG.TestStarExprTest     _ test         ) = translTest test
  translTestStar (AG.TestStarExprStarExpr _ (AG.Star _ e)) = translExpr e
translStmt (AG.WhileElse _ be s1 s2) =
  AST.While (translBoolExpr be) (translSuite s1) (Just $ translSuite s2)
translStmt (AG.While _ be s1) = AST.While (translBoolExpr be) (translSuite s1) Nothing
translStmt (AG.ForElse _ ts1 ts2 s1 s2) = AST.For (map translTest ts1)
                                                  (map translTest ts2)
                                                  (translSuite s1)
                                                  (Just $ translSuite s2)
translStmt (AG.For _ ts1 ts2 s) =
  AST.For (map translTest ts1) (map translTest ts2) (translSuite s) Nothing
translStmt (AG.IfElse _ be s els se) = AST.Conditional guards . Just $ translSuite se
 where
  guards = (translBoolExpr be, translSuite s) : map translElif els

  translElif :: AG.Elif a -> (AST.Expr, AST.Suite)
  translElif (AG.Elif _ e s') = (translBoolExpr e, translSuite s')
translStmt (AG.IfElif _ be s els) = AST.Conditional guards Nothing
 where
  guards = ((translBoolExpr be, translSuite s) :) $ els <&> \(AG.Elif _ e s') ->
    (translBoolExpr e, translSuite s')
translStmt (AG.If _ be s) = AST.Conditional guard Nothing
  where guard = [(translBoolExpr be, translSuite s)]
translStmt (AG.TryEFin _ s excps se sf) = AST.Try
  { AST.body     = translSuite s
  , AST.excepts  = translExcept <$> excps
  , AST.handlers = []
  , AST.else'    = Just $ translSuite se
  , AST.finally  = Just $ translSuite sf
  }
translStmt (AG.TryWEFin _ hs s excps se sf) = AST.Try
  { AST.body     = translSuite s
  , AST.excepts  = translExcept <$> excps
  , AST.handlers = translTest <$> hs
  , AST.else'    = Just $ translSuite se
  , AST.finally  = Just $ translSuite sf
  }
translStmt (AG.TryElse _ s excps se) = AST.Try
  { AST.body     = translSuite s
  , AST.excepts  = translExcept <$> excps
  , AST.handlers = []
  , AST.else'    = Just $ translSuite se
  , AST.finally  = Nothing
  }
translStmt (AG.TryWElse _ hs s excps se) = AST.Try
  { AST.body     = translSuite s
  , AST.excepts  = translExcept <$> excps
  , AST.handlers = translTest <$> hs
  , AST.else'    = Just $ translSuite se
  , AST.finally  = Nothing
  }
translStmt (AG.Try _ s excps) = AST.Try
  { AST.body     = translSuite s
  , AST.excepts  = translExcept <$> excps
  , AST.handlers = []
  , AST.else'    = Nothing
  , AST.finally  = Nothing
  }
translStmt (AG.TryWith _ hs s excps) = AST.Try
  { AST.body     = translSuite s
  , AST.excepts  = translExcept <$> excps
  , AST.handlers = translTest <$> hs
  , AST.else'    = Nothing
  , AST.finally  = Nothing
  }
translStmt (AG.Break    _) = AST.Break
translStmt (AG.Continue _) = AST.Continue
translStmt (AG.Pass     _) = AST.Pass


translExcept :: forall a . AG.Except a -> AST.Handler
translExcept (AG.EExcp _ s) =
  AST.Handler {AST.clause = AST.CatchAll, AST.hanBody = translSuite s}
translExcept (AG.Excp _ ts s) = AST.Handler
  { AST.clause  = AST.Catch $ map translClause ts
  , AST.hanBody = translSuite s
  }
 where
  translClause :: AG.ExceptClause a -> (AST.Type, Maybe AST.Ident)
  translClause (AG.EClauseAs _ tn as) = (translTypeName tn, Just $ translName as)
  translClause (AG.EClause _ tn     ) = (translTypeName tn, Nothing)

translVarType :: forall a . AG.VarType a -> AST.Type
translVarType (AG.IType _  ) = AST.ImplicitType
translVarType (AG.EType _ t) = translType t


translBoolExpr :: forall a . AG.BoolExpr a -> AST.Expr
translBoolExpr (AG.BoolExpr _ e) = translExpr e


translExpr :: forall a . AG.Expr a -> AST.Expr
translExpr (AG.TryExpr   _ e    ) = AST.TryE $ translExpr e
translExpr (AG.AwaitExpr _ e    ) = AST.Await $ translExpr e
translExpr (AG.OpExpr _ e1 op e2) = AST.BinaryOp (translOp op)
                                                 (translExpr e1)
                                                 (translExpr e2)
 where
  translOp :: AG.Op a -> AST.Op
  translOp (AG.Mul    _) = AST.Times
  translOp (AG.MMul   _) = AST.MatrixTimes
  translOp (AG.Div    _) = AST.Divide
  translOp (AG.FDiv   _) = AST.FloorDivide
  translOp (AG.Mod    _) = AST.Modulo
  translOp (AG.Add    _) = AST.Plus
  translOp (AG.Minus  _) = AST.Minus
  translOp (AG.LShift _) = AST.ShiftLeft
  translOp (AG.RShift _) = AST.ShiftRight
  translOp (AG.BitAnd _) = AST.BinaryAnd
  translOp (AG.BitXor _) = AST.BinaryXor
  translOp (AG.BitOr  _) = AST.BinaryOr
  translOp (AG.APipe  _) = AST.Arrow
  translOp (AG.Pipe   _) = AST.Pipe
translExpr (AG.NotExpr _ e       ) = AST.UnaryOp AST.Not $ translExpr e
translExpr (AG.BOpExpr _ e1 op e2) = AST.BinaryOp (translBoolOp op)
                                                  (translExpr e1)
                                                  (translExpr e2)
 where
  translBoolOp :: AG.BoolOp a -> AST.Op
  translBoolOp (AG.And _) = AST.And
  translBoolOp (AG.Or  _) = AST.Or
translExpr (AG.COpExpr _ e1 op e2) = AST.BinaryOp (translCompOp op)
                                                  (translExpr e1)
                                                  (translExpr e2)
 where
  translCompOp :: AG.CompOp a -> AST.Op
  translCompOp (AG.In    _) = AST.In
  translCompOp (AG.NotIn _) = AST.NotIn
  translCompOp (AG.Is    _) = AST.Is
  translCompOp (AG.Lt    _) = AST.LessThan
  translCompOp (AG.Le    _) = AST.LessThanEquals
  translCompOp (AG.Gt    _) = AST.GreaterThan
  translCompOp (AG.Ge    _) = AST.GreaterThanEquals
  translCompOp (AG.Ne    _) = AST.NotEquals
  translCompOp (AG.Eq    _) = AST.Equals
translExpr (AG.LTrue  _    ) = AST.Bool True
translExpr (AG.LFalse _    ) = AST.Bool False
translExpr (AG.TestExpr _ t) = translTest t


translTest :: forall a . AG.Test a -> AST.Expr
translTest (AG.TestNAME   _ name  ) = AST.Var $ translName name
translTest (AG.TestNUMBER _ num   ) = AST.Num $ translNum num
translTest (AG.TestSTRING _ str   ) = AST.String $ translStr str
translTest (AG.Test1 _ name trails) = foldl' translTrailer
                                             (AST.Var $ translName name)
                                             trails
 where
  translTrailer :: AST.Expr -> AG.Trailer a -> AST.Expr
  translTrailer e (AG.Trailer1 _ args ) = AST.Call e $ map translArg args
  translTrailer e (AG.Trailer2 _ subs ) = AST.Indexing e $ translSubs subs
  translTrailer e (AG.Trailer3 _ name') = AST.Dot e $ translName name'

  translSubs :: AG.Subscript a -> AST.Expr
  translSubs (AG.SubscriptTest _ test) = translTest test
  translSubs (AG.Subscript1 _ t1 t2 t3) =
    AST.Slice (translSliceTerm t1) (translSliceTerm t2) (translSliceTerm t3)
  translSubs (AG.Subscript2 _ t2 t3) =
    AST.Slice Nothing (translSliceTerm t2) (translSliceTerm t3)
  translSubs (AG.Subscript3 _ t1 t3) =
    AST.Slice (translSliceTerm t1) Nothing (translSliceTerm t3)
  translSubs (AG.Subscript4 _ t1 t2) =
    AST.Slice (translSliceTerm t1) (translSliceTerm t2) Nothing
  translSubs (AG.Subscript5 _ t1) = AST.Slice (translSliceTerm t1) Nothing Nothing
  translSubs (AG.Subscript6 _ t2) = AST.Slice Nothing (translSliceTerm t2) Nothing
  translSubs (AG.Subscript7 _ t3) = AST.Slice Nothing Nothing (translSliceTerm t3)
  translSliceTerm = Just . translTest

  translArg :: AG.Arg a -> AST.Argument
  translArg (AG.ArgExpr _ e) = AST.Arg $ translExpr e
  translArg (AG.Arg1 _ name' test) =
    AST.KeywordArg (translName name') (translTest test)
  translArg (AG.Arg2 _ test) = AST.VarArg $ translTest test


translName :: AG.NAME -> AST.Ident
translName (AG.NAME name) = AST.Ident . T.pack $ name


translNum :: forall a . AG.NUMBER a -> AST.Number
translNum (AG.ImagF _ d                 ) = AST.Imaginary d
translNum (AG.ImagI _ i                 ) = AST.Imaginary $ fromInteger i
translNum (AG.Zero _                    ) = AST.Int 0 -- Using Int as we can safely coerce it to another type latter.
translNum (AG.Int   _ i                 ) = AST.Int i
translNum (AG.Float _ f                 ) = AST.Float f
translNum (AG.Hex   _ (AG.Hexadecimal h)) = AST.Int . fst . head $ readHex h


translStr :: forall a . AG.STRING a -> AST.Str
translStr (AG.Str     _ str       ) = AST.Str $ T.pack str
translStr (AG.RawStr  _ str       ) = AST.RawStr $ translStr str
translStr (AG.FmtStr  _ str       ) = AST.FormatStr $ translStr str
translStr (AG.ByteStr _ str       ) = AST.ByteStr $ translStr str
translStr (AG.SplitStr _ str1 str2) = AST.SplitStr (translStr str1) (translStr str2)


translCon :: AG.Con -> AST.Ident
translCon (AG.Con con) = AST.Ident . T.pack $ con


translGen :: AG.Gen -> AST.Ident
translGen (AG.Gen gen) = AST.Ident . T.pack . tail $ gen
