{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-|
  Translates the abstract grammar from the parser to the internal
representation used by the rest of the code.

  This give us the power to change the parser AST as we want without
the need to change the internal representation of the grammar.
-}
module Language.Coral.Parser.Translate
  ( translate
  )
where

import           Data.Functor                   ( (<&>) )
import           Data.List                      ( foldl' )
import qualified Data.Text                     as T
import qualified Language.Coral.AST            as AST
import qualified Language.Coral.Parser.AbsGrammar
                                               as AG
import           Numeric


{-| Represents the possibility to translate @from@ to @to@.

  [@from@]: Original type.
  [@to@]: Translated type.

The kind of @from@ is @* -> *@ because all the AST Grammars types
should be annotated.

TODO: Convert kind of @to@ to @* -> *@ after we modify "AST" types to be annotated.
-}
class Translatable (from :: * -> *) to where
  {-# MINIMAL translate | partialTranslate #-}
  -- | Hability to translate @from@ to @to@.
  translate :: forall a . from a -> to
  translate = maybe (error "Impossible input translation!") id . partialTranslate

  -- | Partial translation of @from@ to @to@.
  partialTranslate :: forall a. from a -> Maybe to
  partialTranslate = Just . translate


instance Translatable AG.Program AST.Module where
  translate (AG.Prog _ header defs) = AST.Module name exports (map translate defs)
    where (name, exports) = translate header


instance Translatable AG.Header (AST.ModuleName, AST.Exports) where
  translate (AG.Header1 _ mns) = (map translate mns, AST.All)
  translate (AG.Header2 _ mns exps) = (map translate mns, AST.Some $ map translate exps)


instance Translatable AG.ModuleName AST.Ident where
  translate (AG.ModuleName _ name) = translName name


instance Translatable AG.ExportName AST.Ident where
  translate (AG.ExportName _ name) = translName name


instance Translatable AG.Definition AST.Statement where
  translate (AG.DefinitionAsyncDef _ (AG.AsyncDef _ funcDef)) =
    AST.AsyncFun $ translate funcDef
  translate (AG.DefinitionFuncDef _ funcDef) = translate funcDef
  translate (AG.DefinitionTypeDef _ typeDef) = AST.TypeD $ translate typeDef
  translate (AG.DefinitionDataDef _ dataDef) = AST.DataD $ translate dataDef


instance Translatable AG.FuncDef AST.Statement where
  translate (AG.FuncDef _ name params retType suite) = AST.Fun (translName name)
                                                               (map translate params)
                                                               (translate retType)
                                                               (translate suite)

instance Translatable AG.TypeDef AST.TypeDef where
  translate (AG.RecDef _ typeName fields) =
    AST.Record (translate typeName) (map translate fields)
  translate (AG.ADTDef _ typeName cons) =
    AST.ADT (translate typeName) (map translate cons)


instance Translatable AG.Constructor AST.Constructor where
  translate (AG.Term _ con) = AST.Term $ translCon con
  translate (AG.Product _ con fields) =
    let constName = translCon con
    in  if isProduct fields
          then AST.Product constName (map translate fields)
          else AST.ConstRecord constName (map translate fields)
    where
      isProduct = all isField

      isField (AG.CFieldType _ _) = True
      isField _                   = False


instance Translatable AG.CField AST.Field where
  partialTranslate (AG.CFieldField _ field) = partialTranslate field
  partialTranslate _                        = Nothing


instance Translatable AG.CField AST.Type where
  partialTranslate (AG.CFieldType _ type') = partialTranslate type'
  partialTranslate _                       = Nothing


instance Translatable AG.Field AST.Field where
  translate (AG.Field _ name type') = AST.Field (translName name) (translate type')


instance Translatable AG.TypeName AST.Type where
  translate (AG.Name _ con      ) = AST.Type . AST.Local $ translCon con
  translate (AG.GenName _ con ts) = AST.GenType (AST.Local $ translCon con) (map translate ts)


instance Translatable AG.DataDef AST.DataDef where
  translate (AG.DataDef _ typeName dfs) = AST.DataDef (translate typeName)
                                                      (map translate dfs)

instance Translatable AG.DataField AST.DataField where
  translate (AG.DataFieldProp _ (AG.Prop _ name type')) =
    AST.Prop (translName name) (translate type')
  translate (AG.DataFieldFuncDef _ funcDef) = AST.Method $ translate funcDef


instance Translatable AG.Parameter AST.Parameter where
  translate (AG.Parameter1 _ name argtype) =
    AST.Param (translName name) (translate argtype) Nothing
  translate (AG.Parameter2 _ name argtype def) =
    AST.Param (translName name) (translate argtype) (Just (translate def))
  translate (AG.Parameter3 _ name argtype) =
    AST.VarParam (translName name) (translate argtype)


instance Translatable AG.ArgType AST.Type where
  translate (AG.ArgTypeType _ type') = translate type'
  translate (AG.ArgType1    _ type') = AST.ArgsType $ translate type'


instance Translatable AG.Type AST.Type where
  translate (AG.MutType _ type') = AST.MutType $ translate type'
  translate (AG.GenericType _ type' args) =
    AST.GenType (translate type') (map translate args)
  translate (AG.ConcreteType _ type') = AST.Type $ translate type'
  translate (AG.ParamType _ type') = AST.FreeType $ translGen type'
  translate (AG.FunType _ args ret) = AST.FunType (map translate args) (translate ret)


instance Translatable AG.TName AST.Name where
  translate (AG.TLocal _ con) = AST.Local (translCon con)
  translate (AG.TQualified _ modName con) = AST.Qualified (map translate modName)
                                                          (translCon con)


instance Translatable AG.Suite AST.Suite where
  translate (AG.Suite _ stmts) = map translate stmts


instance Translatable AG.Statement AST.Statement where
  translate (AG.Expr      _ expr                   ) = AST.Expression $ translate expr
  translate (AG.SFuncDef  _ funcDef                ) = translate funcDef
  translate (AG.SAFuncDef _ (AG.AsyncDef _ funcDef)) = AST.AsyncFun $ translate funcDef
  translate (AG.Assign _ test expr) = AST.Assign (translate test) (translate expr)
  translate (AG.MutAssign _ test expr) =
    AST.MutAssign (translate test) (translate expr)
  translate (AG.DefVal _ name varType expr) =
    AST.Declaration (translName name) (translate varType) (Just $ translate expr)
  translate (AG.Def _ name varType) =
    AST.Declaration (translName name) (translate varType) Nothing
  translate (AG.MutDefVal _ name varType e) =
    AST.MutDeclaration (translName name) (translate varType) (Just $ translate e)
  translate (AG.MutDef _ name varType) =
    AST.MutDeclaration (translName name) (translate varType) Nothing
  translate (AG.Return _ testOrStar) = AST.Return $ map translate testOrStar
  translate (AG.WhileElse _ be s1 s2) =
    AST.While (translate be) (translate s1) (Just $ translate s2)
  translate (AG.While _ be s1) = AST.While (translate be) (translate s1) Nothing
  translate (AG.ForElse _ ts1 ts2 s1 s2) =
    AST.For (map translate ts1) (map translate ts2) (translate s1) (Just $ translate s2)
  translate (AG.For _ ts1 ts2 s) =
    AST.For (map translate ts1) (map translate ts2) (translate s) Nothing
  translate (AG.IfElse _ be s els se) = AST.Conditional guards . Just $ translate se
    where
      guards = (translate be, translate s) : map translElif els

      translElif :: AG.Elif a -> (AST.Expr, AST.Suite)
      translElif (AG.Elif _ e s') = (translate e, translate s')
  translate (AG.IfElif _ be s els) = AST.Conditional guards Nothing
    where
      guards = ((translate be, translate s) :) $ els <&> \(AG.Elif _ e s') ->
        (translate e, translate s')
  translate (AG.If _ be s) = AST.Conditional guard Nothing
    where guard = [(translate be, translate s)]
  translate (AG.TryEFin _ s excps se sf) = AST.Try
    { AST.body     = translate s
    , AST.excepts  = translate <$> excps
    , AST.handlers = []
    , AST.else'    = Just $ translate se
    , AST.finally  = Just $ translate sf
    }
  translate (AG.TryWEFin _ hs s excps se sf) = AST.Try
    { AST.body     = translate s
    , AST.excepts  = translate <$> excps
    , AST.handlers = translate <$> hs
    , AST.else'    = Just $ translate se
    , AST.finally  = Just $ translate sf
    }
  translate (AG.TryElse _ s excps se) = AST.Try
    { AST.body     = translate s
    , AST.excepts  = translate <$> excps
    , AST.handlers = []
    , AST.else'    = Just $ translate se
    , AST.finally  = Nothing
    }
  translate (AG.TryWElse _ hs s excps se) = AST.Try
    { AST.body     = translate s
    , AST.excepts  = translate <$> excps
    , AST.handlers = translate <$> hs
    , AST.else'    = Just $ translate se
    , AST.finally  = Nothing
    }
  translate (AG.Try _ s excps) = AST.Try
    { AST.body     = translate s
    , AST.excepts  = translate <$> excps
    , AST.handlers = []
    , AST.else'    = Nothing
    , AST.finally  = Nothing
    }
  translate (AG.TryWith _ hs s excps) = AST.Try
    { AST.body     = translate s
    , AST.excepts  = translate <$> excps
    , AST.handlers = translate <$> hs
    , AST.else'    = Nothing
    , AST.finally  = Nothing
    }
  translate (AG.Break    _) = AST.Break
  translate (AG.Continue _) = AST.Continue
  translate (AG.Pass     _) = AST.Pass


instance Translatable AG.TestStarExpr AST.Expr where
  translate (AG.TestStarExprTest     _ test         ) = translate test
  translate (AG.TestStarExprStarExpr _ (AG.Star _ e)) = translate e


instance Translatable AG.Except AST.Handler where
  translate (AG.EExcp _ s) =
    AST.Handler {AST.clause = AST.CatchAll, AST.hanBody = translate s}
  translate (AG.Excp _ ts s) = AST.Handler
    { AST.clause  = AST.Catch $ map translate ts
    , AST.hanBody = translate s
    }


instance Translatable AG.ExceptClause (AST.Type, Maybe AST.Ident) where
  translate (AG.EClauseAs _ tn as) = (translate tn, Just $ translName as)
  translate (AG.EClause _ tn     ) = (translate tn, Nothing)


instance Translatable AG.VarType AST.Type where
  translate (AG.IType _  ) = AST.ImplicitType
  translate (AG.EType _ t) = translate t


instance Translatable AG.BoolExpr AST.Expr where
  translate (AG.BoolExpr _ e) = translate e


instance Translatable AG.Expr AST.Expr where
  translate (AG.TryExpr   _ e    ) = AST.TryE $ translate e
  translate (AG.AwaitExpr _ e    ) = AST.Await $ translate e
  translate (AG.OpExpr _ e1 op e2) = AST.BinaryOp (translate op)
                                                  (translate e1)
                                                  (translate e2)
  translate (AG.NotExpr _ e       ) = AST.UnaryOp AST.Not $ translate e
  translate (AG.BOpExpr _ e1 op e2) = AST.BinaryOp (translate op)
                                                   (translate e1)
                                                   (translate e2)
  translate (AG.COpExpr _ e1 op e2) = AST.BinaryOp (translate op)
                                                   (translate e1)
                                                   (translate e2)
  translate (AG.LTrue  _    ) = AST.Bool True
  translate (AG.LFalse _    ) = AST.Bool False
  translate (AG.TestExpr _ t) = translate t
  translate (AG.CompExpr _ compr) = translate compr
  translate (AG.Literal _ lit) = translate lit


instance Translatable AG.Comprehension AST.Expr where
  translate (AG.ListComp _ term iters) = AST.ListComp $ AST.Comprehension (translate term)
                                                                          (joinCompIter $ map translate iters)
  translate (AG.DictComp _ term iters) = AST.DictComp $ AST.Comprehension (translate term)
                                                                          (joinCompIter $ map translate iters)
  translate (AG.SetComp  _ term iters) = AST.SetComp $ AST.Comprehension (translate term)
                                                                         (joinCompIter $ map translate iters)
  translate (AG.GenComp  _ term iters) = AST.Generator $ AST.Comprehension (translate term)
                                                                           (joinCompIter $ map translate iters)


joinCompIter :: [AST.CompIter] -> AST.CompFor
joinCompIter []  = error "List should not be empty."
joinCompIter cis = case iter of
  AST.IterFor x -> x
  AST.IterIf{}  -> error "Comprehensions should start with a iteration."
 where
  iter = foldr1 go cis
  go (AST.IterFor new) acc = AST.IterFor $ new { AST.iters = Just acc }
  go (AST.IterIf  new) acc = AST.IterIf $ new { AST.ifIters = Just acc }

instance Translatable AG.CompIter AST.CompIter where
  translate (AG.CompFor _ ts e) = AST.IterFor $ AST.CompFor
    { AST.async = False -- ^ For now we don't support async in comprehensions.
    , AST.exprs = map translate ts
    , AST.inExpr = translate e
    , AST.iters = Nothing
    }
  translate (AG.CompIf _ be) = AST.IterIf $ AST.CompIf (translate be) Nothing


instance Translatable AG.ListTerm AST.CompExpr where
  translate (AG.ListTermExpr _ e) = AST.CompExpr . AST.Item $ translate e
  translate (AG.ListTerm1 _ t) = AST.CompExpr . AST.Unpacking $ translate t

instance Translatable AG.DictTerm AST.CompExpr where
  translate = AST.CompDict . translate


instance Translatable AG.Literal AST.Expr where
  translate (AG.ListLit _ terms) = AST.List $ map translate terms
  translate (AG.SetLit _ terms) = AST.Set $ map translate terms
  translate (AG.DictLit _ terms) = AST.Dict $ map translate terms


instance Translatable AG.ListTerm AST.IterItem where
  translate (AG.ListTermExpr _ e) = AST.Item $ translate e
  translate (AG.ListTerm1 _ t) = AST.Unpacking $ translate t


instance Translatable AG.DictTerm AST.DictItem where
  translate (AG.DictTerm1 _ t e) = AST.DictMappingPair (translate t) (translate e)
  translate (AG.DictTerm2 _ t) = AST.DictUnpacking $ translate t


instance Translatable AG.Op AST.Op where
  translate (AG.Mul    _) = AST.Times
  translate (AG.MMul   _) = AST.MatrixTimes
  translate (AG.Div    _) = AST.Divide
  translate (AG.FDiv   _) = AST.FloorDivide
  translate (AG.Mod    _) = AST.Modulo
  translate (AG.Add    _) = AST.Plus
  translate (AG.Minus  _) = AST.Minus
  translate (AG.LShift _) = AST.ShiftLeft
  translate (AG.RShift _) = AST.ShiftRight
  translate (AG.BitAnd _) = AST.BinaryAnd
  translate (AG.BitXor _) = AST.BinaryXor
  translate (AG.BitOr  _) = AST.BinaryOr
  translate (AG.APipe  _) = AST.Arrow
  translate (AG.Pipe   _) = AST.Pipe


instance Translatable AG.BoolOp AST.Op where
  translate (AG.And _) = AST.And
  translate (AG.Or  _) = AST.Or


instance Translatable AG.CompOp AST.Op where
  translate (AG.In    _) = AST.In
  translate (AG.NotIn _) = AST.NotIn
  translate (AG.Is    _) = AST.Is
  translate (AG.Lt    _) = AST.LessThan
  translate (AG.Le    _) = AST.LessThanEquals
  translate (AG.Gt    _) = AST.GreaterThan
  translate (AG.Ge    _) = AST.GreaterThanEquals
  translate (AG.Ne    _) = AST.NotEquals
  translate (AG.Eq    _) = AST.Equals


instance Translatable AG.Test AST.Expr where
  translate (AG.TestName  _ name  )  = AST.Var $ translate name
  translate (AG.TestNUMBER _ num   ) = AST.Num $ translate num
  translate (AG.TestSTRING _ str   ) = AST.String $ translate str
  translate (AG.Test1 _ name trails) = foldl' translTrailer
                                               (AST.Var $ translate name)
                                               trails
    where
      translTrailer :: AST.Expr -> AG.Trailer a -> AST.Expr
      translTrailer e (AG.Trailer1 _ args ) = AST.Call e $ map translate args
      translTrailer e (AG.Trailer2 _ subs ) = AST.Indexing e $ translate subs
      translTrailer e (AG.Trailer3 _ name') = AST.Dot e $ translName name'


instance Translatable AG.Subscript AST.Expr where
  translate (AG.SubscriptTest _ test) = translate test
  translate (AG.Subscript1 _ t1 t2 t3) =
    AST.Slice (partialTranslate t1) (partialTranslate t2) (partialTranslate t3)
  translate (AG.Subscript2 _ t2 t3) =
    AST.Slice Nothing (partialTranslate t2) (partialTranslate t3)
  translate (AG.Subscript3 _ t1 t3) =
    AST.Slice (partialTranslate t1) Nothing (partialTranslate t3)
  translate (AG.Subscript4 _ t1 t2) =
    AST.Slice (partialTranslate t1) (partialTranslate t2) Nothing
  translate (AG.Subscript5 _ t1) = AST.Slice (partialTranslate t1) Nothing Nothing
  translate (AG.Subscript6 _ t2) = AST.Slice Nothing (partialTranslate t2) Nothing
  translate (AG.Subscript7 _ t3) = AST.Slice Nothing Nothing (partialTranslate t3)


instance Translatable AG.Arg AST.Argument where
  translate (AG.ArgExpr _ e      ) = AST.Arg $ translate e
  translate (AG.Arg1 _ name' test) = AST.KeywordArg (translName name') (translate test)
  translate (AG.Arg2 _ test      ) = AST.VarArg $ translate test


instance Translatable AG.NUMBER AST.Number where
  translate (AG.ImagF _ d                 ) = AST.Imaginary d
  translate (AG.ImagI _ i                 ) = AST.Imaginary $ fromInteger i
  translate (AG.Zero _                    ) = AST.Int 0 -- Using Int as we can safely
                                                        -- coerce it to another type latter.
  translate (AG.Int   _ i                 ) = AST.Int i
  translate (AG.Float _ f                 ) = AST.Float f
  translate (AG.Hex   _ (AG.Hexadecimal h)) = AST.Int . fst . head $ readHex h


instance Translatable AG.STRING AST.Str where
  translate (AG.Str     _ str       ) = AST.Str $ T.pack str
  translate (AG.RawStr  _ str       ) = AST.RawStr $ translate str
  translate (AG.FmtStr  _ str       ) = AST.FormatStr $ translate str
  translate (AG.ByteStr _ str       ) = AST.ByteStr $ translate str
  translate (AG.SplitStr _ str1 str2) = AST.SplitStr (translate str1) (translate str2)


instance Translatable AG.Name AST.Name where
  translate (AG.Local _ name) = AST.Local $ translName name
  translate (AG.Qualified _ modName name) = AST.Qualified (map translate modName) (translName name)


-- TODO: Change these to instances of @Translatable@ after add annotations to the types.
translName :: AG.NAME -> AST.Ident
translName (AG.NAME name) = AST.Ident . T.pack $ name


translCon :: AG.Con -> AST.Ident
translCon (AG.Con con) = AST.Ident . T.pack $ con


translGen :: AG.Gen -> AST.Ident
translGen (AG.Gen gen) = AST.Ident . T.pack . tail $ gen
