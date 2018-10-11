module Language.Coral.Syntax.Parser.Translate where

import           Data.List                               ( foldl', partition )
import qualified Data.Text                               as T
import qualified Language.Coral.Syntax.AST               as AST
import qualified Language.Coral.Syntax.Parser.AbsGrammar as AG
import           Numeric


translateProgram ∷ AG.Program a → AST.Module
translateProgram (AG.Prog _ defs) = AST.Module (map translateDef priv)
                                               (map translateDef pub)
    where
        isPublic ∷ AG.Definition a → Bool
        isPublic (AG.Definition1 _ _) = True
        isPublic (AG.Definition2 _ _) = True
        isPublic (AG.Definition3 _ _) = True
        isPublic (AG.Definition4 _ _) = True
        isPublic _                    = False

        (pub, priv) = partition isPublic defs

translateDef ∷ AG.Definition a → AST.Statement
translateDef (AG.Definition1 _ (AG.AsyncDef _ funcDef)) = AST.AsyncFun $ translateFunc funcDef
translateDef (AG.Definition2 _ funcDef) = translateFunc funcDef
translateDef (AG.Definition3 _ typeDef) = AST.TypeD $ translateTypeDef typeDef
translateDef (AG.Definition4 _ dataDef) = AST.DataD $ translateDataDef dataDef
translateDef (AG.DefinitionAsyncDef _ (AG.AsyncDef _ funcDef)) = AST.AsyncFun $ translateFunc funcDef
translateDef (AG.DefinitionFuncDef _ funcDef) = translateFunc funcDef
translateDef (AG.DefinitionTypeDef _ typeDef) = AST.TypeD $ translateTypeDef typeDef
translateDef (AG.DefinitionDataDef _ dataDef) = AST.DataD $ translateDataDef dataDef

translateFunc ∷ AG.FuncDef a → AST.Statement
translateFunc (AG.FuncDef _ name params retType suite) =
    AST.Fun (translateName name)
            (map translateParam params)
            (translateType retType)
            (translateSuite suite)

translateTypeDef ∷ AG.TypeDef a → AST.TypeDef
translateTypeDef (AG.ADTDef _ typeName cons) = AST.ADT (translateTypeName typeName)
                                                       (map translateCons cons)
  where
    translateCons ∷ AG.Constructor a → AST.Constructor
    translateCons (AG.Term _ (AG.Con s)) = AST.Term . AST.Ident $ T.pack s
    translateCons (AG.Product _  (AG.Con s) fields) =
      let constName = AST.Ident $ T.pack s
      in if isProduct fields
                then AST.Product constName (map translateCFieldToType fields)
                else AST.ConstRecord constName (map translateCFieldToField fields)

    isProduct ∷ [AG.CField a] → Bool
    isProduct = all isFieldType
      where
        isFieldType (AG.CFieldType _ _) = True
        isFieldType _                   = False

    translateCFieldToField ∷ AG.CField a → AST.Field
    translateCFieldToField (AG.CFieldField _ field) = translateField field
    translateCFieldToField _ = error "Impossible input in translation!"

    translateCFieldToType ∷ AG.CField a → AST.Type
    translateCFieldToType (AG.CFieldType _ type') = translateType type'
    translateCFieldToType _ = error "Impossible input in translation!"
translateTypeDef (AG.RecDef _ typeName fields) = AST.Record (translateTypeName typeName)
                                                            (map translateField fields)

translateField ∷ AG.Field a → AST.Field
translateField = undefined

translateTypeName ∷ AG.TypeName a → AST.Type
translateTypeName = undefined

translateDataDef ∷ AG.DataDef a → AST.DataDef
translateDataDef = undefined

translateParam ∷ AG.Parameter a → AST.Parameter
translateParam (AG.Parameter1 _ name argtype) = AST.Param (translateName name)
                                                          (translateArgType argtype)
                                                          Nothing
translateParam (AG.Parameter2 _ name argtype def) = AST.Param (translateName name)
                                                              (translateArgType argtype)
                                                              (Just (translateTest def))
translateParam (AG.Parameter3 _ name argtype) = AST.VarParam (translateName name)
                                                             (translateArgType argtype)

translateArgType ∷ AG.ArgType a → AST.Type
translateArgType (AG.ArgTypeType _ type') = translateType type'
translateArgType (AG.ArgType1 _ type')    = AST.ArgsType $ translateType type'

translateType ∷ AG.Type a → AST.Type
translateType (AG.MutType _ type') = AST.MutType $ translateType type'
translateType (AG.GenericType _  type' args) = AST.GenType [translateCon type']
                                                           (map translateType args)
translateType (AG.ConcreteType _ type') = AST.Type [translateCon type']
translateType (AG.ParamType _ type') = AST.FreeType $ translateGen type'
translateType (AG.FunType _ args ret) = AST.FunType (map translateType args)
                                                    (translateType ret)

translateSuite ∷ AG.Suite a → AST.Suite
translateSuite (AG.Suite _ stmts) = map translateStmt stmts

translateStmt ∷ AG.Statement a → AST.Statement
translateStmt = undefined

translateExpr ∷ AG.Expr a → AST.Expr
translateExpr = undefined

translateTest ∷ AG.Test a → AST.Expr
translateTest (AG.TestNAME _ name) = AST.Var $ translateName name
translateTest (AG.TestNUMBER _ num) = AST.Num $ translateNum num
translateTest (AG.TestSTRING _ str) = AST.String $ translateStr str
translateTest (AG.Test1 _ name trails) = foldl' translateTrailer
                                                (AST.Var $ translateName name)
                                                trails
    where
        translateTrailer ∷ AST.Expr → AG.Trailer a → AST.Expr
        translateTrailer e (AG.Trailer1 _ args) = AST.Call e $ map translateArgs args
        translateTrailer e (AG.Trailer2 _ subs) = AST.Indexing e $ translateSubs subs
        translateTrailer e (AG.Trailer3 _ name') = AST.Dot e $ translateName name'

        translateSubs ∷ AG.Subscript a → AST.Expr
        translateSubs (AG.SubscriptTest _ test) = translateTest test
        translateSubs (AG.Subscript1 _ t1 t2 t3) = AST.Slice (translateSliceTerm t1)
                                                             (translateSliceTerm t2)
                                                             (translateSliceTerm t3)
        translateSubs (AG.Subscript2 _ t2 t3) = AST.Slice Nothing
                                                          (translateSliceTerm t2)
                                                          (translateSliceTerm t3)
        translateSubs (AG.Subscript3 _ t1 t3) = AST.Slice (translateSliceTerm t1)
                                                          Nothing
                                                          (translateSliceTerm t3)
        translateSubs (AG.Subscript4 _ t1 t2) = AST.Slice (translateSliceTerm t1)
                                                          (translateSliceTerm t2)
                                                          Nothing
        translateSubs (AG.Subscript5 _ t1) = AST.Slice (translateSliceTerm t1)
                                                       Nothing
                                                       Nothing
        translateSubs (AG.Subscript6 _ t2) = AST.Slice Nothing
                                                       (translateSliceTerm t2)
                                                       Nothing
        translateSubs (AG.Subscript7 _ t3) = AST.Slice Nothing
                                                       Nothing
                                                       (translateSliceTerm t3)

        translateSliceTerm ∷ AG.Test a → Maybe AST.Expr
        translateSliceTerm = Just . translateTest

        translateArgs ∷ AG.Arg a → AST.Argument
        translateArgs (AG.ArgExpr _ e) = AST.Arg $ translateExpr e
        translateArgs (AG.Arg1 _ name' test) = AST.KeywordArg (translateName name')
                                                              (translateTest test)
        translateArgs (AG.Arg2 _ test) = AST.VarArg $ translateTest test

translateName ∷ AG.NAME → AST.Ident
translateName (AG.NAME name) = AST.Ident . T.pack $ name

translateNum ∷ AG.NUMBER a → AST.Number
translateNum (AG.ImagF _ d)                = AST.Imaginary d
translateNum (AG.ImagI _ i)                = AST.Imaginary $ fromInteger i
translateNum (AG.Zero _)                   = AST.Int 0
translateNum (AG.Int _ i)                  = AST.Int i
translateNum (AG.Float _ f)                = AST.Float f
translateNum (AG.Hex _ (AG.Hexadecimal h)) = AST.Int . fst . head $ readHex h

translateStr ∷ AG.STRING a → AST.Str
translateStr (AG.Str _ str) = AST.Str $ T.pack str
translateStr (AG.RawStr _ str) = AST.RawStr $ translateStr str
translateStr (AG.FmtStr _ str) = AST.FormatStr $ translateStr str
translateStr (AG.ByteStr _ str) = AST.ByteStr $ translateStr str
translateStr (AG.SplitStr _ str1 str2) = AST.SplitStr (translateStr str1) (translateStr str2)

translateCon ∷ AG.Con → AST.Ident
translateCon (AG.Con con) = AST.Ident . T.pack $ con

translateGen ∷ AG.Gen → AST.Ident
translateGen (AG.Gen gen) = AST.Ident . T.pack . tail $ gen
