{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-| Pretty print Coral ASTs.

  We don't enable customizations of this module to
prevent discutions about code style locally. The
approach that we prefer to take is that all the code
written in the language must follow the same rules.
-}
module Language.Coral.Pretty.Common where

import           Control.Monad.State.Strict
import           Data.Text                      ( Text )
import qualified Data.Text.Lazy                as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Language.Coral.AST


-- | Size of each indentation level.
blockLen :: Int
blockLen = 2

indent' :: forall a . Doc a -> Doc a
indent' = indent blockLen


{-# INLINE indPretty #-}
indPretty :: forall a b . Pretty a => a -> Doc b
indPretty = indent' . pretty


{-# INLINE prettySuite #-}
prettySuite :: forall a . Suite -> Doc a
prettySuite = foldr1 (<\>) . map pretty


{-# INLINE sepTopLevel #-}
sepTopLevel :: forall a . Doc a -> Doc a -> Doc a
sepTopLevel = (<>) . (<\>) hardline


{-# INLINE (<\>) #-}
(<\>) :: forall a . Doc a -> Doc a -> Doc a
(<\>) = (<>) . (<>) hardline


instance Pretty Ident where
  pretty = pretty . name


instance Pretty Module where
  pretty mod = pub `sepTopLevel` priv where
    pub = concatWith sepTopLevel $ (kwPub <+>) . pretty <$> public mod
    priv = concatWith sepTopLevel $ pretty <$> private mod


instance Pretty TypeDef where
  pretty (ADT name consts) = kwType <+> pretty name <> colon
                             <\> pConsts where
    pConsts = indPretty consts
  pretty (Record name flds) = kwType <+> pretty name <+> colon
                              <\> pFlds where
    pFlds = indPretty flds


instance Pretty Constructor where
  pretty (ConstRecord name flds) =
    pretty name <> parens (fillSep . punctuate comma $ map pretty flds)
  pretty (Product name tps) =
    pretty name <> parens (fillSep . punctuate comma $ map pretty tps)
  pretty (Term name) = pretty name


instance Pretty Field where
  pretty (Field n t) = pretty n <> colon <+> pretty t


instance Pretty DataDef where
  pretty (DataDef n flds) = kwData <+> pretty n <> colon
                            <\> pFlds where
    pFlds = indPretty flds


instance Pretty DataField where
  pretty (Method f) = pretty f
  pretty (Prop n t) = kwProp <+> pretty n <> colon <+> pretty t


instance Pretty Statement where
  pretty (TypeD td) = pretty td
  pretty (DataD dd) = pretty dd
  pretty w@(While{}) =
    case elseSuite of
      Nothing -> header <\> suite
      Just s -> header <\>
                  suite <\>
                kwElse <\>
                  s
    where
      header = kwWhile <+> pretty (cond w) <> colon
      suite = prettySuite $ body w
      elseSuite = prettySuite <$> else' w
  pretty f@(For{}) =
    case elseSuite of
      Nothing -> header <\> suite
      Just s -> header <\>
                  suite <\>
                kwElse <\>
                  s
    where
      header = kwFor <+> pretty (targets f)
                 <+> kwIn <+> pretty (generator f)
      suite = prettySuite $ body f
      elseSuite = prettySuite <$> else' f


-- Keywords
keyword :: forall a . Text -> Doc a
keyword = pretty

kwPub :: forall a . Doc a
kwPub = keyword "pub"

kwType :: forall a . Doc a
kwType = keyword "type"

kwData :: forall a . Doc a
kwData = keyword "data"

kwProp :: forall a . Doc a
kwProp = keyword "prop"

kwWhile :: forall a . Doc a
kwWhile = keyword "while"

kwFor :: forall a . Doc a
kwFor = keyword "for"

kwIn :: forall a . Doc a
kwIn = keyword "in"

kwElse :: forall a . Doc a
kwElse = keyword "else"
