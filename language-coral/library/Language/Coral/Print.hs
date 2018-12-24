module Language.Coral.Print
    ( prettyPrint
    , format
    )
where


import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text


prettyPrint :: forall a . Pretty a => a -> Text
prettyPrint = renderStrict . format . pretty


format :: forall ann . Doc ann -> SimpleDocStream ann
format = removeTrailingWhitespace . layoutSmart defaultLayoutOptions
