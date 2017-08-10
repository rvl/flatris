{-# LANGUAGE CPP, TemplateHaskell, RankNTypes #-}

module MainWidget (mainWidgetFlatris, mainWidgetFlatrisExtern) where

import Reflex.Dom
import Data.Monoid ((<>))
import Data.Text
import Language.Javascript.JSaddle (JSM)

-- In browser and webkit frame there are slightly different ways of
-- importing stylesheets. Handle both with CPP.

#ifdef GHCJS_BROWSER

mainWidgetFlatris :: (forall x. Widget x ()) -> JSM ()
mainWidgetFlatris = mainWidgetFlatrisExtern

#else
import Data.FileEmbed

mainWidgetFlatris :: (forall x. Widget x ()) -> IO ()
mainWidgetFlatris = mainWidgetWithCss (
  $(embedFile "static/css/normalize.css") <>
  $(embedFile "static/css/skeleton.css") <>
  $(embedFile "static/css/flatris.css") )

#endif

-- mainWidgetFlatrisExtern :: (forall x. Widget x ()) -> IO ()
mainWidgetFlatrisExtern = mainWidgetWithHead $ do
  el "title" $ text "Flatris"
  mobile_
  stylesheet_ "css/normalize.css"
  stylesheet_ "css/skeleton.css"
  stylesheet_ "css/flatris.css"

mobile_ :: DomBuilder t m => m ()
mobile_ = elAttr "meta" m $ pure ()
  where
    m = "name"    =: "viewport"
     <> "content" =: "width=device-width, initial-scale=1.0"

stylesheet_ :: DomBuilder t m => Text -> m ()
stylesheet_ l = elAttr "link" ss $ pure ()
  where
    ss = "rel"  =: "stylesheet"
      <> "type" =: "text/css"
      <> "href" =: l
