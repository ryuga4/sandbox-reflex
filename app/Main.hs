{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
module Main where

import           ClassyPrelude                    hiding ((<>))
import           Data.FileEmbed
import qualified Data.Map                         as Map
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import           Language.Javascript.JSaddle.Warp (run)
import           Reflex
import           Reflex.Dom
import           Lib
import Data.Profunctor


main :: IO ()
main = run 8081 $ mainWidgetWithHead headElement bodyElement


headElement :: MonadWidget t m => m ()
headElement = do
  el "title" $ text "Calculator"
  elAttr "link" (styleMap "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css" <> "crossorigin" =: "annonymous") blank
  where
    styleMap link =  Map.fromList
                      [ ("rel", "stylesheet")
                      , ("type", "text/css")
                      , ("href", link)
                      ]

bodyElement :: MonadWidget t m => m ()
bodyElement = elAttr "div" gridContainer $ do
      rec
         elAttr "div" gridItemInput $ dynText $ do
             i <- sInput <$> ev
             if i == T.empty
               then either id (T.pack . show) <$> sValue <$> ev
               else return i
         bDel <- bootstrapButton "Del" $ gridItem <> "class" =: "btn btn-secondary"
         bClear <- bootstrapButton "clear" $ gridItemClear <> "class" =: "btn btn-warning"
         b1 <- bootstrapButton "1" $ gridItem <> "class" =: "btn btn-secondary"
         b2 <- bootstrapButton "2" $ gridItem <> "class" =: "btn btn-secondary"
         b3 <- bootstrapButton "3" $ gridItem <> "class" =: "btn btn-secondary"
         bAdd <- bootstrapButton "+" $ gridItem <> "class" =: "btn btn-secondary"
         b4 <- bootstrapButton "4" $ gridItem <> "class" =: "btn btn-secondary"
         b5 <- bootstrapButton "5" $ gridItem <> "class" =: "btn btn-secondary"
         b6 <- bootstrapButton "6" $ gridItem <> "class" =: "btn btn-secondary"
         bSub <- bootstrapButton "-" $ gridItem <> "class" =: "btn btn-secondary"
         b7 <- bootstrapButton "7" $ gridItem <> "class" =: "btn btn-secondary"
         b8 <- bootstrapButton "8" $ gridItem <> "class" =: "btn btn-secondary"
         b9 <- bootstrapButton "9" $ gridItem <> "class" =: "btn btn-secondary"
         bMul <- bootstrapButton "*" $ gridItem <> "class" =: "btn btn-secondary"
         b0 <- bootstrapButton "0" $ gridItem <> "class" =: "btn btn-secondary"
         bEq <- bootstrapButton "=" $ gridItem <> "class" =: "btn btn-primary"
         bDot <- bootstrapButton "." $ gridItem <> "class" =: "btn btn-secondary"
         bDiv <- bootstrapButton "/" $ gridItem <> "class" =: "btn btn-secondary"
         ev <- foldDyn eval initialState $ leftmost
           [ Character '.' <$ bDot
           , Character '0' <$ b0
           , Character '1' <$ b1
           , Character '2' <$ b2
           , Character '3' <$ b3
           , Character '4' <$ b4
           , Character '5' <$ b5
           , Character '6' <$ b6
           , Character '7' <$ b7
           , Character '8' <$ b8
           , Character '9' <$ b9
           , Add <$ bAdd
           , Sub <$ bSub
           , Mul <$ bMul
           , Div <$ bDiv
           , Clear <$ bClear
           , Eq <$ bEq
           , Del <$ bDel
           ]
         return ()
      return ()




gridItem = "style" =: ("border-radius: 5px;"
                       <> "padding: 20px;"
                       <> "font-size: 150%;")


gridItemInput = "style" =: ("border-radius: 5px;"
                            <> "padding: 20px;"
                            <> "font-size: 150%;"
                            <> "grid-column: 1 / 4;"
                            <> "grid-row: 1;"
                            <> "z-index: 10;")
gridItemClear = "style" =: ("border-radius: 5px;"
                            <> "padding: 20px;"
                            <> "font-size: 150%;"
                            <> "grid-column: 1 / 5;"
                            <> "grid-row: 2;"
                            <> "z-index: 10;")
gridContainer = "style" =: ("width: 600px;"
                            <> "display: grid;"
                            <> "grid-gap: 10px;"
                            <> "grid-template-columns: 100px 100px 100px 100px;")


bootstrapButton :: DomBuilder t m => Text -> Map.Map T.Text T.Text -> m (Event t ())
bootstrapButton t c = do
  (e, _) <- elAttr' "a" c $ text t
  return $ domEvent Click e


