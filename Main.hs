{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prelude hiding (head, id, div)
import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze (ToMarkup(..))
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Data.Text (Text)
import Data.Monoid


data Rose = Rose String [Rose]  deriving Show

testData = [Rose "hello" [], Rose "world" [Rose "nested" [], Rose "here" []]]

blazeRose :: [Rose] -> H.Html
blazeRose ((Rose s xs):ys) = (H.div $ toHtml s >> (blazeRose xs)) >> blazeRose ys
blazeRose [] = return ()

main = do
  putStrLn . renderHtml . blazeRose $ testData

