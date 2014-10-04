{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prelude hiding (head, id, div)
import Text.Blaze.Html5 hiding (map, option)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze (ToMarkup(..))
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Data.Text (Text)
import Data.Monoid
import Control.Monad.State (State)

import Control.Applicative ((<$>), (<*>), (<*), (<$))
import Control.Monad (liftM)
import Control.Applicative ((<*))
import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Text.Parsec.Pos
import Data.List (isPrefixOf, isInfixOf, intercalate, intersperse)
import qualified Data.Map as M
import System.Environment


data Rose = Rose String [Rose]  deriving Show

testData = [Rose "hello" [], Rose "world" [Rose "nested" [], Rose "here" []]]

blazeRose :: [Rose] -> H.Html
blazeRose ((Rose s xs):ys) = (H.div $ toHtml s >> (blazeRose xs)) >> blazeRose ys
blazeRose [] = return ()

------------------------------------------------------------------------

-- parse indented strings

type IParser a = ParsecT String () (State SourcePos) a

pContainer :: IParser Rose
pContainer = withBlock Rose pLine pContainer

pLine = manyTill anyChar newline <* spaces

runParse :: IParser a -> String -> Either ParseError a
runParse p inp = runIndent "" $ runParserT p () "" inp

main = do
  inp <- getContents
  case runParse (many1 pContainer) inp of
    Left err -> print err
    Right res -> do 
        putStrLn . renderHtml . blazeRose $ res
        print res

