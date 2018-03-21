{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.List         (isInfixOf)
import           ReadArgs          (readArgs)
import           Text.HandsomeSoup
import           Text.XML.HXT.Core

main :: IO ()
main = do
  (inputfile :: String) <- readArgs
  contents <- readFile inputfile
  let doc = parseHtml contents
      titles = runLA (hread >>> css "ul li div a" //> getText) contents
      scores = filter (isInfixOf "fill") (runLA (hread >>> css "use" >>> getAttrValue "xlink:href") contents)
      results = pairToLine <$> (zip titles scores)
  putStr $ unlines results


pairToLine (a,b) = a ++ "," ++ (show $ thumbToInt b)
pairToLine _ = error "unexpected input to pairToLine"

thumbToInt "#thumb-up-filled"   = 1
thumbToInt "#thumb-down-filled" = 0
thumbToInt _                    = error "unexpected input to thumbToInt"
