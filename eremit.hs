{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Data.Attoparsec.Text
import Data.Either
import Text.HTML.DOM as HTML
import Text.XML.Cursor

data Currency = BDT | IDR | INR | LKR | PHP deriving Show

currencyParser = string "BDT" *> pure BDT
             <|> string "IDR" *> pure IDR
             <|> string "INR" *> pure INR
             <|> string "LKR" *> pure LKR
             <|> string "PHP" *> pure PHP

currPair :: Parser (Currency,Double)
currPair = do
  amount <- double
  skipSpace
  curr <- currencyParser
  return (curr,amount)

main = do
  doc <- HTML.readFile "e-remit - Global Money Transfers.html"
  let cursor = fromDocument doc
      result =
        cursor $// element "div"
               >=> attributeIs "id" "exchange"
               >=> element "div"
               &// content

  print $ rights $ map (parseOnly currPair) result

