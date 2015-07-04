{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Control.Applicative
import Data.Attoparsec.Text
import Data.Either
import Data.Text.Lazy (toStrict)
import Data.Text.Format
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Conduit
import Text.HTML.DOM as HTML
import Text.XML.Cursor
import Text.XML.Scraping (innerText)
import Text.XML.Selector.TH

data Currency = BDT | IDR | INR | LKR | MMK | NPR | PHP | PKR | SGD | VND deriving Show

currencyParser = string "BDT" *> pure BDT
             <|> string "IDR" *> pure IDR
             <|> string "INR" *> pure INR
             <|> string "LKR" *> pure LKR
             <|> string "MMK" *> pure MMK
             <|> string "NPR" *> pure NPR
             <|> string "PHP" *> pure PHP
             <|> string "PKR" *> pure PKR
             <|> string "SGD" *> pure SGD
             <|> string "VND" *> pure VND

currPair :: Parser (Currency,Double)
currPair = do
  amount <- double
  skipSpace
  curr <- currencyParser
  return (curr,amount)

main = do
  request <- parseUrl "http://www.eremit.com.my/rates"
  (date,bs) <- withManager $ \manager -> do
    response <- httpLbs request manager
    return (lookup "Date" (responseHeaders response),responseBody response)
  let doc = (fromDocument . parseLBS) bs
      dropCommas = T.filter (/= ',')
      rawrates = queryT [jq| #exchange > div > font |] doc
      rates = rights $ Prelude.map (parseOnly currPair . dropCommas . toStrict . innerText) rawrates
  writeRates date rates

writeRates (Just d) xs =
  mapM_ (\(c,f) ->
          T.appendFile "/home/eremit/eremit_rates.txt"
          (toStrict (format "[{}] {}: {}\n" (decodeUtf8 d,show c,f)))
        ) xs
writeRates _ _ = return ()
