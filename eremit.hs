{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Data.Attoparsec.Text
import Data.Either
import Data.Text.Lazy
import Data.Text.Format
import Data.Text.Encoding
import qualified Data.Text.IO as T
import Network.HTTP.Conduit
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
  request <- parseUrl "http://www.eremit.com.my/"
  (date,bs) <- withManager $ \manager -> do
    Response _ _ headers body <- httpLbs request manager
    return (lookup "Date" headers,body)
  let rawrates = fromDocument (HTML.parseLBS bs)
               $// element "div"
               >=> attributeIs "id" "exchange"
               >=> element "div"
               &// content
      rates = rights $ Prelude.map (parseOnly currPair) rawrates
  writeRates date rates

writeRates (Just d) xs =
  mapM_ (\(c,f) ->
          T.appendFile "/home/eremit/eremit_rates.txt"
          (toStrict (format "[{}] {}: {}\n" (decodeUtf8 d,show c,f)))
        ) xs
writeRates _ _ = return ()
