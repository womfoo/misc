{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Either
import           Data.List
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Text.Lazy                (toStrict)
import           Data.Time
import           System.Locale
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Hamlet                   (shamlet)
import           Text.Printf
import qualified Text.Blaze.Html5              as H

data Currency = BDT | IDR | INR | LKR | PHP deriving (Eq,Ord,Show)

data Rate = Rate {timestamp :: UTCTime
                 ,currency  :: Currency
                 ,amount    :: Double} deriving (Eq,Ord,Show)

tzMYT = TimeZone { timeZoneMinutes = 480
               , timeZoneSummerOnly = False
               , timeZoneName = "MYT"
               }

currencyParser = string "BDT" *> pure BDT
             <|> string "IDR" *> pure IDR
             <|> string "INR" *> pure INR
             <|> string "LKR" *> pure LKR
             <|> string "PHP" *> pure PHP

instance H.ToMarkup UTCTime where
  toMarkup = H.toHtml . formatTime defaultTimeLocale dayTimeFormat . utcToLocalTime tzMYT

instance H.ToMarkup Rate where
  toMarkup = H.toHtml . show

dayTimeFormat :: String
dayTimeFormat = "%b %e %Y %I:%M %p"

dayFormat :: String
dayFormat = "%b %e" -- %Y"

timeFormat :: String
timeFormat = "%I:%M %p"

fxLine :: Parser Rate
fxLine = do
  char '['
  time <- takeTill (== ']')
  char ']'
  skipSpace
  currency' <- currencyParser
  char ':'
  skipSpace
  amount' <- double
  let time' = fromJust $ parseTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" (T.unpack time)
  return $ Rate time' currency' amount'

readRates = fmap T.lines $ T.readFile "/home/eremit/eremit_rates.txt"

main = do
  logs <- fmap (map (parseOnly fxLine)) readRates
  let prices = reverse $ list2Pairs $ dedup $ filter isPHP (rights logs)
      isPHP x = currency x == PHP
      eqRate x y = amount x == amount y
      dedup xs = concatMap dedup' groupedlist
        where
          groupedlist = groupBy eqRate xs
          dedup' (x:[]) = [x]
          dedup' xs' = [minimum xs'] -- add ", maximum xs" for graphing
  T.writeFile "eremitfx.html" $ toStrict $ renderHtml $ htmlPage prices

htmlPage (x:xs) = [shamlet|
  <html>
    <head>
      <link rel="stylesheet" href="eremitfx.css" type="text/css" />
    <body>
      <div>
        <table>
          #{pair2Html True x}
          $forall x' <- xs
            #{pair2Html False x'}
        |]
htmlPage _ = H.toHtml ("invalid input" :: String)

list2Pairs list = if length list == 1
                    then []
                    else go list
  where
    go (x1:x2:[]) = [(x1,x2)]
    go (x1:x2:xs) = [(x1,x2)] ++ go (x2:xs)
    go _          = []

pair2Html :: Bool -> (Rate,Rate) -> H.Html
pair2Html isFirst (prev,curr) = [shamlet|$newline never
  $if isFirst
    <tr>
      <td colspan=4>
        <strong>MYR/PHP
        &nbsp;eremit - #{timestamp curr}
        <div class="currprice">
          #{amount curr}&nbsp;
          $if positive
            <div class="big-arrow-up">
          $else
            <div class="big-arrow-down">
          <div class="#{color}">&nbsp;#{text}
  $else
    <tr>
      <td><strong>#{val}
      <td>
        $if positive
          <div class="arrow-up">
        $else
          <div class="arrow-down">
        <div class="#{color}">&nbsp;#{text}
      <td>#{day}
      <td>#{time}
    |]
  where
    positive = delta' > 0
    val :: String
    val = printf "%.2f" (amount curr)
    text :: String
    text = printf "%.2f (%.2f%s)" delta deltapct pctsign
    delta = abs delta'
    delta' = amount curr - amount prev
    deltapct = abs (delta' / amount curr) * 100
    color :: String
    color=if positive then "green" else "red"
    showTime format = formatTime defaultTimeLocale format . utcToLocalTime tzMYT
    day = showTime dayFormat (timestamp curr)
    time = showTime timeFormat (timestamp curr)
    pctsign = [chr 37]
