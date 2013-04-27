{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import           Control.Applicative
import           Data.Accessor
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Colour
import           Data.Colour.Names
import           Data.Colour.SRGB
import           Data.Either
import           Data.List
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Text.Lazy                (toStrict)
import           Data.Time
import           Graphics.Rendering.Chart
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

readRates f = fmap (rights . map (parseOnly fxLine) . T.lines) (T.readFile f)

datadir = "/home/eremit/"
eremitlog = datadir ++ "eremit_rates.txt"
yahoolog  = datadir ++ "yahoo_myrphp_rates.txt"

main = do
  elogs <-readRates eremitlog
  ylogs <-readRates yahoolog
  let prices = filter isPHP elogs
      prices' = reverse $ list2Pairs $ dedup prices
      isPHP x = currency x == PHP
      eqRate x y = amount x == amount y
      dedup xs = concatMap dedup' groupedlist
        where
          groupedlist = groupBy eqRate xs
          dedup' (x:[]) = [x]
          dedup' xs' = [minimum xs'] -- add ", maximum xs" for graphing
  T.writeFile (datadir ++ "eremitfx.html") $ toStrict $ renderHtml $ htmlPage prices' $ timestamp $ last prices
  let eremitPlot = plotRates prices "eremit" $ sRGB24read "#7aa6da" -- blue
      yahooPlot = plotRates ylogs "yahoo" $ sRGB24read "#e78c45" --orange
  renderableToPNGFile (chart [eremitPlot,yahooPlot]) 470 200 $ datadir ++ "eremitfx.png"

htmlPage (x:xs) updatetime = [shamlet|
  <html>
    <head>
      <link rel="stylesheet" href="eremitfx.css" type="text/css" />
    <body>
      <div>
        <table>
          #{pair2Html True x updatetime}
          $forall x' <- xs
            #{pair2Html False x' updatetime}
        |]
htmlPage _ _ = H.toHtml ("invalid input" :: String)

list2Pairs list = if length list == 1
                    then []
                    else go list
  where
    go (x1:x2:[]) = [(x1,x2)]
    go (x1:x2:xs) = [(x1,x2)] ++ go (x2:xs)
    go _          = []

pair2Html :: Bool -> (Rate,Rate) -> UTCTime -> H.Html
pair2Html isFirst (prev,curr) updatetime = [shamlet|$newline never
  $if isFirst
    <tr>
      <td colspan=4>
        <div class="currprice">
          <strong>MYR = PHP&nbsp;
          #{amount curr}&nbsp;
          $if positive
            <div class="big-arrow-up">
          $else
            <div class="big-arrow-down">
          <div class="#{color}">&nbsp;#{text}
        <div>
           <small>since #{timestamp curr} &bull; updated #{updatetime}
        <div><img src="eremitfx.png">
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

chart :: [Plot LocalTime Double] -> Renderable ()
chart plots = toRenderable layout
  where
    bg = transparent
    fg = opaque white
    fg1 = opaque black
    layout = layout1_background ^= solidFillStyle bg
           $ updateAllAxesStyles (axis_grid_style ^= solidLine 1 fg1)
           $ layout1_bottom_axis ^: laxis_override ^= axisGridHide
 	   $ layout1_plots ^= [ Left x | x <- plots]
           $ layout1_grid_last ^= False
           $ setLayout1Foreground fg
           $ defaultLayout1

lineStyle c = line_width ^= 2
            $ line_color ^= c
            $ defaultPlotLines ^. plot_lines_style

plotRates rates label color
  = toPlot $ plot_lines_style ^= lineStyle (opaque color)
  $ plot_lines_values ^= [[ (utcToLocalTime tzMYT u, a) | Rate u _ a <- rates]]
  $ plot_lines_title ^= label
  $ defaultPlotLines
