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

instance H.ToMarkup Currency where
  toMarkup = H.toHtml . show

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

colorBlue = sRGB24read "#7aa6da"
colorOrange = sRGB24read "#e78c45"

getTripleCurrency ((((rate:_),_,_):_)) = Just $ currency rate
getTripleCurrency _                    = Nothing

main = do
  elogs <-readRates eremitlog
  ylogs <-readRates yahoolog
  let groupedCurrencies = (groupByCurrency . sortByCurrency) elogs
      groupByCurrency = groupBy (\x y -> currency x == currency y)
      sortByCurrency = sortBy (\x y -> compare (currency x) (currency y))
      yahooChartData = (ylogs,"yahoo",colorOrange)
      eremitChartData = (map (\rates -> [(rates,"eremit",colorBlue)]) groupedCurrencies)
      allChartData = map (\cdata -> cdata ++ (auxData cdata)) eremitChartData
      auxData x = case (getTripleCurrency x) of
        Just PHP -> [yahooChartData]
        _        -> []
  T.writeFile (datadir ++ "eremitfx.html") $ toStrict $ renderHtml $ htmlPage groupedCurrencies
  mapM_ renderChart allChartData

htmlPage xs = [shamlet|
  <html>
    <head>
      <link rel="stylesheet" href="eremitfx.css" type="text/css" />
    <body>
      <div>
        $if null xs
          error: null invalid in htmlPage
        $else
          $forall x <- xs
            #{chartTable x}
        |]

chartTable rates = [shamlet|
  $if null rates
    error: null invalid in htmlPage      
  $else
    <table>
      #{pair2Html True headrate updatetime}
      $forall rate <- restrates
        #{pair2Html False rate updatetime}
    <br>
  |]
  where
    (headrate:restrates) = (reverse . list2Pairs . concatMap dedup . groupByAmount) rates
    groupByAmount = groupBy (\x y -> amount x == amount y)
    dedup (x:[]) = [x]
    dedup xs = [minimum xs] -- add ", maximum xs" for graphing
    updatetime = timestamp $ last rates

renderChart rates = do
  let plots = map (\(rates,name,color) -> plotRates rates name color) rates
  case (getTripleCurrency rates) of
    (Just curr') -> renderableToSVGFile (chart plots) 1280 300 ( datadir ++ show curr' ++ ".svg")
    _            -> putStrLn "invalid data in renderChart"

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
          <strong>MYR = #{displayCurrency}&nbsp;
          #{amount curr}&nbsp;
          $if positive
            <div class="big-arrow-up">
          $else
            <div class="big-arrow-down">
          <div class="#{color}">&nbsp;#{text}
        <div>
           <small>since #{timestamp curr} &bull; updated #{updatetime}
        <div><img class="chart" src="#{displayCurrency}.svg">
  $else
    <tr>
      <td><strong>#{displayCurrency} #{val}
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
    displayCurrency = currency prev
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
