import Data.Map
import Data.Time
import Finance.Quote.Yahoo
import System.Locale

myrphp = "MYRPHP=X"
field = "l1" -- Last Trade, see http://www.gummy-stuff.org/Yahoo-data.htm

main = do
  timestamp <- fmap (formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z") getCurrentTime
  q <- getQuote [myrphp] [field]
  print q
  case q of
    Nothing -> error "no map"
    Just m  -> case Data.Map.lookup (myrphp,field) m of
      Nothing -> return ()
      Just a  -> appendFile "/home/eremit/yahoo_myrphp_rates.txt" $ "[" ++ timestamp ++ "]  PHP: " ++ a ++ "\n"
