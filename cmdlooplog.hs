-- cmdlooplog: loops input command until it returns an ExitSuccess
import Control.Concurrent
import Data.Time
import System.Environment
import System.Exit
import System.Locale
import System.Process

getTimeStamp = fmap (formatTime defaultTimeLocale "%Y%m%d%H%M%S") getCurrentTime

main = do 
  (e,a) <- fmap (splitAt 1) getArgs
  if null e
    then do putStrLn "cmdlooplog: no input command"
            exitFailure
    else loop (concat e) a

loop exec args = do
  putStrLn $ "running command: " ++ exec ++ " " ++ show args
  timestamp <- getTimeStamp
  (ecode,stdout,stderr) <- readProcessWithExitCode exec args ""
  writeFile (exec ++ "-stdout-" ++ timestamp ++ ".log") stdout
  writeFile (exec ++ "-stderr-" ++ timestamp ++ ".log") stderr
  case ecode of
    ExitSuccess -> putStrLn "done!"
    _           -> do threadDelay 1000000
                      loop exec args
