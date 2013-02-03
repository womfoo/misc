{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Monad
import Control.Monad.Random
import Data.List
import Data.List.Split (splitOn)

getRandomName names = do
  idx <- evalRandIO $ getRandomR (1,length names - 1) :: IO Int
  return $ names !! idx

main = do
 -- read csv file with format: lastname,givenname,middlename
 -- TODO: replace this with a weighted list of names
  namelines <- fmap (map (splitOn ",") . lines) $ readFile "names.txt"

  let [last,given,middle] = transpose namelines

  [l,g,m] <- forM [last,given,middle] getRandomName
  putStrLn $ concat ["Generated Name: ",l,", ",g," ",m]

{-
  let given' = concatMap words given
  mapM_ print $ sort $ map lengthPair $ groupSort given
  mapM_ print $ sort $ map lengthPair $ groupSort given'
  mapM_ print $ sort $ map lengthPair $ groupSort last
lengthPair s = (length s, head s)
groupSort = group . sort

-}
