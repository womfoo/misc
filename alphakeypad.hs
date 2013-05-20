import Data.List
import System.Environment

main = do
  (arg:_) <- getArgs
  mapM_ putStrLn $ toAlpha arg

numDict = [('2',"abc")
          ,('3',"def")
          ,('4',"ghi")
          ,('5',"jkl")
          ,('6',"mno")
          ,('7',"pqrs")
          ,('8',"tuv")
          ,('9',"wxyz")]

alphaDict = transpose' numDict

alphaGroups = map lookupNum

lookupNum n = case lookup n numDict of
  Just n' -> [n] ++ n'
  Nothing -> [n]

toAlpha input = run a1' a2
  where
    a1' = (group . concat) a1
    (a1,a2) = splitAt 1 $ alphaGroups input
    run s [] = s
    run readstrings (unreadhead:unreadrest) = run apps unreadrest
      where apps = concatMap (\x -> map (\y -> x ++ [y]) unreadhead) readstrings

-- [(k,vs)] -> [(k,v)]
transpose' = concatMap (\(k,vs) -> map (\v -> (v,k)) vs)
