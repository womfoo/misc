-- calculate megaworld payment schedule for preselling condos
import Data.List
import Data.Time

totPayable = 5000000.00
reservationFee = 25000
scheduleStart = fromGregorian 2013 1 1
lumpSumPercentage = 0.05
lumpSumFrequency = 10
monthlyBaseTiers = [15000.0,20000.0,25000.0,30000.0] -- base payment increases every year

main = printAllPayments

printAllPayments = mapM_ print $ zip (listMonthEnds scheduleStart) allPayments

allPayments = zipWith (+) monthlyBasePayments monthlyLumpSum

monthlyBasePayments = concatMap (replicate 12) monthlyBaseTiers

monthlyLumpSum = cycleEveryN lumpSumFrequency ((totPayable - reservationFee) * lumpSumPercentage) 0

cycleEveryN len value defvalue = cycle $ replicate (len-1) defvalue ++ [value]

listMonthEnds day = thisMonthEnd day : iterate nextMonthEnd day

thisMonthEnd day = fromGregorian y m $ gregorianMonthLength y m
  where (y,m,_) = toGregorian day

nextMonthEnd day = fromGregorian y' m' $ gregorianMonthLength y' m'
  where (y,m,_) = toGregorian day
        (y',m') = if m >= 12
                     then (succ y,1)
                     else (y,succ m)
