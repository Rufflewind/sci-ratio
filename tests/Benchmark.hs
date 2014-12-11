module Main where
import Data.Foldable (foldl')

factorizeBase1 :: Integer -> Integer -> (Integer, Integer)
factorizeBase1 b n | r  /= 0   = (n,    0)
                  | r' /= 0   = (n'',  e2 + 1)
                  | otherwise = (n''', e2 + 2)
  where (n',   r)  = n   `quotRem` b
        (n'',  e)  = factorizeBase1 (b * b) n'
        (n''', r') = n'' `quotRem` b
        e2         = e * 2

-- slower (takes 65% more time)
factorizeBase2 :: Integer -> Integer -> (Integer, Integer)
factorizeBase2 b n | r  /= 0   = (n,    0)
                   | r' /= 0   = (n',  e2)
                   | otherwise = (n'', e2 + 1)
  where r         = n `rem` b
        (n',  e)  = factorizeBase2 (b * b) n
        (n'', r') = n' `quotRem` b
        e2        = e * 2

-- slower (takes 80% more time)
factorizeBase3 :: Integer -> Integer -> (Integer, Integer)
factorizeBase3 base = go 0 1 0
  where go  _    _   _ 0 = (0, 0)
        go  _    0   e i = (i, e)
        go maxe next e i =
          if i `mod` power == 0
          then go maxe'   next'  (e + next) (i `div` power)
          else go next'' next'' e i
          where power  = base ^ next
                maxe'  = if maxe == 0 then maxe     else maxe - next
                next'  = if maxe == 0 then next * 2 else maxe `div` 2
                next'' = next `div` 2

-- | A simple algorithm for hashing two integers.
(%) :: Integral a => a -> a -> a
x % y = (x + y * 981604067) `rem` 961753483

main :: IO ()
main = print . (== 830177394) . foldl' (%) 0 $ do
  n <- [1 .. 20000 :: Int]
  return (uncurry (%) (factorizeBase1 10 (35 * 10 ^ n)))
