module Main where
import Control.Applicative (Applicative((<*>), pure))
import Control.Monad (ap)
import Data.Ratio ((%))
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)
import Text.Printf (printf)
import qualified Text.ParserCombinators.ReadP as P
import Data.SciRatio
import Data.SciRatio.Read
import Data.SciRatio.Show

-- | Whether output should be suppressed.
quiet :: Bool
quiet = True

data TestState
  = TestState
    { failCount :: Int }

initTestState :: TestState
initTestState
  = TestState
    { failCount = 0 }

newtype Test a = Test { runTest :: TestState -> IO (a, TestState) }

instance Functor Test where
  fmap f u = return f `ap` u

instance Applicative Test where
  pure  = return
  (<*>) = ap

instance Monad Test where
  return x     = Test $ \ s -> return (x, s)
  Test f >>= u = Test $ \ s -> do
    (x, s') <- f s
    runTest (u x) s'
  fail s       = failTest "aborted" >> fail s

failTest :: String -> Test ()
failTest s = do
  liftIO . hPutStrLn stderr $ "*** FAIL: " ++ s
  s <- getTestState
  let TestState { failCount = n } = s
  putTestState s { failCount = n + 1 }

passTest :: String -> Test ()
passTest s =
  if quiet
  then return ()
  else liftIO . putStrLn $ "ok" ++ if null s then "" else printf " (%s)" s

expect :: String -> Bool -> Test ()
expect s False = failTest s
expect s True  = passTest s

liftIO :: IO a -> Test a
liftIO m = Test $ \ s -> do
  x <- m
  return (x, s)

getTestState :: Test TestState
getTestState = Test $ \ s -> return (s, s)

putTestState :: TestState -> Test ()
putTestState s = Test $ \ _ -> return ((), s)

-- | Set a time limit for a test (in seconds).
timeoutT :: Double -> String -> Test () -> Test ()
timeoutT t n m = Test $ \ s -> do
  result <- timeout (round (t * 1e6)) (runTest m s)
  case result of
    Just (x, s) -> return (x, s)
    Nothing     -> runTest (failTest ("timed out: " ++ n)) s

testCases :: [(String, SciRational)]
testCases =
  [ ("0", -0.0e+3)
  , ("25", 0.25e+2)
  , ("-.1", -1.0e-1)
  , ("2.5e20/3", 5.0e+20 / 6)
  , ("-0.0e+3", (0 % 1) .^ 0)
  , ("0.25e+2", (25 % 1) .^ 0)
  , ("-1.0e-1", ((-1) % 1) .^ (-1))
  , ("5.0e+20/6.e0", (25 % 3) .^ 19)
  , ("0xfeedface", 4277009102)
  , ("0xfeedface", 0xfeedface)
  , ("0e0", 0)
  , ("-00", 0)
  , ("-.0000", 0)
  , ("-.01", -0.01)
  , (".00008", 0.00008)
  , ("1", 1)
  , ("+1", 1)
  , ("-1", -1)
  , ("1.", 1)
  , ("-1.", -1)
  , ("+1.", 1)
  , ("3/5", 3 / 5)
  , ("5e20/5", 5e20 / 5)
  , ("1/3", 1 / 3)
  , ("-2/3", -2 / 3)
  , ("-2000/3", -2000 / 3)
  , ("-2000/-3", -2000 / (-3))
  , ("-2000/-3.e3", -2000 / (-3e3))
  , ("37e-25/125", 37e-25 / 125)
  , ("-1/2", -1 / 2)
  , ("2e1", 2e1)
  , ("1e2", 1e2)
  , ("1e-3", 1e-3)
  , ("-.48e+1/.39", -0.48e1 / 0.39)
  , ("-2.14e+1/17", -2.14e1 / 17)
  , ("-.3e+1/17", -0.3e1 / 17)
  , ("-2.e+1/17", -2e1 / 17)
  , ("3.14e-17", 3.14e-17)
  , ("0xfFed90", 0xfFed90)
  , ("0o013573", 0o013573)
  , ("0b1001101", 77)
  ]

testCasesExtreme :: [(String, SciRational)]
testCasesExtreme =
  [ ("1e99999999", (1 % 1) .^ 99999999)
  , ("37e-200/125", 37 / 125 .^ (-200))
  , ("0.23456e-99999999", 0.23456 .^ (-99999999))
  , ("-0.002397/338792e-9999999999", -0.002397 / 338792 .^ 9999999999)
  ]

readSciRationalT :: String -> (SciRational -> Test ()) -> Test ()
readSciRationalT s f = case P.readP_to_S readSciRationalP s of
  [(x, [])] -> f x
  r         -> failTest $ printf "can't parse: %s (%s)" s (show r)

-- | Tests the comparison operation in both directions.
testOrd :: String -> Ordering -> String -> Test ()
testOrd s ordering s' =
  readSciRationalT s  $ \ x  ->
  readSciRationalT s' $ \ x' -> do
  expect (printf "%s %s %s" s  (show ordering)  s') $
         compare x x' == ordering
  expect (printf "%s %s %s" s' (show ordering') s)  $
         compare x' x == ordering'
  where invert GT = LT
        invert EQ = EQ
        invert LT = GT
        ordering' = invert ordering

testShowRead :: String -> SciRational -> Test ()
testShowRead s x =
  let s' = showNumber x in
  readSciRationalT s' $ \ x' ->
  if x' == x
  then passTest $ printf "%s ==> %s" s s'
  else failTest $ printf "shown result (%s) not equal to original (%s)"
                         (show x') (show x)

testReadShowRead :: (String, SciRational) -> Test ()
testReadShowRead (s, y) =
  readSciRationalT s $ \ x ->
  if x == y
  then testShowRead s x
  else failTest $ printf "parsed result (%s) not equal to expected (%s): %s"
                         (show x) (show y) s

test :: Test ()
test = do

  -- recip must recanonicalize the number otherwise this test would fail
  expect "5 == recip (1 / 5)" $
          5 == recip (1 / 5 :: SciRational)

  -- comparison tests
  testOrd "0" EQ "0"
  testOrd "0" LT "1"
  testOrd "0" GT "-1"
  testOrd "10" LT "11"
  testOrd "11" GT "10"
  testOrd "-5e2" LT "400"
  testOrd "5e2" GT "400"
  testOrd "3.5e99999" GT "-2"
  testOrd "-3.5e99999" LT "2"
  testOrd "-3.5e-99999" LT "2"
  testOrd "3.5e99999" EQ "3.5e99999"
  testOrd "-3.5e99999" LT "3.5e99999"
  testOrd "-3.5e99999" EQ "-3.5e99999"
  testOrd "-3.5e99999" LT "-2.5e99999"
  testOrd "3.5e99999" GT "2.5e99999"
  testOrd "3.5e99999" GT "0"

  mapM_ testReadShowRead testCases
  timeoutT 5 "testCasesExtreme" $ mapM_ testReadShowRead testCasesExtreme

main :: IO ()
main = do
  ((), TestState { failCount = n }) <- runTest test initTestState
  if n > 0 then exitFailure else exitSuccess
