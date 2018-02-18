import System.Process
import System.IO
import Control.DeepSeq (($!!), NFData)
import Control.Monad (forM_, forM, when)
import System.Environment
import Control.Arrow
import Control.Monad.State
import Text.Printf
import Data.List

both f = f *** f

hamming :: String -> String -> Int
hamming = hamming' 0
 where
  hamming' acc [] [] = acc
  hamming' acc (a:as) (b:bs)
    | a == b    = hamming' (acc+1) as bs
    | otherwise = hamming' acc as bs


average :: [Double] -> Double
average xs = sum xs / toEnum (length xs)

force :: NFData a => a -> a
force a = id $!! a

data StepReport = SR
  { clause     :: Int
  , entropy    :: Double
  , assignment :: String
  , probs      :: [Double]
  }

data RunReport = RR
  { start    :: String
  , solution :: Maybe String
  , steps    :: [StepReport]
  }

data Evaluation
  = Success
    { total    :: Int
    , good     :: Int
    , avgHGood :: Double
    , maxHGood :: Double
    , minHGood :: Double
    , bad      :: Int
    , avgHBad  :: Double
    , maxHBad  :: Double
    , minHBad  :: Double
    , initD    :: Int
    }
  | Failure
    { total :: Int
    , avgH  :: Double
    , maxH  :: Double
    , minH  :: Double
    }

instance Show Evaluation where
  show (Success s g avgG maxG minG b avgB maxB minB initD)
    = printf
      (  "SUCCESS total: %-10d; initial distance: %d\n"
      ++ "  good : %-10d avgH: %-10.3f maxH: %-10.3f minH: %-10.3f\n"
      ++ "  bad  : %-10d avgH: %-10.3f maxH: %-10.3f minH: %-10.3f\n")
      s initD g avgG maxG minG b avgB maxB minB
  show (Failure s a max min)
    = printf
      (  "FAILURE\n"
      ++ "  steps: %-10d avgH: %-10.3f maxH: %-10.3f minH: %-10.3f\n")
      s a max min

evalReport :: RunReport -> Evaluation
evalReport (RR a Nothing ss) =
  Failure
    (length ss)
    (average (map entropy ss))
    (maximum (map entropy ss))
    (minimum (map entropy ss))
evalReport (RR a (Just a') ss) =
  Success
    (length ss)
    (length gs)
    (average (map entropy gs))
    (maximum (map entropy gs))
    (minimum (map entropy gs))
    (length bs)
    (average (map entropy bs))
    (maximum (map entropy bs))
    (minimum (map entropy bs))
    (hamming a a')


 where
  evalSteps [] = []
  evalSteps [s] = [(True, s)]
  evalSteps (s:s':ss) =
    (hamming a' (assignment s) < hamming a' (assignment s'), s) : evalSteps (s':ss)

  ss' = evalSteps ss
  (gs, bs) = partition fst >>> map snd *** map snd $ ss'


addStep :: StepReport -> State [RunReport] ()
addStep s = modify (\(RR assgn Nothing ss:rs) -> RR assgn Nothing (s:ss) : rs)

processReport :: [String] -> State [RunReport] ()
processReport (('R':assgn):rest) = do
  modify ((RR assgn Nothing []):)
  processReport rest
processReport (('L':ln):rest) = do
  let (clIdx:clH:assgn:probs) = words ln
  addStep (SR (read clIdx) (read clH) assgn (map read probs))
  processReport rest
processReport (('S':assgn):rest) = do
  modify (\(RR assgn' Nothing ss:rs) -> RR assgn' (Just assgn) ss : rs)
  processReport rest
processReport ((fail):_) = error $ "wtf?: " ++ fail
processReport [] = pure ()

main :: IO ()
main = do
  file:args <- getArgs
  (_, _, Just herr, hpr) <- createProcess
    (proc "./probSAT_L" (args ++ [file]))
      { std_out = UseHandle stdout
      , std_err = CreatePipe
      }

  es <- (lines >>> (\s -> execState (processReport s) []) >>> map evalReport)
    <$> hGetContents herr

  mapM_ print es

