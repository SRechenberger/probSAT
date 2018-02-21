#!/usr/bin/runhaskell

import System.Directory
import System.IO
import Control.Arrow
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Concurrent
import Control.Monad (forM_, forM, when)
import Control.DeepSeq (($!!), NFData)
import System.Process
import System.Exit
import Text.Printf
import Data.Foldable
import Data.Function
import Control.Monad.Random

import Debug.Trace

debug :: Show a => a -> a
debug a = trace (show a) a

force :: NFData a => a -> a
force a = id $!! a

type Testfile = FilePath

type Flips = Int

type Penalty = Int -> Int

data SingleResult
  = Unknown !Flips Double Double
  | Success !Flips Double Double

isSuccess :: SingleResult -> Bool
isSuccess (Success _ _ _) = True
isSuccess _           = False

getFlips :: Penalty -> SingleResult -> Flips
getFlips p (Unknown f _ _) = p f
getFlips _ (Success f _ _) = f

getEntropy :: SingleResult -> Double
getEntropy (Unknown _ e _) = e
getEntropy (Success _ e _) = e

getCB :: SingleResult -> Double
getCB (Success _ _ cb) = cb
getCB (Unknown _ _ cb) = cb

data GroupResult = GR
  { grname   :: String
  , total    :: Int
  , succs    :: Int
  , avgFlips :: Double
  , maxFlips :: Int
  , minFlips :: Int
  , steps    :: [(Int, Double, Double)]
  }

instance Show GroupResult where
  show gr = printf "%-20s\t%-20d\t%-20d\t%-20.2f\t%-20d\t%-20d"
    (grname gr)
    (total gr)
    (succs gr)
    (avgFlips gr)
    (maxFlips gr)
    (minFlips gr)

evalResults :: String -> Penalty -> [SingleResult] -> GroupResult
evalResults n p results = GR
  { grname   = n
  , total    = length results
  , succs    = length $ filter isSuccess results
  , avgFlips = let s = map (getFlips p) >>> sum >>> toEnum $ results
                   l = length >>> toEnum $ results
                in s/l
  , maxFlips = maximumBy (compare `on` getFlips p) >>> getFlips p $ results
  , minFlips = minimumBy (compare `on` getFlips p) >>> getFlips p $ results
  , steps    = map (\l -> (getFlips p l, getEntropy l, getCB l)) results
  }

data Benchmark = Benchmark
  { bmname :: String
  , tests  :: [FilePath]
  }

benchmarkFromDirectory :: String -> FilePath -> IO Benchmark
benchmarkFromDirectory name dir = do
  fps <- map (\f -> dir ++ "/" ++ f) <$> listDirectory dir
  pure $ Benchmark name fps

runBenchmark :: String -> [String] -> Penalty -> Benchmark -> IO GroupResult
runBenchmark solvercmd args p bm = do
  semaphor <- newQSem 4
  results' <- forM (tests bm) $ \fp -> do
    var <- newEmptyMVar
    forkIO $ do
      waitQSem semaphor
      cb <- getRandomR (0,5) :: IO Double
      let cbOpt = ["--cb", printf "%.2f" cb]
      (_, Just hout, _, hdl) <- createProcess (proc solvercmd $ args ++ [fp]){ std_out = CreatePipe, std_err = UseHandle stderr }
      e <- waitForProcess hdl
      rLine' <- (force >>> lines >>> filter isResultLine) <$> hGetContents hout
      (flips,entropy) <- case rLine' of
        []  -> error $ printf "runBenchmark: %s: no result line." fp
        l:_ -> words >>> drop 1 >>> (\(f:h:_) -> (read f, read h)) >>> pure $ l
      case e of
        ExitSuccess     -> putMVar var $ Unknown flips entropy cb
        ExitFailure 10  -> putMVar var $ Success flips entropy cb
        ExitFailure 124 -> putMVar var $ Unknown flips entropy cb
        others          -> error $ printf "invalid exit code: %s" (show others)
      hClose hout
      signalQSem semaphor
    pure var
  evalResults (bmname bm) p <$> mapM takeMVar results'
 where
  isResultLine :: String -> Bool
  isResultLine ('c':'E':_) = True
  isResultLine _ = False

  watch :: MVar Int -> Int -> Double -> IO ()
  watch var 0 _    = pure ()
  watch var n last = do
    n' <- readMVar var
    let percentage = 100 * (toEnum n' / toEnum n)
    when (percentage > last)
      (printf "%-25s [%-100s] %.2f percent.\r" (bmname bm) (replicate (fromEnum percentage) '#') percentage)
    if percentage >= 100
      then putStrLn ""
      else watch var n percentage

main :: IO ()
main = do
  (elogfile:solver:args:penalty'':benchmarks'') <- (force >>> lines) <$> getContents
  let penalty' = read penalty''
      penalty | penalty' <= 1 = id
              | otherwise    = (* penalty')
      benchmarks' = map words >>> map (\[n,d] -> (n,d)) $!! benchmarks''
  benchmarks <- mapM (uncurry benchmarkFromDirectory) benchmarks'
  results <- forM benchmarks $ \b -> do
    runBenchmark solver (words args) penalty b
  printf "%-20s\t%-20s\t%-20s\t%-20s\t%-20s\t%-20s\n"
    "name" "total" "succs" "avg flips" "max flips" "min flips"
  forM_ results print

  withFile elogfile WriteMode $ \h -> do
    hPrintf h "%s,%s,%s\n" "flips" "entropy" "cb"
    forM_ (map steps >>> concat $ results) (\(f,e,cb) -> hPrintf h "%d,%.5f,%.2f\n" f e cb)
    









