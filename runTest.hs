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

force :: NFData a => a -> a
force a = id $!! a

type Testfile = FilePath

type Flips = Int

type Penalty = Int -> Int

data SingleResult
  = Unknown !Flips
  | Success !Flips

isSuccess :: SingleResult -> Bool
isSuccess (Success _) = True
isSuccess _           = False

getFlips :: Penalty -> SingleResult -> Flips
getFlips p (Unknown f) = p f
getFlips _ (Success f) = f

data GroupResult = GR
  { grname   :: String
  , total    :: Int
  , succs    :: Int
  , avgFlips :: Double
  , maxFlips :: Int
  , minFlips :: Int
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
  }

data Benchmark = Benchmark
  { bmname :: String
  , tests  :: [FilePath]
  }

benchmarkFromDirectory :: String -> FilePath -> IO Benchmark
benchmarkFromDirectory name dir = do
  fps <- map (\f -> dir ++ "/" ++ f) <$> listDirectory dir
  pure $ Benchmark name fps

runBenchmark :: String -> [String] -> Penalty -> Benchmark -> IO [(Double, Int)]
runBenchmark solvercmd args p bm = do
  semaphor <- newQSem 4
  -- status <- newMVar 0
  results' <- forM (tests bm) $ \fp -> do
    var <- newEmptyMVar
    forkIO $ do
      waitQSem semaphor
      (_, Just hout, _, hdl) <- createProcess (proc "timeout" $ ["10", solvercmd] ++ args ++ [fp]){ std_out = CreatePipe, std_err = UseHandle stderr }
      e <- waitForProcess hdl
      rLine' <- (force >>> lines >>> filter isResultLine) <$> hGetContents hout
      flips <- case rLine' of
        []  -> error $ printf "runBenchmark: %s: no result line." fp
        l:_ -> words >>> drop 1 >>> concat >>> read >>> pure $ l
      case e of
        ExitSuccess     -> let (e,f) = flips in putMVar var (e, p f)
        ExitFailure 10  -> putMVar var flips
        ExitFailure 124 -> let (e,f) = flips in putMVar var (e, p f)
        others          -> error $ printf "invalid exit code: %s" (show others)
      -- n <- takeMVar status
      -- putMVar status (n+1)
      hClose hout
      signalQSem semaphor
    pure var
  -- forkIO $ watch status (tests >>> length $ bm) 0
  mapM takeMVar results'
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
  (logname:solver:args:penalty'':benchmarks'') <- (force >>> lines) <$> getContents
  let penalty' = read penalty''
      penalty | penalty' <= 1 = id
              | otherwise    = (* penalty')
      benchmarks' = map words >>> map (\[n,d] -> (n,d)) $!! benchmarks''
  benchmarks <- mapM (uncurry benchmarkFromDirectory) benchmarks'
  results <- forM benchmarks $ \b -> do
    runBenchmark solver (words args) penalty b
  putStrLn "output"
  withFile logname WriteMode $ \hdl -> do
    hPrintf hdl "%-20s,%-20s\n" "entropy" "flips"
    forM_ (concat results) (uncurry (hPrintf hdl "%.5f,%d\n"))









