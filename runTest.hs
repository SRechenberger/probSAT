import System.Directory
import System.IO
import Control.Arrow
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad (forM_, forM)
import System.Process
import System.Exit
import Text.Printf
import Data.Foldable
import Data.Function

type Testfile = FilePath

type Flips = Int

type Penalty = Int -> Int

data SingleResult
  = Unknown Flips
  | Success Flips

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
  show gr = printf "%-20s\t%-20d\t%-20d\t%-20.2f\t%-20d\t%-20d\n"
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

runBenchmark :: String -> [String] -> Penalty -> Benchmark -> IO GroupResult
runBenchmark solvercmd args p bm = do
  results' <- forM (tests bm) $ \fp -> do
    var <- newEmptyMVar
    forkIO $ do
      (_, Just hout, _, hdl) <- createProcess (proc solvercmd $ args ++ [fp]){ std_out = CreatePipe }
      e <- waitForProcess hdl
      rLine' <- (lines >>> filter isResultLine) <$> hGetContents hout
      flips <- case rLine' of
        []  -> error $ printf "runBenchmark: %s: no result line." fp
        l:_ -> words >>> drop 2 >>> concat >>> read >>> pure $ l
      case e of
        ExitSuccess    -> putMVar var $ Unknown flips
        ExitFailure 10 -> putMVar var $ Success flips
        others         -> error $ printf "invalid exit code: %s" (show others)

    pure var
  evalResults (bmname bm) p <$> mapM takeMVar results'
 where
  isResultLine :: String -> Bool
  isResultLine ('c':' ':'F':_) = True
  isResultLine _ = False

main :: IO ()
main = do
  (solver:args:penalty'':benchmarks'') <- lines <$> getContents
  let penalty' = read penalty''
      penalty | penalty' <= 1 = id
              | otherwise    = (* penalty')
      benchmarks' = map words >>> map (\[n,d] -> (n,d)) $ benchmarks''
  benchmarks <- mapM (uncurry benchmarkFromDirectory) benchmarks'
  results <- forM benchmarks $ \b -> do
    runBenchmark solver (words args) penalty b
  printf "%-20s\t%-20s\t%-20s\t%-20s\t%-20s\t%-20s\n"
    "name" "total" "succs" "avg flips" "max flips" "min flips"
  forM_ results print









