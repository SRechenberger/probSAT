module Main (main) where
main :: IO ()
main = do
  lns <- map read . concat . map (drop 3) . map words . lines <$> getContents
  print (average lns)

penalty :: Double -> Double
penalty x
  | x >= 2000000 = x * 10
  | otherwise    = x

average :: [Double] -> Double
average ls = sum ls / toEnum (length ls)

