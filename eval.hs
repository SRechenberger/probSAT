module Main (main) where
main :: IO ()
main = do
  lns <- map read . concat . map (drop 3) . map words . lines <$> getContents
  print (average lns)

average :: [Double] -> Double
average ls = sum ls / toEnum (length ls)

