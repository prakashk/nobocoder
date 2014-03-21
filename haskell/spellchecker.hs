-- spellchecker
-- port of the Scala version from https://github.com/xrrocha/nobocoder

import System.IO
import Data.Function (on)
import Data.List (sortBy)
import Control.Monad (forM, forM_)
import Levenshtein (dist)

-- filter when predicate is false
filterNot p = filter (not . p)

-- join list of strings using delimiter
join :: String -> [String] -> String
join _ [] = ""
join d (x:[]) = x
join d (x:xs) = x ++ d ++ join d xs

minSimilarity = 0.75
type Dictionary = [String]

-- get suggestions for a term
suggestions :: String -> Dictionary -> (Double -> Bool) -> [String]
suggestions term dict isSimilar = map (fst)
                                  $ sortBy (compare `on` snd)
                                  $ filter (\a -> isSimilar $ snd a)
                                  $ map (\dw -> (dw, similarity term dw)) dict

-- compute similarity score (inverse of the distance) for pair of
-- words.  Not sure how close this is to the Apache Lucene's
-- LevenshteinDisatnce, but since it returns a float between 0
-- (completely different) and 1.0 (identical), let's try to do the
-- same thing.  The return values appear to coincide with the example
-- words listed on https://github.com/xrrocha/nobocoder

similarity :: String -> String -> Double
similarity w1 w2 =
  let distance = dist w1 w2
      maxLength = max (length w1) (length w2)
  in 1.0 - (fromIntegral distance / fromIntegral maxLength)

main = do
  contents <- readFile "../files/words.txt"
  let dictionary = lines contents
      terms = ["good", "word", "here", "badd", "wurd", "herre", "notaword"]
      badTerms = filterNot (\term -> term `elem` dictionary) terms
      isSimilar = (minSimilarity <=)
  forM_ badTerms
    (\term -> do
        putStrLn $ case suggestions term dictionary isSimilar of
          []   -> "Whaddaya mean '" ++ term ++ "'?"
          sugs -> term ++ ": you probably meant one of " ++ join ", " sugs
    )
