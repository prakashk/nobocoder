-- spellchecker
-- port of the Scala version from https://github.com/xrrocha/nobocoder

import System.IO
import Data.Function (on)
import Data.List (sortBy)
import Control.Monad (liftM)

type Dictionary = [String]

main = do
  print $ map (\term -> suggestions term dictionary isSimilar)
        $ filter (isBadWord) terms
     where terms = ["good", "word", "here", "badd", "wurd", "herre", "notaword"]
           minSimilarity = 0.75
           dictionary = getLines "../files/words.txt"
           isBadWord w = not $ w `elem` dictionary
           isSimilar = (minSimilarity <)

-- get suggestions for a term
suggestions :: String -> Dictionary -> (Double -> Bool) -> [String]
suggestions term dict isSimilar = map (fst)
                                  $ sortBy (compare `on` snd)
                                  $ filter (\a -> isSimilar $ snd a)
                                  $ map (\dw -> (dw, similarity term dw)) dict

-- get all lines from given file
getLines :: FilePath -> [String]
getLines = liftM lines . readFile

-- compute similarity score for the given pair of words
similarity :: String -> String -> Double
similarity w1 w2 = dist w1 w2

-- Levenshtein edit-distance implementation
-- from: http://www.haskell.org/haskellwiki/Edit_distance

dist :: Eq a => [a] -> [a] -> Int
dist a b
    = last (if lab == 0 then mainDiag
            else if lab > 0 then lowers !! (lab - 1)
                 else{- < 0 -}   uppers !! (-1 - lab))
    where mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
          uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
          lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
          eachDiag a [] diags = []
          eachDiag a (bch:bs) (lastDiag:diags) = oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
              where nextDiag = head (tail diags)
          oneDiag a b diagAbove diagBelow = thisdiag
              where doDiag [] b nw n w = []
                    doDiag a [] nw n w = []
                    doDiag (ach:as) (bch:bs) nw n w = me : (doDiag as bs me (tail n) (tail w))
                        where me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
                    firstelt = 1 + head diagBelow
                    thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
          lab = length a - length b
          min3 x y z = if x < y then x else min y z
