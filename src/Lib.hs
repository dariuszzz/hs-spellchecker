module Lib
    ( 
        WordBank (..),
        loadWordBank,
        findSimilarWords,
        SimilarityResult (Correct, Matches, NoMatches)
    ) where

data WordDist = WordDist String Int deriving (Show, Eq)

getWordStr :: WordDist -> String
getWordStr (WordDist str _ ) = str

getWordSimilarity :: WordDist -> Int
getWordSimilarity (WordDist _ simil) = simil

instance Ord WordDist where
    (WordDist _ s1) `compare` (WordDist _ s2) = s1 `compare` s2 


newtype WordBank = WordBank [String]

loadWordBank :: String -> IO WordBank
loadWordBank path = do
    file <- readFile path
    return (WordBank (lines file))

data SimilarityResult = 
    NoMatches
    | Matches [String]
    | Correct

findSimilarWords :: String -> WordBank -> SimilarityResult
findSimilarWords word (WordBank bank) 
    | word `elem` bank = Correct
    | otherwise =  do  
        let similarities = map (\x -> WordDist x (levenDist word x)) bank
        let min_dist = getWordSimilarity $ minimum similarities
        -- Dont match if the min difference is too large
        if min_dist > 3 then NoMatches
        else Matches (map getWordStr $ filter (\x -> min_dist == getWordSimilarity x) similarities)

levenDist :: String -> String -> Int
levenDist a b
    | null b = length a
    | null a = length b
    | head a == head b = levenDist (tail a) (tail b)
    | otherwise = do
        let tab = levenDist (tail a) b
        let atb = levenDist a (tail b)
        let tatab = levenDist (tail a) (tail b)

        1 + min tab (min atb tatab)