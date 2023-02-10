module Lib
    ( 
        WordBank (..),
        loadWordBank,
        findSimilarWords,
        SimilarityResult (Correct, Matches, NoMatches),
        levenDist,
        dameLevenDist
    ) where
import Data.Char (toLower)
import Data.Array((!), listArray, range)

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
    let lowercaseWords = map (map toLower) $ lines file
    return (WordBank lowercaseWords)

data SimilarityResult = 
    NoMatches
    | Matches [String]
    | Correct

findSimilarWords :: WordBank -> (String -> String -> Int) -> String -> SimilarityResult
findSimilarWords (WordBank bank) distF word 
    | word `elem` bank = Correct
    | otherwise =  do  
        let similarities = map (\x -> WordDist x (distF word x)) bank
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
        let tatb = levenDist (tail a) (tail b)

        1 + min tab (min atb tatb)

dameLevenDist :: String -> String -> Int
dameLevenDist a b = ms ! (length a, length b)
    where
        m i 0 = i
        m 0 j = j
        m i j
            | a !! (i - 1) == b !! (j -1) = ms ! (i - 1, j -1)
            | otherwise = 1 + minimum [
                                    ms ! (i - 1, j),
                                    ms ! (i, j - 1),
                                    ms ! (i - 1, j - 1)
                                    ]


        bounds = ((0, 0), (length a, length b))
        ms = listArray bounds
            [m i j | (i, j) <- range bounds]