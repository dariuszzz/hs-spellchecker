module Lib
    ( 
        WordBank (..),
        loadWordBank,
        findSimilarWords,
        SimilarityResult (Correct, Matches, NoMatches),
        levenDist,
        dameLevenDist
    ) where
import Data.Array((!), listArray, range)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)



data WordDist = WordDist Text Int deriving (Show, Eq)

getWordStr :: WordDist -> Text
getWordStr (WordDist str _ ) = str

getWordSimilarity :: WordDist -> Int
getWordSimilarity (WordDist _ simil) = simil

instance Ord WordDist where
    (WordDist _ s1) `compare` (WordDist _ s2) = s1 `compare` s2 


newtype WordBank = WordBank [Text]

loadWordBank :: String -> IO WordBank
loadWordBank path = do
    file <- T.readFile path
    let lowercaseWords = map T.toLower $ T.lines file
    return (WordBank lowercaseWords)

data SimilarityResult = 
    NoMatches
    | Matches [Text]
    | Correct

findSimilarWords :: WordBank -> (Text -> Text -> Int) -> Text -> SimilarityResult
findSimilarWords (WordBank bank) distF word 
    | T.toLower word `elem` bank = Correct
    | otherwise =  do
        let similarities = map (\x -> WordDist x (distF word x)) bank
        let min_dist = getWordSimilarity $ minimum similarities
        -- Dont match if the min difference is too large
        if min_dist > 3 then NoMatches
        else Matches (map getWordStr $ filter (\x -> min_dist == getWordSimilarity x) similarities)

levenDist :: Text -> Text -> Int
levenDist a b
    | T.null b = T.length a
    | T.null a = T.length b
    | T.head a == T.head b = levenDist (T.tail a) (T.tail b)
    | otherwise = do
        let tab = levenDist (T.tail a) b
        let atb = levenDist a (T.tail b)
        let tatb = levenDist (T.tail a) (T.tail b)

        1 + min tab (min atb tatb)

dameLevenDist :: Text -> Text -> Int
dameLevenDist a b = ms ! (T.length a, T.length b)
    where
        m i 0 = i
        m 0 j = j
        m i j
            | T.index a (i - 1) == T.index b (j -1) = ms ! (i - 1, j -1)
            | otherwise = 1 + minimum [
                                    ms ! (i - 1, j),
                                    ms ! (i, j - 1),
                                    ms ! (i - 1, j - 1)
                                    ]


        bounds = ((0, 0), (T.length a, T.length b))
        ms = listArray bounds
            [m i j | (i, j) <- range bounds]