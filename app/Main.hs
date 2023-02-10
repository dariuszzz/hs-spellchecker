module Main (main) where

import Lib
import Data.Char (toLower)

main :: IO ()
main = do 
    wordBank <- loadWordBank "words.txt"
    mainLoop wordBank
    
    
mainLoop :: WordBank -> IO ()
mainLoop bank = do 
        putStrLn "\nType something ('quit' to quit): "
        line <- getLine 
        loopIfNotQuit line (\x -> do
            let lowercaseWords = words $ map toLower x
            let similarities = map (findSimilarWords bank dameLevenDist) lowercaseWords

            let correctedWords = zipWith (curry correctWords) similarities lowercaseWords 
            putStrLn $ "\nCorrected text: " ++ unwords correctedWords
            )
    where
        loopIfNotQuit :: String -> (String -> IO ()) -> IO ()
        loopIfNotQuit "quit" _ = putStrLn "Quitting"
        loopIfNotQuit z f = do 
            f z
            mainLoop bank
        
        correctWords :: (SimilarityResult, String) -> String
        correctWords (Matches matched, _) = head matched 
        correctWords (_, word) = word