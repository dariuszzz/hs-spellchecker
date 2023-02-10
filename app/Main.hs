{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import qualified Data.Text as T

import Text.Colour
import qualified Data.Text.IO as T
import Data.Text (Text)
import GHC.TypeLits (ErrorMessage(Text))

main :: IO ()
main = do 
    wordBank <- loadWordBank "words.txt"
    mainLoop wordBank
    
    
mainLoop :: WordBank -> IO ()
mainLoop bank = do 
        putStrLn "\nType something ('quit' to quit): "
        line <- T.getLine 
        loopIfNotQuit line (\x -> do
            let wordList = T.words x
            let similarities = map (findSimilarWords bank dameLevenDist) wordList

            let correctedWords = zipWith (curry correctWords) similarities wordList 
            T.putStrLn $ T.pack "\nCorrected text: " `T.append` T.unwords correctedWords
            )
    where
        loopIfNotQuit :: Text -> (Text -> IO ()) -> IO ()
        loopIfNotQuit "quit" _ = T.putStrLn $ T.pack "Quitting"
        loopIfNotQuit z f = do 
            f z
            mainLoop bank
        
        correctWords :: (SimilarityResult, Text) -> Text
        correctWords (Matches matched, _) = renderChunkText With24BitColours $ fore green $ chunk $ head matched 
        correctWords (_, word) = word