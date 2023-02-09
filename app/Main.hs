module Main (main) where

import Lib
import Data.Char (toLower)

main :: IO ()
main = do 
    wordBank <- loadWordBank "words.txt"
    mainLoop wordBank
    
    
mainLoop :: WordBank -> IO ()
mainLoop bank = do 
        putStrLn "\nType in a word ('quit' to quit): "
        word <- getLine 
        loopIfNotQuit word (\x -> do
            let lowercase = map toLower x
            let similarWords = findSimilarWords lowercase bank
            putStrLn $ case similarWords of 
                Correct -> "Spelled correclty"
                NoMatches -> "Way off" 
                Matches matched -> "Maybe you meant one of these: " ++ show matched
            )
    where
        loopIfNotQuit :: String -> (String -> IO ()) -> IO ()
        loopIfNotQuit "quit" _ = putStrLn "Quitting"
        loopIfNotQuit z f = do 
            f z
            mainLoop bank

