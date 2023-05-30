module Main where

import DDHC.Scanner

main :: IO ()
main = do
    putStrLn "scanning..."
    lexeme <- readFile "test.hs"
    let ok = rawToToken <$> scanner raw lexeme
    putStrLn $ show ok
