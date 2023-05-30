module Main where

import DDLewd
import DDHC.Scanner
import DDHC.Scanner.Construction
import DDHC.Parser

main :: IO ()
main = do
    putStrLn "parsing..."
    lexeme <- readFile "test.hs"
    let (Success _ ok) = rawToToken <$> scanner raw lexeme
    let pok = parseAST ok
    putStrLn $ show pok
