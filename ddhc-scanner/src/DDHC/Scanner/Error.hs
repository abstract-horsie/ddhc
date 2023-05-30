module DDHC.Scanner.Error
    ( Errno (..)
    , Error (..)
    , throw
    ) where

import DDLewd
import DDHC.Scanner.Position

data Errno
    = UnexpectedEOF
    | Unexpected Char
    | AlternativeEmpty
    deriving (Show, Eq)

data Error
    = Error Position Errno
    deriving Show

throw :: String -> Position -> Error
throw (x:_) pos = Error pos (Unexpected x)
throw [] pos = Error pos UnexpectedEOF
