module DDHC.Scanner.Position
    ( Position (..)
    , posC
    , posL
    ) where

import DDLewd

data Position
    = Pos {-# UNPACK #-} !Int
          {-# UNPACK #-} !Int
    deriving Show

instance Eq Position where
    (==) (Pos x y) (Pos x' y') = (x + y) == (x' + y')

instance Ord Position where
    compare (Pos x y) (Pos x' y') = compare (x + y) (x' + y')
    -- (<=) (Pos x y) (Pos x' y') = (x + y) <= (x' + y')

posC :: Position -> Int -> Position
posC (Pos x y) i = Pos (x + i) y

posL :: Position -> Int -> Position
posL (Pos _ y) i = Pos 1 (y + i)
