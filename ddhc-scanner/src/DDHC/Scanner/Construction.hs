{-# LANGUAGE RankNTypes #-}
module DDHC.Scanner.Construction
    ( Scanner (..)
    , State (..)
    , Result (..)
    , scanner
    , satisfy
    , oneOf
    , choice
    , follow
    , char
    , string
    , Positioned
    , position
    ) where

import DDLewd
import DDHC.Scanner.Position
import DDHC.Scanner.Error
import Data.List (foldl1')
import Control.Applicative

newtype Scanner a
  = Scanner { runScanner :: forall r. State -> Accept a r -> Reject r -> Result r
            }

data State
    = State { stInput :: !String
            , stPos   :: !Position
            } deriving Show

type Accept a r = State -> a -> Result r
type Reject r = State -> Error -> Result r

data Result r
    = Success State !r
    | Failure State !Error
    deriving Show

instance Functor Result where
    fmap f (Success s x) = Success s $ f x
    fmap _ (Failure s e) = Failure s e

instance Functor Scanner where
    fmap f (Scanner x) =
        Scanner $ \s a r ->
            let a' s' = a s' . f
             in x s a' r

instance Applicative Scanner where
    pure x = Scanner $ \s a _ -> a s x
    (<*>) x y = do
        f <- x
        y' <- y
        return $ f y'

instance Alternative Scanner where
    empty = Scanner $ \s@(State inp pos) _ r -> r s $ throw inp pos
    -- | '(<|>)' is done differently than a parser might do.
    -- it's defined as if it finds both sides of '(<|>)' return
    -- successfully then it picks one that has traveled the farthest
    -- via checking the equality of the 'Position' inside of the 'State',
    -- as this is quicker than checking the string lengths as the source
    -- input could be huge.
    (<|>) (Scanner x) (Scanner y) = do
        Scanner $ \s a r ->
            let r' _ _ = y s a r
             in x s a r'

instance Monad Scanner where
    (>>=) (Scanner x) f =
        Scanner $ \s a r ->
            let a' s' x' = runScanner (f x') s' a r
             in x s a' r

scanner :: Scanner a -> String -> Result a
scanner (Scanner x) inp = x (State inp (Pos 1 1)) Success Failure

atomicSt :: State -> Char -> State
atomicSt s@(State (_:inp) pos) '\n' =
    s { stInput = inp
      , stPos   = posL pos 1
      }
atomicSt s@(State (_:inp) pos) '\t' =
    s { stInput = inp
      , stPos   = posC pos 8
      }
atomicSt s@(State (_:inp) pos) _ =
    s { stInput = inp
      , stPos   = posC pos 1
      }
atomicSt (State [] _) _ = undefined

satisfy :: (Char -> Bool) -> Scanner Char
satisfy f =
    Scanner $ \s@(State inp pos) a r ->
        case inp of
          c:_ | f c -> a (atomicSt s c) c
          _         -> r s $ throw inp pos

char :: Char -> Scanner Char
char = satisfy . (==)

oneOf :: String -> Scanner Char
oneOf str = satisfy (\c -> c `elem` str)

choice :: Alternative f => [f a] -> f a
choice = foldl1' (<|>)

follow :: Scanner a -> Scanner a -> Scanner [a]
follow x y = liftA2 (:) x (many y)

string :: String -> Scanner String
string = traverse char

type Positioned a = (Position, a, Position)

-- | 'position' takes in a 'Scanner' and adds positions to it.
-- the positions include: the start of the lexeme and the end
-- of the lexeme, formatted as (Position, x, Position) ('Positioned').
position :: Scanner a -> Scanner (Positioned a)
position (Scanner x) =
    Scanner $ \s@(State _ pos) a r ->
        let a' s'@(State _ pos') x'  = a s' (pos, x', pos')
         in x s a' r
