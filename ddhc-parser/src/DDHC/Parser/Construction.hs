{-# LANGUAGE RankNTypes, LambdaCase #-}
module DDHC.Parser.Construction
    (
      Parser
    , parser
    , Result (..)
    , satisfy
    , token
    , stPeekLO

    , contraband
    , indent -- DONT USE AS (|+) AND (|++) OBVIOUS SUPERSETS OF INDENT
    , ident
    , infixid
    , typeid
    , op
    , int
    , dignum
    , char
    , str

    , paren

    , stPopLO

    , sepBy
    , sepByI
    , sepBy1

    , layer
    , layer1
    , layer1M

    , ignore

    , (|+)
    , (+|)
    , (|?)
    , (?|)
    , (+|+)
    , (=|=)
    , (=|+)
    , (?|?)
    , (|++)
    ) where

import DDLewd
import DDHC.Scanner.Token
import DDHC.Scanner.Position
import DDHC.Scanner.Construction (Positioned)
import DDHC.Parser.Error
import Data.Functor ((<&>))

import Debug.Trace

newtype Parser a
  = Parser { runParser :: forall r. State -> Accept a r -> Reject r -> Result r
           }

data State
    = State { stInput    :: ![Positioned Token]
            , stLayout   :: ![Layout]
            , stPos      :: !Position
            } deriving (Show, Eq)

data Layout = Explicit
            | Implicit {-# UNPACK #-} !Int
            deriving (Show, Eq)

type Accept a r = State -> a -> Result r
type Reject r   = State -> Error -> Result r

data Result r
    = Success State !r
    | Failure State Error
    deriving Show

instance Functor Parser where
    fmap f (Parser x) =
        Parser $ \s a r ->
            let a' s' x' = a s' (f x')
             in x s a' r

instance Applicative Parser where
    pure x = Parser $ \s a _ -> a s x
    (<*>) x y = x >>= (<$> y)

instance Alternative Parser where
    empty = Parser $ \s _ r -> r s $ Try AlternativeEmpty
    (<|>) (Parser x) (Parser y) =
        Parser $ \s a r ->
            let r' s' x' = case x' of
                             Try _ -> y s a r
                             Catastrophic _ -> r s' x'
             in x s a r'

instance Monad Parser where
    (>>=) (Parser x) f =
        Parser $ \s a r ->
            let a' s' x' = runParser (f x') s' a r
             in x s a' r

instance MonadFail Parser where
    fail _ = empty

parser :: Parser a -> [Positioned Token] -> Result a
parser (Parser x) inp = x (State inp [] (Pos 0 0)) Success Failure

atomicSt :: State -> State
atomicSt s@(State ((_, _, pos):inp) _ _) =
    s { stInput = inp
      , stPos = pos
      }
atomicSt _ = undefined

stPeekLO :: Parser (Maybe Layout)
stPeekLO = Parser $ \s@(State _ ls _) a _ ->
    case ls of
      [] -> a s Nothing
      (l:_) -> a s (Just l)

stPushLO :: Layout -> Parser ()
stPushLO l = Parser $ \s@(State _ ls _) a _ -> a (s { stLayout = l : ls }) ()

stPopLO :: Parser ()
stPopLO = Parser $ \s@(State _ (_:ls) _) a _ -> a (s { stLayout = ls }) ()

{-# INLINE unpos #-}
unpos :: Positioned a -> a
unpos (_,x,_) = x

satisfy :: (Token -> Bool) -> Parser (Positioned Token)
satisfy f =
    Parser $ \s@(State ts _ _) a r ->
        case ts of
          tok@(_,t,_):_ | f t -> a (atomicSt s) tok
          _ -> r s $ throw ts

token :: Token -> Parser (Positioned Token)
token t = traceStack (show t) $ satisfy (== t)
-- token = satisfy . (==)

contraband :: (Token -> Maybe a) -> Parser a
contraband f =
    Parser $ \s@(State ts _ _) a r ->
        case ts of
          tok : _ ->
              case f $ unpos tok of
                Just x -> a (atomicSt s) x
                Nothing -> r s $ throw ts
          [] -> r s $ throw ts

indent :: Parser Int
indent = trace "ind"
    contraband $
        \case
          Indent x -> Just x
          _        -> Nothing

ident :: Parser String
ident = trace "ide"
    contraband $
        \case
          Identifier x -> Just x
          _            -> Nothing

infixid :: Parser String
infixid = trace "infi"
    contraband $
        \case
          InfixId x -> Just x
          _         -> Nothing

typeid :: Parser String
typeid = trace "typi"
    contraband $
        \case
          TypeId x -> Just x
          _        -> Nothing

op :: Parser String
op = trace "opp"
    contraband $
        \case
          Operator x -> Just x
          _          -> Nothing

int :: Parser Numeric
int = trace "int"
    contraband $
        \case
          Integer x -> Just x
          _         -> Nothing

dignum :: Parser Int
dignum =
    contraband $
        \case
          DigitNum x -> Just x
          _          -> Nothing

char :: Parser Char
char = trace "cha"
    contraband $
        \case
          Char x -> Just x
          _      -> Nothing

str :: Parser String
str = trace "str"
    contraband $
        \case
          String x -> Just x
          _        -> Nothing

paren :: Parser a -> Parser a
paren x = token ParenOpen *> x <* token ParenClose

perr :: Error -> Parser a
perr e = Parser $ \s _ r -> r s e

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy x sep = many $ sep *> x

sepByI :: Parser a -> Parser b -> Parser [a]
sepByI x sep = many $ sep *> x

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 x sep = some $ sep |? x

layer :: [Parser a] -> Parser [a]
layer (x:xs) = do
    x' <- start
    xs' <- traverse go xs
    stPopLO
    return $ x' : xs'
        where
            start = do
                _ <- token Newline
                lc <- stPeekLO <&> \case Just Explicit -> 0
                                         Just (Implicit i) -> i
                                         Nothing -> 0
                ln <- indent
                if ln > lc
                   then stPushLO (Implicit ln) *> x
                   else perr (Try LayoutMismatch)
            go p = do
                _ <- token Newline
                lc <- stPeekLO <&> \case Just Explicit -> 0
                                         Just (Implicit i) -> i
                                         Nothing -> 0
                ln <- indent
                if ln == lc
                   then p
                   else perr (Try LayoutMismatch)
layer [] = pure []

layer1 :: Parser a -> Parser a
layer1 x = do
    _ <- token Newline
    lc <- stPeekLO <&> \case Just Explicit -> 0
                             Just (Implicit i) -> i
                             Nothing -> 0
    ln <- indent
    -- trace ("L: " ++ (show (lc )))
    if ln > lc
       then stPushLO (Implicit ln) *> x <* stPopLO
       else perr (Try LayoutMismatch)

layer1M :: Parser a -> Parser a
layer1M x = layer1 x <|> x

ignore :: Parser a -> Parser a
ignore x = stPopLO *> x

infixl 4 |+
infixl 4 |?
infixl 4 ?|?
infixl 4 +|+
infixl 4 =|=
infixl 4 =|+
infixl 4 |++

(|+) :: Parser a -> Parser b -> Parser b
(|+) x y = do
    _ <- x
    _ <- token Newline
    lc <- stPeekLO <&> \case Just Explicit -> 0
                             Just (Implicit i) -> i
                             Nothing -> 0
    ln <- indent
    trace (show (lc, ln)) $ pure ()
    if ln > lc
       then stPushLO (Implicit ln) *> y <* stPopLO
       else perr (Try LayoutMismatch)

(+|) :: Parser a -> Parser b -> Parser a
(+|) x y = do
    x' <- x
    _ <- token Newline
    lc <- stPeekLO <&> \case Just Explicit -> 0
                             Just (Implicit i) -> i
                             Nothing -> 0
    ln <- indent
    if ln > lc
       then x' <$ (stPushLO (Implicit ln) *> y <* stPopLO)
       else perr (Try LayoutMismatch)

(|?) :: Parser a -> Parser b -> Parser b
(|?) x y = (x |+ y) <|> (x *> y)

(?|) :: Parser a -> Parser b -> Parser a
(?|) x y = (x +| y) <|> (x <* y)

(+|+) :: Parser (a -> b) -> Parser a -> Parser b
(+|+) x y = do
    x' <- x
    _ <- token Newline
    lc <- stPeekLO <&> \case Just Explicit -> 0
                             Just (Implicit i) -> i
                             Nothing -> 0
    ln <- indent
    if ln > lc
       then stPushLO (Implicit ln) *> (x' <$> y) <* stPopLO
       else trace (show (lc, ln)) $ perr (Try LayoutMismatch)

(=|=) :: Parser (a -> b) -> Parser a -> Parser b
(=|=) x y = do
    x' <- x
    _ <- token Newline
    lc <- stPeekLO <&> \case Just Explicit -> 0
                             Just (Implicit i) -> i
                             Nothing -> 0
    ln <- indent
    if ln == lc
       then stPushLO (Implicit ln) *> (x' <$> y) <* stPopLO
       else trace "ok =|=" $ perr (Try LayoutMismatch)

(=|+) :: Parser (a -> b) -> Parser a -> Parser b
(=|+) x y = do
    x' <- x
    _ <- token Newline
    lc <- stPeekLO <&> \case Just (Implicit i) -> i
                             _ -> 0
    ln <- indent
    if ln >= lc
       then stPushLO (Implicit ln) *> (x' <$> y) <* stPopLO
       else perr (Try LayoutMismatch)

(?|?) :: Parser (a -> b) -> Parser a -> Parser b
(?|?) x y = (x +|+ y) <|> (x <*> y)

(|++) :: Parser a -> Parser b -> Parser [b]
(|++) x y = do
    _ <- x
    _ <- token Newline
    lc <- stPeekLO <&> \case Just Explicit -> 0
                             Just (Implicit i) -> i
                             Nothing -> 0
    ln <- indent
    _ <- stPushLO $ Implicit ln
    y1 <- if ln > lc
             then y
             else perr (Try LayoutMismatch)

    yx <-
        many $ do
            _ <- token Newline
            ln' <- indent
            if ln' == ln
               then y
               else perr (Try LayoutMismatch)
    _ <- stPopLO

    return $ y1:yx

