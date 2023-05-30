{-# LANGUAGE RankNTypes #-}
module DDHC.Annihilation.Checks
    ( Checks (..)
    , State (..)
    , Result (..)
    , tdfWithoutDef
    ) where

import DDLewd
import DDHC.Annihilation.Collection
import DDHC.Annihilation.Error
import Data.Functor ((<&>))

data Checks a
    = Checks { check :: forall r. State -> Reject r -> Advice r -> Accept a r -> Result r
             }

type Reject r = State -> Error -> Result r
type Advice r = State -> Warn -> Result r
type Accept a r = State -> a -> Result r

data State
    = State { stColle :: Colle
            , stErrs  :: [Error]
            , stWarns :: [Warn]
            } deriving Show

data Result r
    = Failure State Error
    | Warning State Warn
    | Success State r
    deriving Show

instance Functor Checks where
    fmap f (Checks x) =
        Checks $ \s r ad a ->
            let a' s' x' = a s' (f x')
             in x s r ad a'

instance Applicative Checks where
    pure x = Checks $ \s _ _ a -> a s x
    f <*> x = do
        f' <- f
        x' <- x
        return $ f' x'

instance Alternative Checks where
    empty = Checks $ \s r _ _ -> r s AlternativeEmpty
    (Checks x) <|> (Checks y) =
        Checks $ \s r ad a ->
            let r' s'@(State _ errs _) x' =
                    y (s' { stErrs = x':errs }) r ad a
                ad' s'@(State _ _ wrns) x' =
                    y (s' { stWarns = x':wrns }) r ad a
             in x s r' ad' a

instance Monad Checks where
    (Checks m) >>= f =
        Checks $ \s r ad a ->
            let a' s' x = check (f x) s' r ad a
             in m s r ad a'

tdfWithoutDef :: Checks ()
tdfWithoutDef =
    Checks $ \s@(State c@(Colle _ _ _ tf _ fd _) err _) r _ a ->
        last $ (\(nm, _) -> case nm `elem` (map fst fd) of
                                 True -> a s ()
                                 False -> r s $ FoundTDFNotDefined nm) <$> tf

patMismatch :: Checks ()
patMismatch =
    Checks $ \s@(State c@(Colle _ _ _ _ _ fd _) err _) r _ a ->

--     = Colle { typeSynonyms :: [(TypeName, ([TypeVar], Type))]
--             , typeData     :: [(TypeName, ([TypeVar], [Constr]))]
--             , typeConstr   :: [(ConstrName, (TypeName, [Type]))]
--             , typeFunc     :: [(Id, Type)]
--             , typeOp       :: [(Id, Type)]
--             , funcDefs     :: [Id]
--             , opInfixity   :: [(Id, (Assoc, Int))]
--             } deriving Show
