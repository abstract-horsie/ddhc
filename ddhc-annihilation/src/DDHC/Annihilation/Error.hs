module DDHC.Annihilation.Error
    ( Error (..)
    , Warn (..)
    ) where

import DDLewd

data Error
    = FoundTDFNotDefined String
    | PatternMismatch String
    | AlternativeEmpty
    | SomeError
    deriving Show

data Warn
    = SomeWarn
    deriving Show
