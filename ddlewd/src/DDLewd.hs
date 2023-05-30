module DDLewd
    ( module Prelude
    , Alternative (..)
    , first
    , second
    , ($>)
    ) where

import Prelude
import Control.Applicative (Alternative (..))
import Data.Bifunctor (first, second)
import Data.Functor (($>))
