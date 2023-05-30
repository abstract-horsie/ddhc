module DDHC.Parser.Error
    ( Errno (..)
    , Error (..)
    , throw
    ) where

import DDLewd
import DDHC.Scanner.Token
-- import DDHC.Scanner.Position
import DDHC.Scanner.Construction (Positioned)

data Errno
    = UnexpectedEOF
    | Unexpected (Positioned Token)
    | LayoutMismatch
    | ExpectedLayout
    | AlternativeEmpty
    deriving Show

data Error
  = Try Errno
  | Catastrophic Errno
  deriving Show

throw :: [Positioned Token] -> Error
throw (t:_) = Try (Unexpected t)
throw [] = Try UnexpectedEOF
