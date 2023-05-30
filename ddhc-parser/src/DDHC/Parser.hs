module DDHC.Parser
    ( parseAST
    ) where

import DDHC.Parser.Construction
import DDHC.Parser.AST
import DDHC.Scanner.Token
import DDHC.Scanner.Construction (Positioned)

parseAST :: [Positioned Token] -> Result AST
parseAST = parser ast
