module DDHC.Parser.AST
    (
      AST (..)
    , Def (..)
    , module DDHC.Parser.AST.Type
    , module DDHC.Parser.AST.Expr

    , ast
    ) where

import DDLewd
import DDHC.Parser.AST.Type
import DDHC.Parser.AST.Expr
import DDHC.Parser.Construction
import qualified DDHC.Scanner.Token as Tok
import Data.Functor ((<&>))

newtype AST = AST [Def]
            deriving (Show, Eq)

ast :: Parser AST
ast = AST <$> (many (token Tok.Newline) *> many (def <* some (token Tok.Newline)))

data Def
    = TypeSynonym TypeName [TypeVar] Type
    | DataDef TypeName [TypeVar] [Constr]
    | Infixity Assoc Int Id
    | FuncDef Func
    deriving (Show, Eq)

def :: Parser Def
def =  typeSynonym
   <|> dataDef
   <|> infixity
   <|> funcDef

typeSynonym :: Parser Def
typeSynonym =
    TypeSynonym <$> (token Tok.TypeDef |? typeid)
                ?|? many (layer1M ident)
                ?|? (token Tok.Assignment |? typeSig)

dataDef :: Parser Def
dataDef =
    DataDef <$> (token Tok.DataDef |? typeid)
            ?|? many (layer1M ident)
            ?|? (token Tok.Assignment |? cdef)
    where
        cdef = (:) <$> constrDef <*> many (layer1M (token Tok.Pipe) |? constrDef)

infixity :: Parser Def
infixity = infixExpr <&> (\(InfixExpr a i f) -> Infixity a i f)

funcDef :: Parser Def
funcDef = FuncDef <$> (func <|> funcType)
