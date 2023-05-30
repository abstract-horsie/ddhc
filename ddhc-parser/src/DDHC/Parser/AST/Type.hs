module DDHC.Parser.AST.Type
    ( TypeVar
    , TypeName
    , TypeClass
    , Type (..)
    , Context (..)
    , Forall (..)

    , ConstrName
    , Constr (..)

    , typeSig
    , constrDef
    ) where

import DDLewd
import DDHC.Scanner.Token
import DDHC.Parser.Construction

type TypeVar = String
type TypeName = String
type TypeClass = String

infixr 2 :->
infixr 2 :=>
infixr 2 :.

data Type
    = IntPT
    | WordPT
    | UserT TypeName [Type]
    | TVar TypeVar
    | Type :-> Type
    | Context :=> Type
    | Forall :. Type
    deriving (Show, Eq)

newtype Context
    = Context [(TypeClass, TypeVar)]
    deriving (Show, Eq)

newtype Forall
    = Forall [TypeVar]
    deriving (Show, Eq)

type ConstrName = String

data Constr
    = Constr ConstrName [Type]
    deriving (Show, Eq)

constrDef :: Parser Constr
constrDef =
    Constr <$> typeid
           ?|? many constrType

constrType :: Parser Type
constrType = oneType

typeSig :: Parser Type
typeSig = onlyStart <|> types
    where
        onlyStart = ctxType <|> faType

context :: Parser Context
context = Context <$> (pctx <|> solo)
    where
        solo = (:[]) <$> ctx
        pctx = paren ((:) <$> ctx <*> sepBy ctx (token Comma))
        ctx = (\x y -> (x,y)) <$> typeid ?|? ident

forallTypes :: Parser Forall
forallTypes = Forall <$> (token ForallTypes *> some ident <* token (Operator "."))

oneType :: Parser Type
oneType = intType <|> wordType <|> varType <|> emptyUsrType <|> paren (arrType <|> usrType <|> oneType)

types :: Parser Type
types = arrType <|> oneType

intType :: Parser Type
intType = token IntPrim   $> IntPT

wordType :: Parser Type
wordType = token WordPrim $> WordPT

emptyUsrType :: Parser Type
emptyUsrType = (`UserT` []) <$> typeid

usrType :: Parser Type
usrType = UserT <$> typeid
                ?|? many (typeSig <|> paren types)

varType :: Parser Type
varType = TVar <$> ident

arrType :: Parser Type
arrType = (:->) <$> oneType ?|? (token Arrow |? types)

ctxType :: Parser Type
ctxType = (:=>) <$> context ?|? (token CtxArrow |? types)

faType :: Parser Type
faType = (:.) <$> forallTypes ?|? types

