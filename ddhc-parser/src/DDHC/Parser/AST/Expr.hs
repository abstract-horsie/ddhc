module DDHC.Parser.AST.Expr
    ( Id
    , Var
    , Func (..)
    , Expr (..)
    , Literal (..)
    , Assoc (..)
    , Pat (..)

    , expr
    , funcType
    , func
    , infixExpr
    ) where

import DDLewd
import DDHC.Parser.AST.Type
import DDHC.Parser.Construction
import DDHC.Scanner.Token
import Control.Applicative (liftA2)

type Id = String
type Var = String

infixr 2 :::

data Func
    = Var ::: Type
    | Func Var [Pat] Expr
    deriving (Show, Eq)

data Expr
    = LitExpr Literal
    | IdExpr Id
    | OpIdExpr Id
    | InfixExpr Assoc Int Id
    | IfExpr Expr Expr Expr
    | LambdaExpr [Var] Expr
    | ApplyExpr Expr [Expr]
    | OpExpr Expr Id Expr
    | NegateExpr Expr
    | CaseExpr Expr [(Pat, Expr)]
    | LetExpr [Func] Expr
    | WhereExpr Expr [Func]
    -- | DoLetExpr [Func]
    -- | DoExpr [Expr]
    deriving (Show, Eq)

data Literal
    = IntLit Int
    | CharLit Char
    | StringLit String
    deriving (Show, Eq)

data Assoc
    = AssocLeft
    | AssocRight
    deriving (Show, Eq)

data Pat
    = WildcardPat
    | PVar Var
    | LitPat Literal
    | ConstrPat ConstrName [Pat]
    deriving (Show, Eq)

funcType :: Parser Func
funcType = (:::) <$> ident ?|? (token TypeOf |? typeSig)

func :: Parser Func
func =
    Func <$> ident
         <*> many (layer1M pat)
         <*> (token Assignment |? expr)

expr :: Parser Expr
expr =  caseExpr
    <|> ifExpr
    <|> lambdaExpr
    <|> infixExpr
    <|> opExpr
    <|> applyExpr
    <|> idExpr
    <|> litExpr

litExpr :: Parser Expr
litExpr = LitExpr <$> lit

lit :: Parser Literal
lit = c <|> s
    where
        c = CharLit <$> char
        s = StringLit <$> str

idExpr :: Parser Expr
idExpr = IdExpr <$> (ident <|> typeid)

infixExpr :: Parser Expr
infixExpr = left <|> right
    where
        left  = (token InfixDecl <|> token InfixLDecl) |? (InfixExpr AssocLeft <$> dignum <*> op)
        right = token InfixRDecl |? (InfixExpr AssocRight <$> dignum <*> op)

ifExpr :: Parser Expr
ifExpr = do
    _ <- token IfStmt
    predicate <- layer1M expr
    true <- layer1M $ token ThenStmt |? expr
    false <- layer1M $ token ElseStmt |? expr
    return $ IfExpr predicate true false
    -- where
    --     ifThen = token ThenStmt |? expr
    --     ifElse = token ElseStmt |? expr

lambdaExpr :: Parser Expr
lambdaExpr = do
    _ <- token Lambda
    vs <- some ident
    x <- token Arrow |? expr
    return $ LambdaExpr vs x

applyExpr :: Parser Expr
applyExpr =
    ApplyExpr <$> idExpr
              <*> some operatable

opExpr :: Parser Expr
opExpr =
    OpExpr <$> operatable
           ?|? op
           ?|? (opExpr <|> operatable)

operatable :: Parser Expr
operatable = litExpr <|> idExpr <|> paren expr

-- | LetExpr [FuncDef] Expr
-- | WhereExpr Expr [FuncDef]

caseExpr :: Parser Expr
caseExpr = token CaseStmt |? (CaseExpr <$> expr ?|? (token OfStmt |++ cases))
    where
        cases = liftA2 (\x y -> (x, y))
                       pat
                       (layer1M $ token Arrow |? expr)

-- letExpr :: Parser Expr
-- letExpr = token LetStmt |? ()

pat :: Parser Pat
pat = std <|> lpat <|> cpat
    where
        std = wild <|> vpat

        wild = token Wildcard $> WildcardPat
        vpat = PVar <$> ident

        lpat = LitPat <$> lit

        cpat = constr <|> emptyCPat <|> paren cpat

        emptyCPat = (`ConstrPat` []) <$> typeid
        constr = ConstrPat <$> typeid
                           <*> many (std <|> lpat <|> emptyCPat <|> paren cpat)
