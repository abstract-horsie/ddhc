module DDHC.Scanner.Token
    ( Token (..)
    , RawToken (..)
    , Numeric (..)
    ) where

import DDLewd

data Token
    = Identifier String
    | InfixId String
    | TypeId String
    | Operator String
    | Integer Numeric
    | DigitNum Int
    -- | Float String
    | Char Char
    | String String

    -- Primative Type Tokens
    | IntPrim  -- Signed Int   || x | x <- Z
    | WordPrim -- Unsigned Int || x | x <- N_0

    | CaseStmt
    | ClassDef
    | DataDef
    | DefaultDef
    | DerivingDecl
    | DoNotation
    | ElseStmt
    | ForeignReq
    | ForallTypes
    | IfStmt
    | ImportReq
    | InStmt
    | InfixDecl
    | InfixLDecl
    | InfixRDecl
    | InstanceDecl
    | LetStmt
    | ModuleDecl
    | NewtypeDef
    | OfStmt
    | ThenStmt
    | TypeDef
    | WhereDecl
    | Wildcard

    | Assignment
    | TypeOf
    | Arrow
    | CtxArrow
    | Lambda
    | Pipe

    | ParenOpen
    | ParenClose

    | ListOpen
    | ListClose

    | BraceOpen
    | BraceClose

    | SemiColon
    | Comma

    -- Layout tokens
    | Indent Int
    | Newline
    deriving (Show, Eq)

data RawToken
    = NonGraphical
    | Comment
    | Token Token
    deriving (Show, Eq)

data Numeric
    = Hexit String
    | Octit String
    | Digit String
    deriving (Show, Eq)
