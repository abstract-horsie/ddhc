module DDHC.Cataclysm.Catalyst
    ( Catalyst (..)
    , accumilate
    ) where

import DDLewd
import DDHC.Parser.AST
import Data.List (foldl')

data Catalyst
    = Catalyst { typeSynonyms :: [(TypeName, ([TypeVar], Type))]
               , dataTypes    :: [(TypeName, ([TypeVar], [Constr]))]
               , constructors :: [(ConstrName, (TypeName, [Type]))]
               , funcTypes    :: [(Id, Type)]
               , funcDefs     :: [(Id, [Pat])]
               , funcExprs    :: [(Id, Expr)]
               , infixities   :: [(Id, (Assoc, Int))]
               } deriving Show

accumilate :: AST -> Catalyst
accumilate (AST defs) =
    foldl' react emptyCatalyst defs
        where
            react acc@(Catalyst ts dt cs ft fd fe is) x =
                case x of
                  TypeSynonym nm vs t ->
                      acc { typeSynonyms = (nm, (vs, t)):ts }
                  DataDef nm vs cs ->
                      acc { dataTypes = (nm, (vs, cs)):dt }
                  FuncDef (nm ::: t) ->
                      acc { funcTypes = (nm, t):ft }
                  FuncDef (Func nm vs e) ->
                      acc { funcDefs  = (nm, vs):fd
                          , funcExprs = (nm, e):fe
                          }
                  Infixity a i op ->
                      acc { infixities = (op, (a, i)):is }

emptyCatalyst :: Catalyst
emptyCatalyst = Catalyst [] [] [] [] [] [] []
