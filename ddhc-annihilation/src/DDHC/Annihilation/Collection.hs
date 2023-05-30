-- | This module is used to collect information about your source file
-- before initiating the ANNIHILATOR :O. The information gathered by the
-- Collection are:
--   * Type Synonyms;
--   * Type Constructors;
--   * Type Class Definitions;
--   * Type Class Instances;
--   * Type Definitions for Functions (TDF);
--   * Type Definitions for Constructors (TDC);
--   * Function Identifiers;
--   * Operator Identifiers;
--   * Infixity Definitions;
--
-- This information is used by the Type Checker and
-- by the Operator Applicator (Defines operator infixes)
module DDHC.Annihilation.Collection
    ( Colle (..)
    , collect
    ) where

import DDLewd
import DDHC.Parser.AST
import Data.List (foldl')

data Colle
    = Colle { typeSynonyms :: [(TypeName, ([TypeVar], Type))]
            , typeData     :: [(TypeName, ([TypeVar], [Constr]))]
            , typeConstr   :: [(ConstrName, (TypeName, [Type]))]
            , typeFunc     :: [(Id, Type)]
            , typeOp       :: [(Id, Type)]
            , funcDefs     :: [(Id, [Pat])]
            , opInfixity   :: [(Id, (Assoc, Int))]
            } deriving Show

collect :: AST -> Colle
collect (AST defs) =
    foldl' predicate emptyColle defs
        where
            predicate acc@(Colle ts td tc tf to fd oi) x =
                case x of
                  TypeSynonym nm vs t ->
                      acc { typeSynonyms = (nm, (vs, t)):ts }
                  DataDef nm vs cs ->
                      acc { typeData = (nm, (vs, cs)):td }
                  Infixity a i op ->
                      acc { opInfixity = (op, (a, i)):oi }
                  FuncDef (Func nm vs _) ->
                      acc { funcDefs = (nm, vs):fd }
                  FuncDef (nm ::: t) ->
                      acc { typeFunc = (nm, t):tf }

emptyColle :: Colle
emptyColle = Colle [] [] [] [] [] [] []
