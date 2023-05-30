{-# LANGUAGE LambdaCase #-}
module DDHC.Cataclysm.Analysis
    ( Analysis (..)
    , State (..)
    , fnTypeNoDef
    ) where

import DDLewd
import DDHC.Cataclysm.Catalyst
import DDHC.Parser.AST.Expr
import DDHC.Parser.AST.Type
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Functor ((<&>))

data Analysis
    = Analysis { analyze :: State -> State }

type Error = String
type Warn = String

data State
    = State { stCatalyst :: Catalyst
            , stErrors   :: [Error]
            } deriving Show

fnTypeNoDef :: Analysis
fnTypeNoDef =
    Analysis $ \s@(State (Catalyst _ _ _ ft fd _ _) err) ->
        let tns = fst <$> ft
            fns = fst <$> fd
         in s { stErrors = foldl' (\acc x -> if x `notElem` fns
                                                then x:acc
                                                else acc) err tns
              }

-- fnIncorrectApplication :: Analysis
-- fnIncorrectApplication =
--     Analysis $ \s@(State (Catalyst _ _ _ ft _ fe _) err) ->
--         let applies = mapMaybe (\case
--                                   ApplyExpr x y -> Just $ ApplyExpr x y
--                                   _             -> Nothing) (snd <$> fe)
--             check = all (\(ApplyExpr id args) -> chk id args) applies
--          in if check
--                then s
--                else s { stErrors = "Not Correct":err }
--     where
--         chk (ApplyExpr (IdExpr id) args) = chk_ id args
--         chk (ApplyExpr (OpIdExpr id) args) = chk_ id args
--         chk (ApplyExpr _ _) = undefined
--         chk_ id args =
--             let fnType = lookup id ft
--                 typeChk _ _ False = False
--                 typeChk _ [] True = True
--                 typeChk (t :-> ts) (x:xs) _ = t == (lookup x ft)
--              in typeChk fnType args True

-- data Catalyst
--     = Catalyst { typeSynonyms :: [(TypeName, ([TypeVar], Type))]
--                , dataTypes    :: [(TypeName, ([TypeVar], [Constr]))]
--                , constructors :: [(ConstrName, (TypeName, [Type]))]
--                , funcTypes    :: [(Id, Type)]
--                , funcDefs     :: [(Id, [Pat])]
--                , funcExprs    :: [(Id, Expr)]
--                , infixities   :: [(Id, (Assoc, Int))]
--                } deriving Show


-- fnExprTypeChk :: Analysis
-- fnExprTypeChk =
--     Analysis $ \s@(State (Catalyst _ _ _ ft fd fe _) err) ->
--         let funcName = fst <$> fd
--             funcType = lookup funcName ft
--             funcExpr = lookup funcName fe
--          in if funcType /= Nothing && funcExpr /= Nothing
--                then s { stErrors = (show funcType):err }
--                else error "oops"
