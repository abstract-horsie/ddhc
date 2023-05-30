module DDHC.Cataclysm.Typing
    ( typeCheck

    ) where

import DDLewd

import DDHC.Parser.AST.Expr
import DDHC.Parser.AST.Type
import DDHC.Cataclysm.Catalyst
import DDHC.Cataclysm.Analysis

typeCheck :: Analysis
typeCheck =
    Analysis $ \s@(State (Catalyst _ _ _ ftypes fdefs fexprs _) err) ->
        if all (== True) $ map tc ftypes
           then s
           else s { stErrors = "Error Type":err }
    where
        tc :: (Id, Type) -> Bool
        tc (f, t) =
            let fd = lookup f fdefs
                fe = lookup f fexprs
             in tc' t fd fe

        tc' :: Type -> [Pat] -> Expr
        tc' t vs e =
            case e of
              LitExpr x ->
                  case t of
                    IntLit _ -> t == IntPT
                    CharLit _ ->
                    StringLit _ ->

-- data Catalyst
--     = Catalyst { typeSynonyms :: [(TypeName, ([TypeVar], Type))]
--                , dataTypes    :: [(TypeName, ([TypeVar], [Constr]))]
--                , constructors :: [(ConstrName, (TypeName, [Type]))]
--                , funcTypes    :: [(Id, Type)]
--                , funcDefs     :: [(Id, [Pat])]
--                , funcExprs    :: [(Id, Expr)]
--                , infixities   :: [(Id, (Assoc, Int))]
--                } deriving Show

