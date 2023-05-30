module DDHC.Scanner.Spec.Construction
    ( satisfy
    , char
    , string
    , oneOf
    , choice
    , follow
    , position
    ) where

import DDLewd
import Test.Hspec
import qualified Data.Char as C
import qualified DDHC.Scanner.Construction as S
import qualified DDHC.Scanner.Error as Err
import qualified DDHC.Scanner.Position as Pos

satisfy :: Spec
satisfy =
    context "[DDHC Scanner] - Satisfy" $ do
        it "[LEGAL] - Lower-case letter" $
            S.scanner (S.satisfy C.isLower) "f" `shouldSatisfy`
                (\(S.Success _ x) -> x == 'f')
        it "[LEGAL] - Multiple lower-case letter" $
            S.scanner (some $ S.satisfy C.isLower) "abcdefghijklmnopqrstuvwxyz" `shouldSatisfy`
                (\(S.Success _ x) -> x == "abcdefghijklmnopqrstuvwxyz")

        it "[ILLEGAL] - Lower-case letter" $
            S.scanner (S.satisfy C.isLower) "F" `shouldSatisfy`
                (\(S.Failure _ (Err.Error _ e)) -> e == Err.Unexpected 'F')
        it "[ILLEGAL] - Multiple lower-case letter" $
            S.scanner (some $ S.satisfy C.isLower) "ABCdefghijklmnopqrstuvwxyz" `shouldSatisfy`
                (\(S.Failure _ (Err.Error _ e)) -> e == Err.Unexpected 'A')

char :: Spec
char =
    context "[DDHC Scanner] - Char" $ do
        it "[LEGAL] - Char parsing" $
            S.scanner (S.char '\\') "\\" `shouldSatisfy`
                (\(S.Success _ x) -> x == '\\')

        it "[ILLEGAL] - Char parsing" $
            S.scanner (S.char '\\') "oops" `shouldSatisfy`
                (\(S.Failure _ (Err.Error _ e)) -> e == Err.Unexpected 'o')

string :: Spec
string =
    context "[DDHC Scanner] - String" $ do
        it "[LEGAL] - String parsing" $
            S.scanner (S.string "class") "class" `shouldSatisfy`
                (\(S.Success _ x) -> x == "class")

        it "[ILLEGAL] - String parsing" $
            S.scanner (S.string "data") "daya" `shouldSatisfy`
                (\(S.Failure _ (Err.Error _ e)) -> e == Err.Unexpected 'y')

oneOf :: Spec
oneOf =
    context "[DDHC Scanner] - OneOf" $ do
        it "[LEGAL] - Haskell symbol" $
            S.scanner (S.oneOf ":!#$%&*+./<=>?@\\^|-~") ":" `shouldSatisfy`
                (\(S.Success _ x) -> x == ':')
        it "[LEGAL] - Haskell operator" $
            S.scanner (some $ S.oneOf ":!#$%&*+./<=>?@\\^|-~") "<$>" `shouldSatisfy`
                (\(S.Success _ x) -> x == "<$>")

        it "[ILLEGAL] - Haskell symbol" $
            S.scanner (S.oneOf ":!#$%&*+./<=>?@\\^|-~") "{" `shouldSatisfy`
                (\(S.Failure _ (Err.Error _ e)) -> e == Err.Unexpected '{')
        it "[ILLEGAL] - Haskell operator" $
            S.scanner (some $ S.oneOf ":!#$%&*+./<=>?@\\^|-~") "oops" `shouldSatisfy`
                (\(S.Failure _ (Err.Error _ e)) -> e == Err.Unexpected 'o')

choice :: Spec
choice =
    context "[DDHC Scanner] - Context" $ do
        it "[LEGAL] - O or R? O" $
            S.scanner (S.choice [S.char 'O', S.char 'R']) "O" `shouldSatisfy`
                (\(S.Success _ x) -> x == 'O')
        it "[LEGAL] - O or R? R" $
            S.scanner (S.choice [S.char 'O', S.char 'R']) "R" `shouldSatisfy`
                (\(S.Success _ x) -> x == 'R')

        it "[ILLEGAL] - O or R? X" $
            S.scanner (S.choice [S.char 'O', S.char 'R']) "X" `shouldSatisfy`
                (\(S.Failure _ (Err.Error _ e)) -> e == Err.Unexpected 'X')

follow :: Spec
follow =
    context "[DDHC Scanner] - Follow" $ do
        it "[LEGAL] - a `follow` b = abbb" $
            S.scanner (S.follow (S.char 'a') (S.char 'b')) "abbb" `shouldSatisfy`
                (\(S.Success _ x) -> x == "abbb")

        it "[ILLEGAL] - a `follow` b = baaa?" $
            S.scanner (S.follow (S.char 'a') (S.char 'b')) "baaa" `shouldSatisfy`
                (\(S.Failure _ (Err.Error _ e)) -> e == Err.Unexpected 'b')

position :: Spec
position =
    context "[DDHC Scanner] - Position" $ do
        it "[LEGAL] - Position char" $
            S.scanner (S.position $ S.char 'o') "o" `shouldSatisfy`
                (\(S.Success _ x) -> x == (Pos.Pos 1 1, 'o', Pos.Pos 1 2))
