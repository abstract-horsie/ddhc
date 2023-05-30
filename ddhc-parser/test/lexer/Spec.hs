import Test.Hspec
import Control.Exception (evaluate)

import DDHC.Lexer
import DDHC.Lexer.Position
import DDHC.Lexer.Construction (lexit, LexerState (..))
import qualified DDHC.Lexer.Token as Tok

emptyLS :: Position -> Tok.Token -> String -> LexerState
emptyLS pos tok lo = LexerState "" pos lo False 0 0 [tok]

main :: IO ()
main = hspec $ do
    describe "DDHC.Lexer.lexAll" $ do
        it "character:normal" $ do
            lexit lexAll "'k'"
                `shouldBe`
                    (Right
                        ( [Tok.Char 'k']
                        , LexerState "" (markPos 1 3) "'k'" False 0 0 ([Tok.Char 'k'])
                        )
                    )

        it "character:escaped" $ do
            lexit lexAll "'\\n'"
                `shouldBe`
                    (Right
                        ( [Tok.Char '\n']
                        , LexerState "" (markPos 1 4) "'n\\'" False 0 0 ([Tok.Char '\n'])
                        )
                    )
            lexit lexAll "'\\r'"
                `shouldBe`
                    (Right
                        ( [Tok.Char '\r']
                        , LexerState "" (markPos 1 4) "'r\\'" False 0 0 ([Tok.Char '\r'])
                        )
                    )
            lexit lexAll "'\\t'"
                `shouldBe`
                    (Right
                        ( [Tok.Char '\t']
                        , LexerState "" (markPos 1 4) "'t\\'" False 0 0 ([Tok.Char '\t'])
                        )
                    )
            lexit lexAll "'\\\\'"
                `shouldBe`
                    (Right
                        ( [Tok.Char '\\']
                        , LexerState "" (markPos 1 4) "'\\\\'" False 0 0 ([Tok.Char '\\'])
                        )
                    )

        it "string:normal" $ do
            lexit lexAll "\"hello world\""
                `shouldBe`
                    (Right
                        ( [Tok.String "hello world"]
                        , LexerState "" (markPos 1 13) "\"dlrow olleh\"" False 0 0
                                     ([Tok.String "hello world"])
                        )
                    )

        it "string:escaped" $ do
            lexit lexAll "\"hello\\\"world\"" -- "hello\"world"
                `shouldBe`
                    (Right
                        ( [Tok.String "hello\"world"]
                        , LexerState "" (markPos 1 14) "\"dlrow\"\\olleh\"" False 0 0
                                     ([Tok.String "hello\"world"])
                        )
                    )

        it "identifier:normal" $ do
            lexit lexAll "identifier"
                `shouldBe`
                    (Right
                        ( [Tok.Identifier "identifier"]
                        , LexerState "" (markPos 1 10) "reifitnedi" False 0 0
                                     ([Tok.Identifier "identifier"])
                        )
                    )
            lexit lexAll "id_under"
                `shouldBe`
                    (Right
                        ( [Tok.Identifier "id_under"]
                        , LexerState "" (markPos 1 8) "rednu_di" False 0 0
                                     ([Tok.Identifier "id_under"])
                        )
                    )
            lexit lexAll "idnum100"
                `shouldBe`
                    (Right
                        ( [Tok.Identifier "idnum100"]
                        , LexerState "" (markPos 1 8) "001mundi" False 0 0
                                     ([Tok.Identifier "idnum100"])
                        )
                    )

        it "identifier:reserved:case" $ do
            lexit lexAll "case"
                `shouldBe`
                    (Right
                        ( [Tok.CaseStmt]
                        , LexerState "" (markPos 1 4) "esac" False 0 0
                                     ([Tok.CaseStmt])
                        )
                    )
            lexit lexAll "of"
                `shouldBe`
                    (Right
                        ( [Tok.OfStmt]
                        , LexerState "" (markPos 1 2) "fo" False 0 0
                                     ([Tok.OfStmt])
                        )
                    )

        it "identifier:reserved:if" $ do
            lexit lexAll "if"
                `shouldBe`
                    (Right
                        ( [Tok.IfStmt]
                        , LexerState "" (markPos 1 2) "fi" False 0 0
                                     ([Tok.IfStmt])
                        )
                    )
            lexit lexAll "then"
                `shouldBe`
                    (Right
                        ( [Tok.ThenStmt]
                        , LexerState "" (markPos 1 4) "neht" False 0 0
                                     ([Tok.ThenStmt])
                        )
                    )
            lexit lexAll "else"
                `shouldBe`
                    (Right
                        ( [Tok.ElseStmt]
                        , LexerState "" (markPos 1 4) "esle" False 0 0
                                     ([Tok.ElseStmt])
                        )
                    )

        it "identifier:reserved:data" $ do
            lexit lexAll "data"
                `shouldBe`
                    (Right
                        ( [Tok.DataDef]
                        , LexerState "" (markPos 1 4) "atad" False 0 0
                                     ([Tok.DataDef])
                        )
                    )
            lexit lexAll "newtype"
                `shouldBe`
                    (Right
                        ( [Tok.NewtypeDef]
                        , LexerState "" (markPos 1 7) "epytwen" False 0 0
                                     ([Tok.NewtypeDef])
                        )
                    )
            lexit lexAll "type"
                `shouldBe`
                    (Right
                        ( [Tok.TypeDef]
                        , LexerState "" (markPos 1 4) "epyt" False 0 0
                                     ([Tok.TypeDef])
                        )
                    )

        it "identifier:reserved:typeclasses" $ do
            lexit lexAll "class"
                `shouldBe`
                    (Right
                        ( [Tok.ClassDef]
                        , LexerState "" (markPos 1 5) "ssalc" False 0 0
                                     ([Tok.ClassDef])
                        )
                    )
            lexit lexAll "deriving"
                `shouldBe`
                    (Right
                        ( [Tok.DerivingDecl]
                        , LexerState "" (markPos 1 8) "gnivired" False 0 0
                                     ([Tok.DerivingDecl])
                        )
                    )
            lexit lexAll "instance"
                `shouldBe`
                    (Right
                        ( [Tok.InstanceDecl]
                        , LexerState "" (markPos 1 8) "ecnatsni" False 0 0
                                     ([Tok.InstanceDecl])
                        )
                    )

        it "identifier:reserved:let_in" $ do
            lexit lexAll "let"
                `shouldBe`
                    (Right
                        ( [Tok.LetStmt]
                        , LexerState "" (markPos 1 3) "tel" False 0 0
                                     ([Tok.LetStmt])
                        )
                    )
            lexit lexAll "in"
                `shouldBe`
                    (Right
                        ( [Tok.InStmt]
                        , LexerState "" (markPos 1 2) "ni" False 0 0
                                     ([Tok.InStmt])
                        )
                    )

        it "identifier:reserved:infix" $ do
            lexit lexAll "infix"
                `shouldBe`
                    (Right
                        ( [Tok.InfixDecl]
                        , LexerState "" (markPos 1 5) "xifni" False 0 0
                                     ([Tok.InfixDecl])
                        )
                    )
            lexit lexAll "infixl"
                `shouldBe`
                    (Right
                        ( [Tok.InfixLDecl]
                        , LexerState "" (markPos 1 6) "lxifni" False 0 0
                                     ([Tok.InfixLDecl])
                        )
                    )
            lexit lexAll "infixr"
                `shouldBe`
                    (Right
                        ( [Tok.InfixRDecl]
                        , LexerState "" (markPos 1 6) "rxifni" False 0 0
                                     ([Tok.InfixRDecl])
                        )
                    )

        it "identifier:reserved:misc" $ do
            lexit lexAll "default"
                `shouldBe`
                    (Right
                        ( [Tok.DefaultDef]
                        , LexerState "" (markPos 1 7) "tluafed" False 0 0
                                     ([Tok.DefaultDef])
                        )
                    )
            lexit lexAll "do"
                `shouldBe`
                    (Right
                        ( [Tok.DoNotation]
                        , LexerState "" (markPos 1 2) "od" False 0 0
                                     ([Tok.DoNotation])
                        )
                    )
            lexit lexAll "foreign"
                `shouldBe`
                    (Right
                        ( [Tok.ForeignReq]
                        , LexerState "" (markPos 1 7) "ngierof" False 0 0
                                     ([Tok.ForeignReq])
                        )
                    )
            lexit lexAll "import"
                `shouldBe`
                    (Right
                        ( [Tok.ImportReq]
                        , LexerState "" (markPos 1 6) "tropmi" False 0 0
                                     ([Tok.ImportReq])
                        )
                    )
            lexit lexAll "module"
                `shouldBe`
                    (Right
                        ( [Tok.ModuleDecl]
                        , LexerState "" (markPos 1 6) "eludom" False 0 0
                                     ([Tok.ModuleDecl])
                        )
                    )
            lexit lexAll "where"
                `shouldBe`
                    (Right
                        ( [Tok.WhereDecl]
                        , LexerState "" (markPos 1 5) "erehw" False 0 0
                                     ([Tok.WhereDecl])
                        )
                    )
            lexit lexAll "_"
                `shouldBe`
                    (Right
                        ( [Tok.Wildcard]
                        , LexerState "" (markPos 1 1) "_" False 0 0
                                     ([Tok.Wildcard])
                        )
                    )

        it "identifier:unused" $ do
            lexit lexAll "_understart"
                `shouldBe`
                    (Right
                        ( [Tok.Identifier "_understart"]
                        , LexerState "" (markPos 1 11) "tratsrednu_" False 0 0
                                     ([Tok.Identifier "_understart"])
                        )
                    )

        it "identifier:prime" $ do
            lexit lexAll "idprime'"
                `shouldBe`
                    (Right
                        ( [Tok.Identifier "idprime'"]
                        , LexerState "" (markPos 1 8) "'emirpdi" False 0 0
                                     ([Tok.Identifier "idprime'"])
                        )
                    )

        it "identifier:mess" $ do
            lexit lexAll "_ev3ry_th1ng'"
                `shouldBe`
                    (Right
                        ( [Tok.Identifier "_ev3ry_th1ng'"]
                        , LexerState "" (markPos 1 13) "'gn1ht_yr3ve_" False 0 0
                                     ([Tok.Identifier "_ev3ry_th1ng'"])
                        )
                    )

        it "operator:reserved" $ do
            lexit lexAll "::"
                `shouldBe`
                    (Right
                        ( [Tok.TypeOf]
                        , LexerState "" (markPos 1 2) "::" False 0 0
                                     ([Tok.TypeOf])
                        )
                    )
            lexit lexAll "="
                `shouldBe`
                    (Right
                        ( [Tok.Assignment]
                        , LexerState "" (markPos 1 1) "=" False 0 0
                                     ([Tok.Assignment])
                        )
                    )
            lexit lexAll "->"
                `shouldBe`
                    (Right
                        ( [Tok.Arrow]
                        , LexerState "" (markPos 1 2) ">-" False 0 0
                                     ([Tok.Arrow])
                        )
                    )

        it "operator:restricted" $ do
            lexit lexAll "("
                `shouldBe`
                    (Right
                        ( [Tok.ParenOpen]
                        , LexerState "" (markPos 1 1) "(" False 0 0
                                     ([Tok.ParenOpen])
                        )
                    )
            lexit lexAll ")"
                `shouldBe`
                    (Right
                        ( [Tok.ParenClose]
                        , LexerState "" (markPos 1 1) ")" False 0 0
                                     ([Tok.ParenClose])
                        )
                    )
            lexit lexAll "["
                `shouldBe`
                    (Right
                        ( [Tok.ListOpen]
                        , LexerState "" (markPos 1 1) "[" False 0 0
                                     ([Tok.ListOpen])
                        )
                    )
            lexit lexAll "]"
                `shouldBe`
                    (Right
                        ( [Tok.ListClose]
                        , LexerState "" (markPos 1 1) "]" False 0 0
                                     ([Tok.ListClose])
                        )
                    )
            lexit lexAll "{"
                `shouldBe`
                    (Right
                        ( [Tok.BraceOpen]
                        , LexerState "" (markPos 1 1) "{" False 0 0
                                     ([Tok.BraceOpen])
                        )
                    )
            lexit lexAll "}"
                `shouldBe`
                    (Right
                        ( [Tok.BraceClose]
                        , LexerState "" (markPos 1 1) "}" False 0 0
                                     ([Tok.BraceClose])
                        )
                    )
            lexit lexAll ";"
                `shouldBe`
                    (Right
                        ( [Tok.SemiColon]
                        , LexerState "" (markPos 1 1) ";" False 0 0
                                     ([Tok.SemiColon])
                        )
                    )
            lexit lexAll ","
                `shouldBe`
                    (Right
                        ( [Tok.Comma]
                        , LexerState "" (markPos 1 1) "," False 0 0
                                     ([Tok.Comma])
                        )
                    )

        it "operator:custom" $ do
            lexit lexAll "<$>"
                `shouldBe`
                    (Right
                        ( [Tok.Operator "<$>"]
                        , LexerState "" (markPos 1 3) ">$<" False 0 0
                                     ([Tok.Operator "<$>"])
                        )
                    )
            lexit lexAll ":!#$%&*+./<=>?@\\^|-~" -- Actually a valid operator
                `shouldBe`
                    (Right
                        ( [Tok.Operator ":!#$%&*+./<=>?@\\^|-~"]
                        , LexerState "" (markPos 1 20) "~-|^\\@?>=</.+*&%$#!:" False 0 0
                                     ([Tok.Operator ":!#$%&*+./<=>?@\\^|-~"])
                        )
                    )

        it "integer:digit" $ do
            lexit lexAll "100"
                `shouldBe`
                    (Right
                        ( [Tok.Integer (Tok.Digit "100")]
                        , LexerState "" (markPos 1 3) "001" False 0 0
                                     ([Tok.Integer (Tok.Digit "100")])
                        )
                    )

        it "integer:octit" $ do
            lexit lexAll "0o1234567"
                `shouldBe`
                    (Right
                        ( [Tok.Integer (Tok.Octit "1234567")]
                        , LexerState "" (markPos 1 9) "7654321o0" False 0 0
                                     ([Tok.Integer (Tok.Octit "1234567")])
                        )
                    )
            lexit lexAll "0O1234567"
                `shouldBe`
                    (Right
                        ( [Tok.Integer (Tok.Octit "1234567")]
                        , LexerState "" (markPos 1 9) "7654321O0" False 0 0
                                     ([Tok.Integer (Tok.Octit "1234567")])
                        )
                    )

        it "integer:hexit" $ do
            lexit lexAll "0x14FF"
                `shouldBe`
                    (Right
                        ( [Tok.Integer (Tok.Hexit "14FF")]
                        , LexerState "" (markPos 1 6) "FF41x0" False 0 0
                                     ([Tok.Integer (Tok.Hexit "14FF")])
                        )
                    )
            lexit lexAll "0X14FF"
                `shouldBe`
                    (Right
                        ( [Tok.Integer (Tok.Hexit "14FF")]
                        , LexerState "" (markPos 1 6) "FF41X0" False 0 0
                                     ([Tok.Integer (Tok.Hexit "14FF")])
                        )
                    )

        -- it "indent/dedent" $ do
        --     lexit lexAll "f\n\t\th\n\tg"
        --         `shouldBe`
        --             (Right
        --                 ( [Tok.Identifier "f", Tok.Identifier "h", Tok.Identifier "g"]
        --                 , LexerState "" (markPos 3 2) "g\t" False 16 8
        --                              ([Tok.Identifier "g", Tok.Dedent, Tok.Identifier "h", Tok.Indent, Tok.Identifier "f"])
        --                 )
        --             )
