module DDHC.Scanner
    ( raw
    , rawToToken
    , token
    , scanner
    ) where

import DDLewd
import DDHC.Scanner.Construction
import DDHC.Scanner.Position
import DDHC.Scanner.Token

import Data.Char
import Data.List (foldl')

raw :: Scanner [Positioned RawToken]
raw = some $ position (nonGraphical <|> comment <|> tok)
    where
        nonGraphical = NonGraphical <$ some (satisfy (\x -> isSpace x && x /= '\n'))
        comment = Comment <$
            (string "--" *>
                satisfy (`notElem` ":!#$%&*+./<=>?@\\^|-~") *>
                    many (satisfy (/= '\n'))) -- <* char '\n'
        tok = Token <$> token

rawToToken :: [Positioned RawToken] -> [Positioned Token]
rawToToken = reverse . foldl' translate []
    where
        translate :: [Positioned Token] -> Positioned RawToken -> [Positioned Token]
        translate (ind@(_, Indent _, _):acc) r =
            case r of
              (_, NonGraphical, _)   -> acc
              (_,Comment,_)          -> acc
              (pos, Token tok, pos') -> (pos, tok, pos'):ind:acc
        translate acc r =
            case r of
              (Pos 1 y, NonGraphical, Pos x y') -> (Pos 1 y, Indent x, Pos x y'):acc
              (_, NonGraphical, _)              -> acc
              (_,Comment,_)                     -> acc
              (pos, Token tok, pos')            -> (pos, tok, pos'):acc

token :: Scanner Token
token = prim <|> op <|> symbol <|> ident <|> digit <|> literal <|> newline
    where
        prim =  string "&Int"  $> IntPrim
            <|> string "&Word" $> WordPrim

        literal = intLit <|> stringLit <|> charLit

        intLit = Integer <$> (hexit <|> octit)
        digit = DigitNum . read <$> some (satisfy isDigit)
        hexit = (string "0x" <|> string "0X") *> (Hexit <$> some (satisfy isHexDigit))
        octit = (string "0o" <|> string "0O") *> (Octit <$> some (satisfy isOctDigit))

        stringLit = String <$> (char '"' *> many stringItem <* char '"')
        stringItem = charEsc <|> satisfy (/= '\"')

        charLit = Char <$> (char '\'' *> charItem <* char '\'')
        charItem = charEsc <|> satisfy (/= '\'')

        charEsc =
            char '\\' *> (
                (char 'n'  $> '\n')
            <|> (char 'r'  $> '\r')
            <|> (char 't'  $> '\t')
            <|>  char '\\'
            <|>  char '\''
            <|>  char '\"')

        symbol =
            choice [ char '(' $> ParenOpen
                   , char ')' $> ParenClose
                   , char '[' $> ListOpen
                   , char ']' $> ListClose
                   , char '{' $> BraceOpen
                   , char '}' $> BraceClose
                   , char ';' $> SemiColon
                   , char ',' $> Comma
                   ]

        op = isReservedop <$> some (oneOf ":!#$%&*+./<=>?@\\^|-~")

        ident = iid <|> fid <|> tid
        allowedFollow = satisfy isAlphaNum <|> oneOf "_'"
        startFid = satisfy isLower <|> char '_'
        iid = char '`' *> (InfixId <$> (startFid `follow` allowedFollow)) <* char '`'
        fid = isReservedid <$> (startFid `follow` allowedFollow)
        tid = TypeId <$> (satisfy isUpper `follow` allowedFollow)

        newline = char '\n' $> Newline

isReservedop :: String -> Token
isReservedop str =
    case str of
      "="  -> Assignment
      "::" -> TypeOf
      "->" -> Arrow
      "=>" -> CtxArrow
      "\\" -> Lambda 
      "|"  -> Pipe
      _    -> Operator str

isReservedid :: String -> Token
isReservedid str =
    case str of
      "case"     -> CaseStmt
      "class"    -> ClassDef
      "data"     -> DataDef
      "default"  -> DefaultDef
      "deriving" -> DerivingDecl
      "do"       -> DoNotation
      "else"     -> ElseStmt
      "foreign"  -> ForeignReq
      "forall"   -> ForallTypes
      "if"       -> IfStmt
      "import"   -> ImportReq
      "in"       -> InStmt
      "infix"    -> InfixDecl
      "infixl"   -> InfixLDecl
      "infixr"   -> InfixRDecl
      "instance" -> InstanceDecl
      "let"      -> LetStmt
      "module"   -> ModuleDecl
      "newtype"  -> NewtypeDef
      "of"       -> OfStmt
      "then"     -> ThenStmt
      "type"     -> TypeDef
      "where"    -> WhereDecl
      "_"        -> Wildcard
      _          -> Identifier str
