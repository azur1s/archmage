module Parse where

import Data.Foldable (toList)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Model as M
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

-- | Basic parsers

ws :: Parser ()
ws = L.space
    space1
    (L.skipLineComment ";")
    (L.skipBlockComment "#|" "|#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

symbol :: T.Text -> Parser T.Text
symbol = L.symbol ws

int :: Parser Int
int = lexeme L.decimal

float :: Parser Float
float = lexeme L.float

str :: Parser T.Text
str = lexeme $ T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

ident :: Parser String
ident = lexeme $ (:) <$> oneOf first <*> many (oneOf rest)
    where
        first = ['a'..'z'] ++ ['A'..'Z']
            ++ "!@$%^&*-=_+\\|:,.<>/?"
            ++ "λ"
        rest = first ++ ['0'..'9']

-- | Parsers for the model

parseBool :: Parser M.Cst
parseBool = M.CstBool <$> (symbol "true" $> True <|> symbol "false" $> False)

parseInt :: Parser M.Cst
parseInt = M.CstInt <$> int

parseFloat :: Parser M.Cst
parseFloat = M.CstFloat <$> float

parseStr :: Parser M.Cst
parseStr = M.CstStr <$> str

parseSym :: Parser M.Cst
parseSym = M.CstSym . T.pack <$> ident

parseList :: Parser M.Cst
parseList = M.CstList <$> (symbol "(" *> many parseCst <* symbol ")")

parseList2 :: Parser M.Cst
parseList2 = M.CstList <$> (symbol "[" *> many parseCst <* symbol "]")

parseList3 :: Parser M.Cst
parseList3 = M.CstList <$> (symbol "{" *> many parseCst <* symbol "}")

parseQuote :: Parser M.Cst
parseQuote = do
    cst <- symbol "'" *> parseCst
    return $ M.CstList [M.CstSym "quote", cst]

parseUnquote :: Parser M.Cst
parseUnquote = do
    cst <- symbol "~" *> parseCst
    return $ M.CstList [M.CstSym "unquote", cst]

parseUnquoteSplicing :: Parser M.Cst
parseUnquoteSplicing = do
    cst <- symbol "~@" *> parseCst
    return $ M.CstList [M.CstSym "unquote-splicing", cst]

parseQuasiquote :: Parser M.Cst
parseQuasiquote = do
    cst <- symbol "`" *> parseCst
    return $ M.CstList [M.CstSym "quasiquote", cst]

parseCommented :: Parser M.Cst
parseCommented = do
    cst <- symbol "#" *> parseCst
    return $ M.CstList [M.CstSym "comment", cst]

parseCst :: Parser M.Cst
parseCst = parseBool
    <|> (try parseFloat <|> parseInt)
    <|> parseStr
    <|> parseSym
    <|> parseList <|> parseList2 <|> parseList3
    <|> parseQuote <|> try parseUnquoteSplicing <|> parseUnquote <|> parseQuasiquote
    <|> parseCommented

parseProgram :: String -> String -> Either (ParseErrorBundle T.Text Void) [M.Cst]
parseProgram path source = parse (ws *> many parseCst <* eof) path (T.pack source)

-- | Miscellaneous functions

errorUnpack :: ParseErrorBundle T.Text Void -> NonEmpty (SourcePos, T.Text)
errorUnpack peb = fmap (\(err, pos) -> (pos, T.pack . parseErrorTextPretty $ err)) . fst $
    attachSourcePos errorOffset (bundleErrors peb) (bundlePosState peb)

fmtParseError :: NonEmpty (SourcePos, T.Text) -> [String]
fmtParseError = fmap
    (\(pos, msg) -> sourceName pos ++ ":"
                ++ (s . sourceLine) pos ++ ":"
                ++ (s . sourceColumn) pos ++ " "
                ++ (unln . T.unpack) msg)
    . toList
    where
        s = show . unPos
        unln ""        = ""
        unln ('\n':xs) = ',':' ':unln xs
        unln (x:xs)    = x:unln xs