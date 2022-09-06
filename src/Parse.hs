module Parse where

import Data.Foldable (toList)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Types (toSeq)
import qualified Types as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

--- Basic parsers ---

ws :: Parser ()
ws = L.space
    space1
    (L.skipLineComment ";")
    (L.skipBlockComment "#|" "|#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

symbol :: Text -> Parser Text
symbol = L.symbol ws

int :: Parser Int
int = lexeme L.decimal

float :: Parser Float
float = lexeme L.float

str :: Parser Text
str = lexeme $ pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

ident :: Parser String
ident = lexeme $ (:) <$> oneOf first <*> many (oneOf rest)
    where
        first = ['a'..'z'] ++ ['A'..'Z']
            ++ "!@$%^&*-=_+\\|:,.<>/?"
            ++ "λ"
        rest = first ++ ['0'..'9']

--- Parsers for the value type ---

parseBool, parseInt, parseFloat, parseStr, parseSym :: Parser T.Value
parseBool  = T.Bool <$> (symbol "true" $> True <|> symbol "false" $> False)
parseInt   = T.Int <$> int
parseFloat = T.Float <$> float
parseStr   = T.Str . unpack <$> str
parseSym   = T.Sym <$> ident

parseSeq :: Text -> Text -> Parser T.Value
parseSeq l r = toSeq <$> between (symbol l) (symbol r) (many parseValue)

parseQuote, parseUnquote, parseUnquoteSplicing, parseQuasiquote, parseCommented :: Parser T.Value
parseQuote           = toSeq . (T.Sym "quote"            :) . pure <$> (symbol "'"  *> parseValue)
parseUnquote         = toSeq . (T.Sym "unquote"          :) . pure <$> (symbol "~"  *> parseValue)
parseUnquoteSplicing = toSeq . (T.Sym "unquote-splicing" :) . pure <$> (symbol "~@" *> parseValue)
parseQuasiquote      = toSeq . (T.Sym "quasiquote"       :) . pure <$> (symbol "`"  *> parseValue)
parseCommented       = toSeq . (T.Sym "quasiquote"       :) . pure <$> (symbol "#"  *> parseValue)

parseValue :: Parser T.Value
parseValue = parseBool
    <|> (try parseFloat <|> parseInt)
    <|> parseStr
    <|> parseSym
    <|> parseSeq "(" ")" <|> parseSeq "[" "]" <|> parseSeq "{" "}"
    <|> parseQuote <|> try parseUnquoteSplicing <|> parseUnquote <|> parseQuasiquote
    <|> parseCommented

parseProgram :: String -> String -> Either (ParseErrorBundle Text Void) T.Value
parseProgram path source = parse (ws *> parseValue <* eof) path (pack source)

--- Helper functions ---

readstr :: String -> T.ExceptV T.Value
readstr s = case parseProgram "<stdin>" ("(do " ++ s ++ ")") of
    Left err -> T.throwStr $ concat $ fmtParseError $ errorUnpack err
    Right v  -> return v

errorUnpack :: ParseErrorBundle Text Void -> NonEmpty (SourcePos, Text)
errorUnpack peb = fmap (\(err, pos) -> (pos, pack . parseErrorTextPretty $ err)) . fst $
    attachSourcePos errorOffset (bundleErrors peb) (bundlePosState peb)

fmtParseError :: NonEmpty (SourcePos, Text) -> [String]
fmtParseError = fmap
    (\(pos, msg) -> sourceName pos ++ ":"
                ++ (s . sourceLine) pos ++ ":"
                ++ (s . sourceColumn) pos ++ " "
                ++ (unln . unpack) msg)
    . toList
    where
        s = show . unPos
        unln ""        = ""
        unln ('\n':xs) = ',':' ':unln xs
        unln (x:xs)    = x:unln xs