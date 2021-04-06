module Core.Lexer
  ( lex
  ) where

import Data.Either.Combinators (mapLeft)
import Data.Functor (($>))
import Data.Text (Text, pack)
import Data.Void (Void)
import Prelude hiding (lex)
import Text.Megaparsec hiding (Token, token, tokens)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Core.Token (Token(..))

type Parser = Parsec Void Text

-- | Swallow any whitespace or comment.
sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

-- | Swallow space and apply given parser.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

bool :: Parser Token
bool = Bool <$> (true <|> false)
  where
    true  = L.symbol sc "#t" $> True
    false = L.symbol sc "#f" $> False

-- | Read a number, allowing no space between the sign and the digits.
number :: Parser Token
number = Number <$> L.signed (pure ()) (lexeme L.decimal)

-- | Read a symbol.
sym :: Parser Token
sym = Symbol . pack <$> lexeme (try special <|> normal)
  where
    -- Symbol that could be mistaken for a number.
    special = do
      a <- symChar
      notFollowedBy digitChar
      b <- many (alphaNumChar <|> symChar)
      pure $ a : b

    normal = do
      a <- letterChar
      b <- many (alphaNumChar <|> symChar)
      pure $ a : b

    symChar = char '*' <|> char '/' <|> char '+' <|> char '-' <|> char '<' <|> char '>' <|> char '='

-- | Read a char, handling escaped characters.
charLiteral :: Parser Token
charLiteral = Char <$> (L.symbol sc "#\\" *> printChar)

-- | Read a string, handling escaped characters.
stringLiteral :: Parser Token
stringLiteral = String . pack <$> lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))

-- | Match open brace.
openBrace :: Parser Token
openBrace = L.symbol sc "(" $> OpenBrace

-- | Match close brace.
closeBrace :: Parser Token
closeBrace = L.symbol sc ")" $> CloseBrace

-- | Match quote.
quote :: Parser Token
quote = L.symbol sc "'" $> Quote

-- | Match any token.
token :: Parser Token
token = bool <|> try number <|> sym <|> charLiteral <|> stringLiteral <|> openBrace <|> closeBrace <|> quote

-- | Match a series of tokens terminated by EOF.
tokens :: Parser [Token]
tokens = some token <* eof

-- | Lex a program.
lex :: Text -> Either String [Token]
lex = mapLeft errorBundlePretty . runParser tokens "<input>"
