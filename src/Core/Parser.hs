module Core.Parser
  ( parse
  ) where

import Data.Either.Combinators (mapLeft)
import Data.Set (singleton)
import Data.Text (pack)
import Text.Megaparsec hiding (parse, token, tokens, many)
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (bool, some)

import Core.AST
import Core.Transform (Transform, transform)

type Parser = Parsec Void Text

-- | Parse a text and return the AST or a pretty parse error.
parse :: Monad m => Text -> Transform m [Expr]
parse = transform . mapLeft errorBundlePretty . runParser program "<input>"

-- | A program is a sequence of expressions.
program :: Parser [Expr]
program = many expr <* eof

-- | An expression is a symbol, literal, or a form.
expr :: Parser Expr
expr = literal
  <|> Sym <$> sym
  <|> form

-- | A form is a list, with keywords identifying types.
form :: Parser Expr
form = openBrace *> body <* closeBrace
  where
    body = defForm
      <|> ifForm
      <|> letForm
      <|> lambda
      <|> List <$> some expr
      <|> lookAhead closeBrace $> Nil

-- | A definition for a function which takes arguments, or variable which does not.
defForm :: Parser Expr
defForm = keyword "define" *> (funDef <|> varDef)
  where
    funDef = FunDef <$> (openBrace *> userSym) <*> (Args <$> many sym <* closeBrace) <*> some expr
    varDef = VarDef <$> userSym <*> expr

-- | If is a conditional expression with an optional else.
ifForm :: Parser Expr
ifForm = If
  <$> (keyword "if" *> expr)
  <*> expr
  <*> (expr <|> lookAhead closeBrace $> Nil)

-- | Let binds a sequence of name-expression pairs around a sub-program.
letForm :: Parser Expr
letForm = Let <$> (keyword "let" *> openBrace *> many letVar <* closeBrace) <*> some expr

-- | A name-expression pair without an expression defaults the expression to nil.
letVar :: Parser Binding
letVar = letNil <|> letExpr
  where
    letNil  = Binding <$> userSym <*> pure Nil
    letExpr = Binding <$> (openBrace *> userSym) <*> (expr <* closeBrace)

-- | An anonymous function takes a list of symbols and a sequence of expressions.
lambda :: Parser Expr
lambda = LamDef
  <$> (keyword "lambda" *> openBrace *> (Args <$> many userSym) <* closeBrace)
  <*> pure (FreeArgs [])
  <*> some expr

-- | A symbol
sym :: Parser Text
sym = pack <$> lexeme (some symChar)

-- | A user symbol.
userSym :: Parser Text
userSym = pack <$> lexeme (some userSymChar)

-- | A valid character for user symbols.
userSymChar :: Parser Char
userSymChar = C.alphaNumChar
      <|> C.char '*'
      <|> C.char '/'
      <|> C.char '+'
      <|> C.char '-'
      <|> C.char '<'
      <|> C.char '>'
      <|> C.char '='
      <|> C.char '!'
      <|> C.char '?'

-- | A valid symbol character.
symChar :: Parser Char
symChar = userSymChar <|> C.char '#'

-- | Constants.
literal :: Parser Expr
literal = Lit <$> (try bool <|> number <|> try char <|> string)

-- | Fixnums have low limits.
fixnumMin, fixnumMax :: Int
fixnumMin = -536870912
fixnumMax =  536870911

-- | A number is an optional sign and sequence of decimals within boundaries.
number :: Parser Literal
number = do
  n <- try (L.signed (pure ()) (lexeme L.decimal))
  when (n < fixnumMin || n > fixnumMax) $
    -- FIXME Attaches to the following token.
    fancyFailure $ singleton $ ErrorFail "Fixnum too large"
  pure $ Fixnum n

-- | A unicode character.
char :: Parser Literal
char = Char <$> (L.symbol sc "#\\" *> C.printChar)

-- | A sequence of characters.
string :: Parser Literal
string = String . Left . pack <$> lexeme (C.char '\"' *> manyTill L.charLiteral (C.char '\"'))

-- | True or false.
bool :: Parser Literal
bool = Bool <$> (true <|> false)
  where
    true  = L.symbol sc "#t" $> True
    false = L.symbol sc "#f" $> False

-- | An open brace.
openBrace :: Parser ()
openBrace = L.symbol sc "(" $> ()

-- | A close brace.
closeBrace :: Parser ()
closeBrace = L.symbol sc ")" $> ()

sc :: Parser ()
sc = L.space C.space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

keyword :: Text -> Parser Text
keyword = lexeme . C.string
