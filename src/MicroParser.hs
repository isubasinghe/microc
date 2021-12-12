{-# LANGUAGE OverloadedStrings #-}

module MicroParser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import MicroAST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

dquotes :: Parser a -> Parser a
dquotes = between (single '"') (single '"')

squotes :: Parser a -> Parser a
squotes = between (single '\'') (single '\'')

semi :: Parser ()
semi = void $ symbol ";"

comma :: Parser ()
comma = void $ symbol ","

star :: Parser ()
star = void $ symbol "$"

rword :: Text -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [Text]
rws =
  [ "if",
    "then",
    "else",
    "while",
    "true",
    "false",
    "for",
    "int",
    "bool",
    "char",
    "float",
    "void",
    "return",
    "struct",
    "NULL",
    "sizeof"
  ]

strlit :: Parser Text
strlit = do
  content <- dquotes $ takeWhileP Nothing (/= '"')
  -- Hijack haskell's string lexer so we don't have to deal with escaping
  pure $ T.pack (read ('"' : (T.unpack content) ++ "\""))

charlit :: Parser Int
charlit =
  squotes $
    (ord <$> satisfy (`notElem` ['\\', '\'']))
      <|> (single '\\' >> int)

identifier :: Parser Text
identifier = (lexeme . try) (p >>= check)
  where
    p =
      fmap T.pack $
        (:) <$> letterChar
          <*> many (alphaNumChar <|> single '_')
    check x =
      if x `elem` rws
        then fail $ "keyword " <> show x <> " cannot be an identifier"
        else pure x

int :: Parser Int
int = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

opTable :: [[Operator Parser Expr]]
opTable =
  [ [ InfixL $ Access <$ symbol ".",
      InfixL $ (\lhs rhs -> Access (Deref lhs) rhs) <$ symbol "->"
    ],
    [ unary (Unop Neg) "-",
      unary (Unop Not) "!",
      unary Deref "*",
      unary Addr "&",
      Prefix (try $ Cast <$> (parens typeP))
    ],
    [infixR Power "**"],
    [infixL Mult "*", infixL Div "/"],
    [infixL Add "+", infixL Sub "-"],
    [infixL Leq "<=", infixL Geq ">=", infixL Less "<", infixL Greater ">"],
    [infixL' Equal "==", infixL Neq "!="],
    [infixL' BitAnd "&"],
    [infixL' BitOr "|"],
    [infixL' And "&&"],
    [infixL' Or "||"],
    [InfixR $ Assign <$ symbol "="]
  ]
  where
    -- Megaparsec doesn't support multiple prefix operators by default,
    -- but we need this in order to parse things like double negatives,
    -- nots, and dereferences
    unary op sym = Prefix $ foldr1 (.) <$> some (op <$ symbol sym)
    infixL op sym = InfixL $ BinOp op <$ symbol sym
    -- Primed infixL' is useful for operators which are prefixes of other operators
    infixL' op sym = InfixL $ BinOp op <$ operator sym
    infixR op sym = InfixR $ BinOp op <$ symbol sym
    operator sym = lexeme $ try (symbol sym <* notFollowedBy opChar)
    opChar = oneOf ("!#$%&*+./<=>?@\\^|-~" :: String)

termP :: Parser Expr 
termP = try (Cast <$> parent typeP <*> exprP)
        <|> parens exprP 
        <|> Null <$ rword "NULL"
        <|> try (Fliteral <$> float)
        <|> Literal <$> int 
        <|> BoolLit <$> (True <$ rword "true" <|> False <$ rword "false")
        <|> Sizeof <$> (rword "sizeof" *> parens typeP)
        <|> try (Call <$> identifier <*> parens (exprP `sepBy` comma))
        <|> CharLit <$> charlit 
        <|> StrLit <$> strlit 
        <|> Id <$> identifier

exprP :: Parser Expr 
exprP = makeExprParser termP opTable

exprMaybe :: Parser Expr 
exprMaybe :: option Noexpr exprP


structP :: Parser Struct 
struct = Struct <$> (rword "struct" *> identifier) <*> braces (many vdeclP) <* semi

typeP :: Parser Type 
typeP = do 
  baseType <- TyInt <$ rword "int"
          <|> TyBool <$ rword "bool"
          <|> TyFloat <$ rword "float"
          <|> TyChar <$ rword "char"
          <|> TyVoid <$ rword "void"
          <|> TyStruct <$ (rword "struct" *> identifier)
  foldr (const Pointer) baseType <$> many star 


vdeclP :: Parser Bind 
vdeclP = Bind <$> typeP <*> identifier <* semi


statement :: Parser Statement 
statement = Expr <$> exprP <* semi 
          <|> Return <$> (rword "return" *> exprMaybe <* semi)
          <|> Block <$> braces (many statementP)
          <|> ifP 
          <|> forP 
          <|> whileP

ifP :: Parser Statement 
ifP = liftA3 If (rword "if" *> parens exprP) statementP maybeElse 
  where 
    maybeElse = option (Block []) (rword "else" *> statementP)

