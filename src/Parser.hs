module Parser (Parser, parse, parseExpr, parseDefinition, parseStatement) where

import Ast
import Control.Applicative (Alternative (..), some, many)
import Control.Monad (ap)
import Data.Char (isAlpha, isAlphaNum, isSpace, ord)
import Data.List (isPrefixOf)
import Diagnostic

-- A failure records both its location and whether input was committed. Only
-- lexical lookahead backtracks; malformed syntax cannot silently become a prefix.
data Cursor = Cursor {remaining :: String, position :: Position, nesting :: Int}
data Reply a = Parsed a Cursor | Failed Bool Diagnostic
newtype Parser a = Parser {runParser :: Cursor -> Reply a}

instance Functor Parser where
  fmap f p = p >>= pure . f
instance Applicative Parser where
  pure a = Parser (Parsed a)
  (<*>) = ap
instance Monad Parser where
  Parser p >>= f = Parser $ \s -> case p s of
    Failed committed d -> Failed committed d
    Parsed a s' -> case runParser (f a) s' of
      Failed committed d -> Failed (committed || position s' /= position s) d
      success -> success
instance Alternative Parser where
  empty = expected "expression"
  Parser p <|> Parser q = Parser $ \s -> case p s of
    Failed False first -> case q s of
      Failed committed second -> Failed committed (furthest first second)
      success -> success
    result -> result

furthest :: Diagnostic -> Diagnostic -> Diagnostic
furthest a b
  | start (sourceSpan a) > start (sourceSpan b) = a
  | otherwise = b

expected :: String -> Parser a
expected label = Parser $ \s -> Failed False $ Diagnostic "parse.expected"
  ("Expected " ++ label) (SourceSpan (position s) (advance (position s) ' '))

attempt :: Parser a -> Parser a
attempt (Parser p) = Parser $ \s -> case p s of
  Failed _ d -> Failed False d
  result -> result

advance :: Position -> Char -> Position
advance (Position row col) c
  | c == '\n' = Position (row + 1) 1
  | otherwise = Position row (col + if ord c > 0xffff then 2 else 1)

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy label predicate = Parser $ \s -> case remaining s of
  c : rest | predicate c -> Parsed c s {remaining = rest, position = advance (position s) c}
  _ -> runParser (expected label) s

char :: Char -> Parser Char
char c = satisfy (show c) (== c)

string :: String -> Parser String
string = traverse char

space :: Parser ()
space = Parser $ \s -> Parsed () (skip s)
  where
    skip s = case remaining s of
      c : rest | isSpace c -> skip s {remaining = rest, position = advance (position s) c}
      rest | "--" `isPrefixOf` rest ->
        let comment = takeWhile (/= '\n') rest
         in skip s {remaining = drop (length comment) rest,
                    position = foldl advance (position s) comment}
      _ -> s

lexeme :: Parser a -> Parser a
lexeme p = p <* space

symbol :: String -> Parser String
symbol = lexeme . string

reservedWords :: [String]
reservedWords = ["let", "in", "cons", "head", "tail", "isEmpty", "True", "False", "if", "then", "else"]

keyword :: String -> Parser ()
keyword word = lexeme $ attempt $ do
  _ <- string word
  Parser $ \s -> case remaining s of
    c : _ | isAlphaNum c -> runParser (expected ("boundary after " ++ word)) s
    _ -> Parsed () s

identifier :: Parser String
identifier = lexeme $ do
  name <- (:) <$> satisfy "identifier" isAlpha <*> many (satisfy "identifier character" isAlphaNum)
  if name `elem` reservedWords then expected "non-reserved identifier" else pure name

located :: Parser Expr -> Parser Expr
located p = Parser $ \s -> case runParser p s of
  Parsed e s' -> Parsed (At (SourceSpan (position s) (position s')) e) s'
  Failed committed d -> Failed committed d

withDepth :: Parser a -> Parser a
withDepth p = Parser $ \s ->
  if nesting s >= 1000 then Failed True (Diagnostic "limit.parse-depth" "Parser nesting limit exceeded" (SourceSpan (position s) (advance (position s) ' ')))
  else case runParser p s {nesting = nesting s + 1} of
    Parsed value s' -> Parsed value s' {nesting = nesting s}
    Failed committed d -> Failed committed d

parseExpr :: Parser Expr
parseExpr = withDepth $ located (parseIf <|> parseLet <|> parseLambda <|> parseEq)

parseIf :: Parser Expr
parseIf = IfThenElse <$> (keyword "if" *> parseExpr)
  <*> (keyword "then" *> parseExpr) <*> (keyword "else" *> parseExpr)

parseLet :: Parser Expr
parseLet = Let <$> (keyword "let" *> identifier)
  <*> (assignment *> parseExpr) <*> (keyword "in" *> parseExpr)

parseLambda :: Parser Expr
parseLambda = do
  _ <- lexeme (char '\\' <|> char 'λ')
  names <- some identifier
  _ <- symbol "->"
  body <- parseExpr
  pure (foldr Lam body names)

parseAtom :: Parser Expr
parseAtom = located $
      (Num . read <$> lexeme (some (satisfy "digit" (\c -> c >= '0' && c <= '9'))))
  <|> (BoolLit True <$ keyword "True")
  <|> (BoolLit False <$ keyword "False")
  <|> (Cons <$> (keyword "cons" *> withDepth parseAtom) <*> withDepth parseAtom)
  <|> (Head <$> (keyword "head" *> withDepth parseAtom))
  <|> (Tail <$> (keyword "tail" *> withDepth parseAtom))
  <|> (IsEmpty <$> (keyword "isEmpty" *> withDepth parseAtom))
  <|> parseLambda
  <|> (List <$> (symbol "[" *> separated parseExpr <* symbol "]"))
  <|> (symbol "(" *> parseExpr <* symbol ")")
  <|> (Var <$> attempt identifier)
  where
    separated p = ((:) <$> p <*> many (symbol "," *> p)) <|> pure []

chainLeft :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr
chainLeft term op = do
  first <- term
  rest first
  where
    rest left = (do f <- op; right <- term; rest (f left right)) <|> pure left

parseEq, parseAddSub, parseMul, parseApp :: Parser Expr
parseEq = chainLeft parseAddSub (Eq <$ symbol "==")
parseAddSub = chainLeft parseMul ((Add <$ symbol "+") <|> (Sub <$ symbol "-"))
parseMul = chainLeft parseApp (Mul <$ symbol "*")
parseApp = foldl1 App <$> some parseAtom

assignment :: Parser ()
assignment = lexeme $ attempt $ do
  _ <- char '='
  Parser $ \s -> case remaining s of
    '=' : _ -> runParser (expected "single = in definition") s
    _ -> Parsed () s

eof :: Parser ()
eof = Parser $ \s -> case remaining s of
  [] -> Parsed () s
  _ -> runParser (expected "end of statement") s

parseDefinition :: Parser (String, Expr)
parseDefinition = (,) <$> attempt (identifier <* assignment) <*> parseExpr <* eof

parseStatement :: Int -> String -> Either Diagnostic (Maybe Statement)
parseStatement row input = case runParser parser (Cursor input (Position row 1) 0) of
  Parsed result _ -> Right result
  Failed _ diagnostic -> Left diagnostic
  where
    parser = space *> ((Nothing <$ eof) <|> (Just <$> statement <* eof))
    statement = (uncurry Definition <$> parseDefinition) <|> (Expression <$> parseExpr)

-- Compatibility entry point for expression-level consumers.
parse :: Parser a -> String -> Maybe (a, String)
parse p input = case runParser (space *> p) (Cursor input (Position 1 1) 0) of
  Parsed a s -> Just (a, remaining s)
  Failed _ _ -> Nothing
