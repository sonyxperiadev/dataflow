module DataFlow.Reader (readDiagram, readDiagramFile) where

import Text.ParserCombinators.Parsec
import Data.Char
import DataFlow.Core

nameToID :: String -> String
nameToID = filter isLetter . map toLower

identifier :: Parser ID
identifier = do
  first <- letter
  rest <-  many (letter <|> digit <|> char '_')
  return $ first : rest

quoted :: Parser ID
quoted = do
  -- TODO: Handle escaped characters.
  _ <- char '\''
  s <- many (noneOf "'")
  _ <- char '\''
  return s

skipWhitespace :: Parser ()
skipWhitespace = skipMany $ space <|> newline

skipWhitespace1 :: Parser ()
skipWhitespace1 = skipMany $ space <|> newline

inBraces :: Parser t -> Parser t
inBraces inside = do
  skipWhitespace
  _ <- char '{'
  skipWhitespace
  c <- inside
  skipWhitespace
  _ <- char '}'
  skipWhitespace
  return c

idAndNameObject :: String -> (ID -> ID -> t) -> Parser t
idAndNameObject keyword f = do
  _ <- string keyword
  skipMany1 space
  id' <- identifier
  skipMany1 space
  name <- quoted
  skipWhitespace1
  return $ f id' name

function :: Parser Object
function = idAndNameObject "function" Function

database :: Parser Object
database = idAndNameObject "database" Database

io :: Parser Object
io = idAndNameObject "io" InputOutput

data FlowType = Back | Forward

arrow :: Parser FlowType
arrow = do
  s <- string "->" <|> string "--" <|> string "<-"
  case s of
    "->" -> return Back
    "<-" -> return Forward
    _ -> fail "Invalid flow statement"

flow :: Parser Object
flow = do
  i1 <- identifier
  skipMany1 space
  a <- arrow
  skipMany1 space
  i2 <- identifier
  skipMany1 space
  data' <- quoted
  skipMany1 space
  desc <- quoted
  skipWhitespace1
  case a of
    Back -> return $ Flow i2 i1 data' desc
    Forward -> return $ Flow i1 i2 data' desc

boundary :: Parser Object
boundary = do
  _ <- string "boundary"
  skipMany1 space
  name <- quoted
  let id' = nameToID name
  skipMany1 space
  objs <- inBraces objects
  skipWhitespace1
  return $ TrustBoundary id' name objs

object :: Parser Object
object =
  try boundary
  <|> try function
  <|> try database
  <|> try io
  <|> flow

objects :: Parser [Object]
objects = object `sepBy` many (space <|> newline)

namedDiagram :: Parser Diagram
namedDiagram = do
  _ <- string "diagram"
  skipMany1 space
  name <- quoted
  skipMany1 space
  objs <- inBraces objects
  return $ Diagram (Just name) objs

unnamedDiagram :: Parser Diagram
unnamedDiagram = do
  _ <- string "diagram"
  skipMany1 space
  objs <- inBraces objects
  return $ Diagram Nothing objs

diagram :: Parser Diagram
diagram = try unnamedDiagram <|> namedDiagram

readDiagram :: String -> String ->  Either ParseError Diagram
readDiagram = parse diagram

readDiagramFile :: FilePath -> IO (Either ParseError Diagram)
readDiagramFile = parseFromFile diagram
