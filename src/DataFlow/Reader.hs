module DataFlow.Reader (
    document,
    readDiagram,
    readDiagramFile
) where

import Control.Monad
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Data.Char
import Data.List
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

import DataFlow.Core

commentsAndSpace :: Parser ()
commentsAndSpace = do
  spaces
  skipMany comment
  spaces
  where
  comment = do
    _ <- string "/*"
    _ <- manyTill anyChar (try $ string "*/")
    return ()

identifier :: Parser ID
identifier = do
  first <- letter
  rest <-  many (letter <|> digit <|> char '_')
  return $ first : rest

str :: Parser String
str = do
  -- TODO: Handle escaped characters.
  _ <- char '"'
  s <- many (noneOf "\"\r\n")
  _ <- char '"'
  return s

textBlock :: Parser String
textBlock = do
  _ <- char '`'
  s <- anyToken `manyTill` try (char '`')
  return $ intercalate "\n" $ map (dropWhile isSpace) $ lines s

inBraces :: Parser t -> Parser t
inBraces inside = do
  commentsAndSpace
  _ <- char '{'
  commentsAndSpace
  c <- inside
  commentsAndSpace
  _ <- char '}'
  commentsAndSpace
  return c

attr :: Parser (String, String)
attr = do
  key <- identifier
  skipMany1 $ char ' '
  _ <- char '='
  skipMany1 $ char ' '
  value <- try textBlock <|> str
  commentsAndSpace
  return (key, value)

attrs :: Parser Attributes
attrs = liftM M.fromList $ many (try attr)

-- | Construct a parser for an node with an ID:
--
-- @
-- \<keyword\> \<id\> {
--   ...
-- }
-- @
idAndAttrsNode :: String -> (ID -> Attributes -> t) -> Parser t
idAndAttrsNode keyword f = do
  _ <- string keyword
  skipMany1 space
  id' <- identifier
  f id' <$> option M.empty (try (inBraces attrs))

function :: Parser Node
function = idAndAttrsNode "function" Function

database :: Parser Node
database = idAndAttrsNode "database" Database

io :: Parser Node
io = idAndAttrsNode "io" InputOutput

data FlowType = Back | Forward

arrow :: Parser FlowType
arrow = do
  s <- string "->" <|> string "--" <|> string "<-"
  case s of
    "->" -> return Forward
    "<-" -> return Back
    _ -> fail "Invalid flow statement"

flow :: Parser Flow
flow = do
  i1 <- identifier
  skipMany1 space
  arr <- arrow
  skipMany1 space
  i2 <- identifier
  a <- option M.empty $ try (inBraces attrs)
  case arr of
    Back -> return $ Flow i2 i1 a
    Forward -> return $ Flow i1 i2 a

node :: Parser Node
node = do
  n <- try function
       <|> try database
       <|> io
  commentsAndSpace
  return n

boundary :: Parser RootNode
boundary = do
  _ <- string "boundary"
  inBraces (TrustBoundary <$> attrs <*> many node)

rootNode :: Parser RootNode
rootNode = try (Node <$> node)
           <|> boundary

diagram :: Parser Diagram
diagram =
  string "diagram" *> inBraces (Diagram <$> attrs <*> many (try rootNode) <*> many flow)

document :: Parser Diagram
document = commentsAndSpace *> diagram <* commentsAndSpace

readDiagram :: String -> String -> Either ParseError Diagram
readDiagram = parse document

readDiagramFile :: FilePath -> IO (Either ParseError Diagram)
readDiagramFile = parseFromFile document
