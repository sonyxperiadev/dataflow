module DataFlow.Reader {-(readDiagram, readDiagramFile)-} where

import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Text.Parsec.Char (endOfLine)

import DataFlow.Core

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
  s <- anyToken `manyTill` (try $ char '`')
  return $ intercalate "\n" $ map (dropWhile isSpace) $ lines s

inBraces :: Parser t -> Parser t
inBraces inside = do
  spaces
  _ <- char '{'
  spaces
  c <- inside
  spaces
  _ <- char '}'
  spaces
  return c

attr :: Parser (String, String)
attr = do
  key <- identifier
  skipMany1 $ char ' '
  _ <- char '='
  skipMany1 $ char ' '
  value <- try textBlock <|> str
  spaces
  return (key, value)

attrs :: Parser Attributes
attrs = liftM M.fromList $ attr `sepBy` spaces

data BracesDeclaration = AttrDecl (String, String) | NodeDecl Node

-- | Parses the contents of a braced region containing both attributes and
-- | nodes:
--
-- @
-- attr = "value"
-- node {
--   ...
-- }
-- @
attrsAndNodes :: Parser (Attributes, [Node])
attrsAndNodes = do
  decls <- (try attrDecl <|> objDecl) `sepBy` spaces
  let (pairs, nodes) = foldr iter ([], []) decls
  return (M.fromList pairs, nodes)
  where
  attrDecl = liftM AttrDecl attr
  objDecl = liftM NodeDecl node
  iter (AttrDecl a) (as, os) =    (a : as, os    )
  iter (NodeDecl o) (as, os) =  (as,     o : os)

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
  skipMany space
  a <- option M.empty $ inBraces attrs
  spaces
  return $ f id' a

-- Construct a parser for an node without an ID:
--  <keyword> {
--    ...
--  }
attrsNode :: String -> (Attributes -> [Node] -> t) -> Parser t
attrsNode keyword f = do
  _ <- string keyword
  skipMany1 space
  (a, nodes) <- inBraces attrsAndNodes
  spaces
  return $ f a nodes

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

flow :: Parser Node
flow = do
  i1 <- identifier
  skipMany1 space
  arr <- arrow
  skipMany1 space
  i2 <- identifier
  skipMany1 space
  a <- option M.empty $ inBraces attrs
  case arr of
    Back -> return $ Flow i2 i1 a
    Forward -> return $ Flow i1 i2 a

boundary :: Parser Node
boundary = attrsNode "boundary" TrustBoundary

node :: Parser Node
node =
  try boundary
  <|> try function
  <|> try database
  <|> try io
  <|> flow

diagram :: Parser Diagram
diagram = attrsNode "diagram" Diagram

readDiagram :: String -> String -> Either ParseError Diagram
readDiagram = parse diagram

readDiagramFile :: FilePath -> IO (Either ParseError Diagram)
readDiagramFile = parseFromFile diagram
