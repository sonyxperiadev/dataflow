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

data BracesDeclaration = AttrDecl (String, String) | ObjectDecl Object

-- | Parses the contents of a braced region containing both attributes and
-- | objects:
--
-- @
-- attr = "value"
-- object {
--   ...
-- }
-- @
attrsAndObjects :: Parser (Attributes, [Object])
attrsAndObjects = do
  decls <- (try attrDecl <|> objDecl) `sepBy` spaces
  let (pairs, objs) = foldr iter ([], []) decls
  return (M.fromList pairs, objs)
  where
  attrDecl = liftM AttrDecl attr
  objDecl = liftM ObjectDecl object
  iter (AttrDecl a) (as, os) =    (a : as, os    )
  iter (ObjectDecl o) (as, os) =  (as,     o : os)

-- | Construct a parser for an object with an ID:
--
-- @
-- \<keyword\> \<id\> {
--   ...
-- }
-- @
idAndAttrsObject :: String -> (ID -> Attributes -> t) -> Parser t
idAndAttrsObject keyword f = do
  _ <- string keyword
  skipMany1 space
  id' <- identifier
  skipMany space
  a <- option M.empty $ inBraces attrs
  spaces
  return $ f id' a

-- Construct a parser for an object without an ID:
--  <keyword> {
--    ...
--  }
attrsObject :: String -> (Attributes -> [Object] -> t) -> Parser t
attrsObject keyword f = do
  _ <- string keyword
  skipMany1 space
  (a, objs) <- inBraces attrsAndObjects
  spaces
  return $ f a objs

function :: Parser Object
function = idAndAttrsObject "function" Function

database :: Parser Object
database = idAndAttrsObject "database" Database

io :: Parser Object
io = idAndAttrsObject "io" InputOutput

data FlowType = Back | Forward

arrow :: Parser FlowType
arrow = do
  s <- string "->" <|> string "--" <|> string "<-"
  case s of
    "->" -> return Forward
    "<-" -> return Back
    _ -> fail "Invalid flow statement"

flow :: Parser Object
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

boundary :: Parser Object
boundary = attrsObject "boundary" TrustBoundary

object :: Parser Object
object =
  try boundary
  <|> try function
  <|> try database
  <|> try io
  <|> flow

diagram :: Parser Diagram
diagram = attrsObject "diagram" Diagram

readDiagram :: String -> String -> Either ParseError Diagram
readDiagram = parse diagram

readDiagramFile :: FilePath -> IO (Either ParseError Diagram)
readDiagramFile = parseFromFile diagram
