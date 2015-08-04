module DataFlow.Reader {-(readDiagram, readDiagramFile)-} where

import Control.Monad
import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import DataFlow.Core

identifier :: Parser ID
identifier = do
  first <- letter
  rest <-  many (letter <|> digit <|> char '_')
  return $ first : rest

str :: Parser ID
str = do
  -- TODO: Handle escaped characters.
  _ <- char '"'
  s <- many (noneOf "\"\r\n")
  _ <- char '"'
  return s

skipWhitespace :: Parser ()
skipWhitespace = skipMany $ space <|> newline

whiteSpaceWithNewLine :: Parser ()
whiteSpaceWithNewLine = do
  skipMany space
  skipMany1 newline
  skipMany $ newline <|> space

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

attr :: Parser (String, String)
attr = do
  key <- identifier
  _ <- string " = "
  value <- str
  skipWhitespace
  return (key, value)

attrs :: Parser Attributes
attrs = liftM M.fromList $ attr `sepBy` (many $ space <|> newline)

data BracesDeclaration = AttrDecl (String, String) | ObjectDecl Object

attrsAndObjects :: Parser (Attributes, [Object])
attrsAndObjects = do
  decls <- (try attrDecl <|> objDecl) `sepBy` (many $ space <|> newline)
  let (pairs, objs) = foldr iter ([], []) decls
  return (M.fromList pairs, objs)
  where
  attrDecl = liftM AttrDecl attr
  objDecl = liftM ObjectDecl object
  iter (AttrDecl a) (as, os) =    (a : as, os    )
  iter (ObjectDecl o) (as, os) =  (as,     o : os)

idAndAttrsObject :: String -> (ID -> Attributes -> t) -> Parser t
idAndAttrsObject keyword f = do
  _ <- string keyword
  skipMany1 space
  id' <- identifier
  skipMany space
  a <- option M.empty $ inBraces attrs
  skipWhitespace
  return $ f id' a

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
boundary = do
  _ <- string "boundary"
  skipMany1 space
  (a, objs) <- inBraces attrsAndObjects
  skipWhitespace
  return $ TrustBoundary a objs

object :: Parser Object
object =
  try boundary
  <|> try function
  <|> try database
  <|> try io
  <|> flow

diagram :: Parser Diagram
diagram = do
  _ <- string "diagram"
  skipMany1 space
  (a, objs) <- inBraces $ attrsAndObjects
  return $ Diagram a objs

readDiagram :: String -> String -> Either ParseError Diagram
readDiagram = parse diagram

readDiagramFile :: FilePath -> IO (Either ParseError Diagram)
readDiagramFile = parseFromFile diagram
