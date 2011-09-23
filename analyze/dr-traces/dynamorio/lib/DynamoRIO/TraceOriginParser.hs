module DynamoRIO.TraceOriginParser(parser) where

import Data.Word
import Numeric
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.String
import DynamoRIO.Trace
import DynamoRIO.ParserHelp(addr, traceId)

parser :: Parser [Trace]
parser = many1 trace

trace :: Parser Trace
trace = do
  string "Trace "
  traceNum <- traceId
  blocks   <- many1 block
  return $ Trace traceNum blocks

block :: Parser BasicBlock
block = do
  spaces
  string "bb"
  spaces
  many1 digit
  spaces
  char '='
  spaces
  a <- addr
  spaces
  return a


