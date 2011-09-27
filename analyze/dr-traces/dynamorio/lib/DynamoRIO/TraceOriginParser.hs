module DynamoRIO.TraceOriginParser(parser) where

import Data.Word
import Numeric
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.String
import DynamoRIO.Trace
import qualified DynamoRIO.ParserHelp as P

data OriginTrace = OriginTrace TraceId [BasicBlock] deriving(Show)

parser :: Parser [OriginTrace]
parser = many1 trace

trace :: Parser OriginTrace
trace = do
  string "Trace "
  traceNum <- P.traceId
  blocks   <- many1 block
  return $ OriginTrace traceNum blocks

block :: Parser BasicBlock
block = do
  spaces
  string "bb"
  spaces
  many1 digit
  spaces
  char '='
  spaces
  a <- P.addr
  spaces
  return a


