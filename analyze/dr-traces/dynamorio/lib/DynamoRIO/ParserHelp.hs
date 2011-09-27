module DynamoRIO.ParserHelp(
    addr
  , traceId
)
where

import DynamoRIO.Trace(Addr, TraceId)
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.String
import Numeric

addr :: Parser Addr
addr = do
  char '0'
  char 'x'
  ds <- many1 hexDigit
  [(hex,"")] <- return (readHex ds)
  return hex

traceId :: Parser TraceId
traceId = do
  traceNum <- many1 digit
  return (read traceNum)

