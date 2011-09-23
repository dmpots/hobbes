module DynamoRIO.TraceDumpParser(parser) where

import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Prim
import Data.Word
import DynamoRIO.Trace(Trace(..), Addr, BasicBlock)
import DynamoRIO.ParserHelp(addr, traceId)
import qualified Data.ByteString.Char8 as B

parser :: Parser [Trace]
parser = many trace

traceSep :: Parser String
traceSep = string (take 75 $ repeat '=')

trace :: Parser Trace
trace = do
  spaces
  traceSep
  spaces
  string "TRACE #"
  spaces
  traceNum <- traceId
  spaces
  string "Tag ="
  spaces
  addr
  spaces
  string "Thread ="
  spaces
  many1 digit
  spaces
  blocks <- originalCode
  spaces
  string "Size ="
  spaces
  many1 digit
  spaces
  string "Body:"
  traceBody
  return (Trace traceNum blocks)
  
originalCode :: Parser [BasicBlock]
originalCode =
  between (string "ORIGINAL CODE:") (string "END ORIGINAL CODE") (many1 basicBlock)

basicBlock :: Parser BasicBlock
basicBlock = do
  spaces
  string "basic block # "
  many1 digit
  char ':'
  spaces
  (bbHeader:insns) <- many1 instruction
  return bbHeader

instruction :: Parser Addr
instruction = do
  address  <- addr
  spaces
  code     <- opcode
  spaces
  name     <- mnemonic
  leftOverOpcode
  spaces
  return address

opcode :: Parser [String]
opcode = do
  ops <-  (try $ count 2 hexDigit) `sepEndBy1` (char ' ')
  return ops

leftOverOpcode :: Parser [String]
leftOverOpcode = option [""] (try ((many1 space) >> opcode))

mnemonic :: Parser String
mnemonic = manyTill anyChar (char '\n')

traceBody :: Parser String
traceBody = do
  body <- manyTill anyChar (string "END TRACE")
  char ' '
  many1 digit
  spaces
  return body
