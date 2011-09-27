module DynamoRIO.TraceDumpParser(parser) where

import Control.Applicative((<$>), (<*>))
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Prim
import Data.Word
import Data.Maybe(catMaybes, mapMaybe)
import DynamoRIO.Trace(
        Trace(..), TraceLayout(..),
        TraceEntry(..), TraceExit(..),
        Addr, BasicBlock, CacheAddr
      )
import qualified DynamoRIO.ParserHelp  as P
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
  traceNum <- P.traceId
  spaces
  string "Tag ="
  spaces
  tag <- P.addr
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
  layout <- traceBody
  return (Trace traceNum tag blocks layout)
  
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
  return (iAddr bbHeader)

data Instruction = Instruction {iAddr :: Addr, iOp :: Opcode, iName :: String}
instruction :: Parser Instruction
instruction = do
  address  <- P.addr
  spaces
  code     <- opcode
  spaces
  name     <- mnemonic
  extraOps <- leftOverOpcode
  spaces
  return (Instruction address (code ++ extraOps) name)

type Opcode = [String]
opcode :: Parser Opcode
opcode = do
  ops <-  (try $ count 2 hexDigit) `sepEndBy1` (char ' ')
  return ops

leftOverOpcode :: Parser [String]
leftOverOpcode = option [""] (try ((many1 space) >> opcode))

mnemonic :: Parser String
mnemonic = manyTill anyChar (char '\n')

traceBody :: Parser TraceLayout
traceBody = do
  entries <- many1 traceEntry
  body    <- many1 instruction
  stubs   <- catMaybes <$> many exitStub
  endTrace
  return (TraceLayout entries (parseTraceExits stubs body))

data Stub = Stub CacheAddr deriving(Eq, Show)
exitStub  :: Parser (Maybe Stub)
exitStub = do
  spaces
  string "-------- exit stub"
  manyTill anyChar (char '\n')
  spaces
  ((Just . Stub) <$> iAddr <$> head <$> many1 instruction) <|> unlinkedStub

unlinkedStub :: Parser (Maybe a)
unlinkedStub  = string "<no stub created since linked>" >> spaces >> return Nothing

endTrace :: Parser ()
endTrace = do
  string "END TRACE"
  char ' '
  many1 digit
  spaces
  return ()

traceEntry :: Parser TraceEntry
traceEntry = do
  spaces
  string "-------- "
  iblEntry <|> prefixEntry <|> normalEntry

iblEntry = do
  string "indirect branch target entry: --------"
  e <- entryInst
  many instruction
  return (IndirectBranchEntry e)

prefixEntry = do
  string "prefix entry: --------"
  e <- entryInst
  many instruction
  return (PrefixEntry e)

normalEntry = do
  string "normal entry: --------"
  NormalEntry <$> entryInst

entryInst :: Parser Addr
entryInst = iAddr <$> (spaces >> instruction)

parseTraceExits :: [Stub] -> [Instruction] -> [TraceExit]
parseTraceExits stubs = mapMaybe (parseTraceExit stubs)

parseTraceExit :: [Stub] -> Instruction -> Maybe TraceExit
parseTraceExit stubs inst =
  case parse (parseExit stubs) "" (iName inst) of
    Right e -> Just e
    Left _  -> Nothing

parseExit stubs = do
  target <- char 'j' >> many letter >> spaces >> char '$' >> P.addr
  spaces
  name <- optionMaybe (between (char '<') (char '>') (many1 (letter <|> char '_')))
  return (mkExit target name)
  where
  mkExit target Nothing = if (Stub target) `elem` stubs then ExitStub target else ExitLinked target
  mkExit target (Just t)= ExitIbl target t
  
test = do
  input <- readFile "_T.txt"
  return (runParser (many1 trace) () "" input)
