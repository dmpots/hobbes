module DynamoRIO.PcSampleParser(parser)
where

import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Prim
import Text.Parsec.String
import Data.Word
import DynamoRIO.Trace(Addr)
import DynamoRIO.PcSample
import qualified DynamoRIO.ParserHelp  as P


parser :: Parser [PcSample]
parser = do
  manyTill anyChar (try (string "PC PROFILING RESULTS"))
  spaces
  s <- many sample
  spaces
  eof
  return s

sample = do
  string "pc="
  pc <- P.addr
  spaces
  string "#="
  cnt <- P.word64
  spaces
  string "in"
  spaces
  loc <- (dynamoLoc <|> traceLoc <|> appLoc)
  spaces
  return  (PcSample (adjustPc pc loc) cnt loc)
  where
    adjustPc pc (TraceLoc offset) = pc - offset
    adjustPc pc _________________ = pc

dynamoLoc :: Parser SampleLoc
dynamoLoc = do
  string "DynamoRIO "
  dispatch <|> interpOrIbl <|> monitor <|> contextSwitch <|> sigOrSys <|> unknown

drLoc :: String -> SampleLoc -> Parser SampleLoc
drLoc s l = string s >> return l
interpOrIbl   = string "in" >>
                  (drLoc "terpreter" InterpLoc <|>
                   drLoc "direct_branch_lookup"  IblLoc)
dispatch      = drLoc "dispatch"   DispatchLoc
monitor       = drLoc "monitor"    MonitorLoc
contextSwitch = drLoc "context switch"  ContextSwitchLoc 
unknown       = string "<SOMEWHERE>" >> manyTill anyChar (char '\n') >> return UnknownLoc
sigOrSys      = char 's' >> 
                (drLoc "ignal handler"   SignalHandlerLoc  <|> 
                 drLoc "yscall handler"   SyscallHandlerLoc)
appLoc = string "the app" >> return AppLoc

traceLoc = do
  string "trace"
  spaces
  char '@'
  P.addr
  spaces
  string "w/ offs"
  spaces
  offs <- P.addr
  return (TraceLoc offs)

test = do
  input <- readFile "_Profile.txt"
  return (runParser parser () "" input)
