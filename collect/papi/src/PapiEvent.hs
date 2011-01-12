module PapiEvent (
    PapiEvent(..)
  , parseEvents
  , papiRtsArgs 
)
where
import PapiPresetEvent
import Data.Maybe
import Text.Printf

data PapiEvent =
    PapiPreset PapiPresetEvent
  | PapiNative Int
  deriving(Show, Read, Eq, Ord)

-- |Parse the list of events to capture
parseEvents :: [String]       -- ^ The lines to parse
            -> [[PapiEvent]]  -- ^ The list of events found on each line
parseEvents = map parseLine

-- |Return the arguments to the runtime that will collect the events
papiRtsArgs :: [PapiEvent] -- ^ The list of papi events to collect
               -> String   -- ^ The argument to pass to program runtime
                           --   to collect the events
papiRtsArgs events = startRts ++ eventArgs ++ stopRts
  where
  startRts = " +RTS "
  stopRts  = " -RTS "
  eventArgs = concatMap eventArg events
  eventArg (PapiPreset e) = " -a+" ++ show e ++ " "
  eventArg (PapiNative e) = " -a#" ++ printf "0x%x" e ++ " "


parseLine :: String -> [PapiEvent]
parseLine line = 
  if length tokens == length events then
    events
  else
    error $ "Bad PAPI event list\n"
          ++"line: "++line++"\n"
          ++"parse: "++(show mbEvents)
  where
  tokens   = words line
  events   = catMaybes mbEvents
  mbEvents = (map parseEvent tokens)

parseEvent :: String -> Maybe PapiEvent
parseEvent token =
  case mbParsePreset of 
    Just e  -> Just e
    Nothing -> mbParseNative 
  where 
  mbParsePreset =
    case reads token of
      [(t, "")] -> Just (PapiPreset t)
      _         -> Nothing
  mbParseNative =
    case reads token of
      [(t, "")] -> Just (PapiNative t)
      _         -> Nothing


