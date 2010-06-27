module StatsFile (
    StatsFile(..)
  , toFilePath
  , fromFilePath
)
where
import Text.Printf
import Text.Regex
import Debug.Trace

data StatsFile = StatsFile {
      baseName   :: FilePath   -- ^ The path to the base file name
    , progName   :: String     -- ^ The program name for the stats file
    , uniqueId   :: Integer    -- ^ Unique id to avoid name conflicts
    , ext        :: String     -- ^ File extension
    , eventSetId :: Int        -- ^ Event set number
    , seqNum     :: Int        -- ^ Sequence number
  } deriving (Show, Read)


toFilePath :: StatsFile -> FilePath
toFilePath sf =
  printf "%s#%s.%d.%s.%03d.%s" base prog pid setN seqN ex
  where 
    base = baseName sf
    prog = progName sf
    pid  = uniqueId sf
    seqN = seqNum sf
    setN = encodeEventSet (eventSetId sf)
    ex   = ext sf

fromFilePath :: FilePath -> Maybe StatsFile
fromFilePath path =
  case splitBase path of
    [base, file] -> 
      case splitFile file of
        [prog, pid, setN, seqN, ex] -> 
          mbMakeStatsFile base prog pid ex setN seqN
        _ -> Nothing
    _ -> Nothing


mbMakeStatsFile :: String -> String -> String -> String -> String -> String -> Maybe StatsFile
mbMakeStatsFile base prog pid ex setN seqN = do
  bn <- return base
  pr <- return prog
  pd <- safeParse pid
  e  <- return ex
  tN <- return setN
  qN <- safeParse seqN 
  return (StatsFile bn pr pd e (decodeEventSet tN) qN)
  
safeParse :: Read a => String -> Maybe a
safeParse s =
  case reads s of
  [(a, [])] -> Just a
  _         -> Nothing

baseRegex :: Regex
baseRegex = mkRegex "#"

fileRegex :: Regex
fileRegex = mkRegex "\\."

splitFile :: String -> [String]
splitFile = splitRegex fileRegex

splitBase :: FilePath -> [String]
splitBase = splitRegex baseRegex

encodeEventSet :: Int -> String
encodeEventSet n | n `div` 26 == 0 = [toChar n]
encodeEventSet n | otherwise = 
  toChar (n `mod` 26) : encodeEventSet (n `div` 26)

decodeEventSet :: String -> Int
decodeEventSet []     = 0
decodeEventSet (c:cs) = (fromChar c) + decodeEventSet cs

toChar :: Int -> Char
toChar n = toEnum (n + (fromEnum 'A')) :: Char

fromChar :: Char -> Int
fromChar c = (fromEnum c) - (fromEnum 'A')  


