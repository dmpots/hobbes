module Main (
  main
)
where
import Command
import PapiEvent
import System.Environment
import System.Exit
import System.Directory
import System.FilePath
import System.Posix.Process
import System.Posix.Types
import System.Console.GetOpt
import Text.Printf

main :: IO ()
main = do
  config <- parseOpts
  eContent <- readFile (optEventsFile config)
  cContent <- readFile (optProgramsFile config)
  let events    = PapiEvent.parseEvents (cleanFile $ lines eContent)
  let commands  = Command.parseCommands (cleanFile $ lines cContent)
  runAll config commands events

data Config = Config {
      optProgramsFile :: FilePath
    , optEventsFile   :: FilePath
    , optOutDir       :: FilePath
    , optNumRuns      :: Int
  }

defaultConfig :: Config
defaultConfig = Config {
      optOutDir       = "."
    , optNumRuns      = 3
    , optProgramsFile = ""
    , optEventsFile   = ""
  }

options :: [OptDescr (Config -> Config)]
options =
  [ 
    Option ['o']     ["odir"]
      (ReqArg ((\d opts -> opts { optOutDir = d })) "DIR")
      "output directory"

  , Option ['r']     ["runs"]
      (ReqArg ((\n opts -> opts { optNumRuns = parseInt n })) "INT")
      "number of runs for each program"
  ]

parseInt :: String -> Int
parseInt s =
  case reads s of 
    [(i, [])] -> i
    _         -> error ("Can not parse int option: "++s)

parseOpts :: IO Config
parseOpts = do
  argv <- getArgs
  case getOpt Permute options argv of
    (o,[p,e],[]  ) -> do
      let userConfig = foldl (flip id) defaultConfig o
      let fullConfig = userConfig {optProgramsFile = p, optEventsFile = e}
      config <- expandDirs fullConfig
      checkConfig config
      return config 
    (_,_,errs)  -> do 
      putStrLn (concat errs ++ usageInfo header options)
      exitFailure
  where header = "Usage: papi-collect [OPTION...] programFile eventsFile"

expandDirs :: Config -> IO Config
expandDirs config = do
  odir <- expandPath (optOutDir config)
  return config {optOutDir = odir }

expandPath :: FilePath -> IO FilePath
expandPath path =
  case path of
    ('/':_) -> return path
    _       -> getCurrentDirectory >>= (\d -> return $ d </> path)

checkConfig :: Config -> IO ()
checkConfig config = do
  check doesFileExist (optProgramsFile config) (notThere (optProgramsFile config))
  check doesFileExist (optEventsFile config) (notThere (optEventsFile config))
  check doesDirectoryExist (optOutDir config) (notThere (optOutDir config))
  where
  check :: (String -> IO Bool) -> String -> String -> IO ()
  check action file msg = do
    ok <- action file
    if ok then 
        return ()
      else 
        putStrLn msg >> exitFailure
  notThere f = "ERROR: "++(f)++ " does not exist"


cleanFile :: [String] -> [String]
cleanFile []             = []
cleanFile ("":rest)      = cleanFile rest
cleanFile (('#':_):rest) = cleanFile rest
cleanFile (line:rest)    = line : cleanFile rest

statsFile :: FilePath -> ProcessID -> String -> Int -> FilePath
statsFile baseName pid ext seqNum = fileName
  where fileName = printf "%s.%d.%03d.%s" baseName intPid seqNum ext
        intPid   = toInteger pid

rtsStatsArg :: FilePath -> String
rtsStatsArg file = "+RTS -s" ++ file ++ " -RTS"

runAll :: Config -> [Command] -> [[PapiEvent]] -> IO ()
runAll config commands eventSets = do
  mapM_ (\command ->
    mapM_ (\events -> runNTimes config command events) eventSets) commands

runNTimes :: Config -> Command -> [PapiEvent] -> IO ()
runNTimes config command events = do 
  pid <- getProcessID
  let outDirPath = optOutDir config
  let progName   = name command
  let outFile n  = statsFile (outDirPath</>"__PAPI."++progName) pid "stats" n
  let count      = optNumRuns config
  mapM_ (\n -> runChecked command events (outFile n)) [1 .. count]

runChecked :: Command -> [PapiEvent] -> FilePath -> IO ()
runChecked command events outFile = do
  rc <- runCommand commandWithArgs
  case rc of
    ExitSuccess   -> return ()
    ExitFailure _ -> 
      putStrLn  ("Command: "++(name command)++" failed") >> exitFailure
  where
    commandWithArgs = command {cmd = (cmd command) ++ papiArgs ++ outArgs}
    papiArgs = papiRtsArgs events
    outArgs  = rtsStatsArg outFile

