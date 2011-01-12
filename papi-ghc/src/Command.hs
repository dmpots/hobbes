module Command (
    Command(..)
  , runCommand
  , parseCommands
)
where


import System.Exit
import System.Process hiding(runCommand)
import Text.Regex

data Command = Command {
      name :: String
    , dir  :: FilePath
    , cmd  :: String
    , checked :: Bool -- ^ Do we check the return code for this command
  }

runCommand :: Command -> IO ExitCode
runCommand  command = do
  (_,_,_, p) <- createProcess create
  waitForProcess p
  where
  create = (shell (cmd command)) {cwd = Just (dir command)}


cmdRegex :: Regex
cmdRegex = mkRegex "\\|"

splitLine :: String -> [String]
splitLine = splitRegex cmdRegex 

parseCommands :: [String] -> [Command]
parseCommands commands =
  map parseCommand commands

parseCommand :: String -> Command
parseCommand ('@':line) = parseCommand line   -- make it easy for fibon
parseCommand ('+':line) = (parseCommand line) {checked = False}
parseCommand line =
  case splitLine line of
    [n,d,c] -> Command n d c True
    parse   -> 
      error $ "Bad command \n"
            ++"line: "++line++"\n"
            ++"parse: "++(show parse)
   

