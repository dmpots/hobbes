{-# LANGUAGE Arrows #-}
module Main
where
 
import qualified Data.Map  as M
import Data.Maybe
import qualified Data.List as L
import Group
import Text.XML.HXT.Arrow
import System.Environment

type OpcodeGroupPair = (String, String)
 
main :: IO ()
main
    = do
      argv <- getArgs
      (al, src, dst) <- cmdlineOpts argv
      groupList <- runX (application al src dst)
      let groups = collectGroups groupList
      putStrLn "module OpcodeGroup where"
      putStrLn ""
      putStrLn "import Opcodes"
      putStrLn "import OpcodeType"
      putStrLn "opcodeGroup :: Opcode -> OpcodeType"
      putStrLn (unlines $ mapMaybe mkAssignCase groups)
      putStrLn "opcodeGroup _ = Other"
 
-- | the dummy for the boring stuff of option evaluation,
-- usually done with 'System.Console.GetOpt'
cmdlineOpts 	:: [String] -> IO (Attributes, String, String)
cmdlineOpts argv
    = return ([(a_validate, "0"), (a_indent, "1")], argv!!0, argv!!1)
 
-- | the main arrow
application	:: Attributes -> String -> String -> IOSArrow b OpcodeGroupPair
application al src _dst
    = readDocument al src
-- Uncomment to produce a temporary xml document with only relavant data
--      >>>
--      processChildren (processDocumentRootElement `when` isElem)  -- (1)
--      >>>
--      writeDocument al dst
      >>>
      getOpcodeGroups
 
processDocumentRootElement	:: IOSArrow XmlTree XmlTree
processDocumentRootElement = selectOpcodeNodes

selectOpcodeNodes :: IOSArrow XmlTree XmlTree
selectOpcodeNodes =
  deep (isElem >>> hasName "entry" >>> (
          selem  "entry" [
            deep (groupSelector <+> (hasName "mnem"))
          ]
        ))

groupSelector :: ArrowXml a => a XmlTree XmlTree
groupSelector = 
      (hasName "grp1") 
  <+> (hasName "grp2") 
  <+> (hasName "grp3")
  <+> (hasName "instr_ext")

getOpcodeGroups :: IOSArrow XmlTree OpcodeGroupPair
getOpcodeGroups = deep (isElem >>> hasName "entry") >>>
  proc x -> do
    name   <- getText <<< getChildren <<< deep (hasName "mnem") -< x
    group  <-  getText <<< getChildren <<< deep groupSelector   -< x
    returnA -< (name, group)

collectGroups :: [(String, String)] -> [OpcodeGroup]
collectGroups groups = groupList
  where
  groupList  = L.map (uncurry OpcodeGroup) (M.toList nubed)
  --nubed      = M.map (L.nub) condensed
  nubed      = M.map (assignGroup . L.nub) condensed
  condensed  = foldr foldFun (M.empty) groups
  foldFun (opcode, group) = M.insertWith (++) opcode [group]


