module Data.Elf.Symtab(
    readSymbolTables
  , findSymbolByAddress
  , ElfSymbolTableEntry(..)
  , SymbolTable
  ) 
where

import Control.Exception(try, ErrorCall)
import qualified Data.ByteString as B
import Data.Elf(Elf, ElfSymbolTableEntry(..), parseElf, parseSymbolTables)
import Data.Word(Word64)
import Data.IntervalMap.FingerTree(IntervalMap)
import qualified Data.IntervalMap.FingerTree   as I
import Data.Foldable(toList)
import Data.List(sort,sortBy, foldl')
import Data.Ord(comparing)
import Debug.Trace as D
import Data.Maybe(listToMaybe)
import Control.Monad(liftM)

type Addr = Word64

data SymbolTable = SymbolTable {
    tab :: (IntervalMap Addr ElfSymbolTableEntry)
  }

instance Show SymbolTable where
  show (SymbolTable t) = show (map steName (toList t))

findSymbolByAddress :: SymbolTable -> Addr -> Maybe (ElfSymbolTableEntry)
findSymbolByAddress symtab addr = 
  listToMaybe $ map snd (I.search addr (tab symtab))

readSymbolTables :: FilePath -> IO (Maybe SymbolTable)
readSymbolTables elfFile = do
  bytes <- B.readFile elfFile
  res <- try (parse bytes) :: IO (Either ErrorCall Elf)
  case res of
    Left  err -> return   Nothing
    Right elf -> return $ Just (makeSymbolTable elf)
  
  where
  -- parseElf calls `error` on error so we lift it into IO to catch the
  -- exception and return nothing to the user
  parse = return . parseElf
    
makeSymbolTable :: Elf -> SymbolTable
makeSymbolTable elf = SymbolTable table
  where 
  order   = (sortBy (comparing steValue)) . (filter nonZero) . concat
  table   = snd $ foldr insertSym (head ordered, I.empty) ordered
  nonZero s = (steValue s) /= 0
  ordered = order symtabs
  symtabs = parseSymbolTables elf
  insertSym sym (prev, t) = 
    (sym, I.insert (I.Interval (steValue sym) (steValue prev - 1)) sym t)
    
sym t a = steName `liftM` findSymbolByAddress t a

