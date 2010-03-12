module ClusterElement where


data ProgramClass = 
    Unknown
  | HaskellProgram
  | CProgram
  | NofibGhc
  | SpecGcc
  | SpecIcc
  | ShootoutGhc
  | ShootoutGcc
  | NofibparGhc
  | DphGhc
  | ParallelGhc
  | SpecLlvm
  | NofibGhc_Llvm
  | ShootoutGhc_Llvm
  | ShootoutLlvm
  | NofibparGhc_Llvm
  | DphGhc_Llvm
  | ParallelGhc_Llvm
  deriving (Enum, Ord, Eq, Show, Read)

data GenClusterElement a b = CE { 
    shortName  :: String        -- for printing
  , dataLabel  :: a             -- for validation/printing
  , dataPoint  :: [(b,Double)]  -- b is type of feature, Double is value
} 
type GenCluster a b = [GenClusterElement a b]
type ClusterElement b = GenClusterElement ProgramClass b
type Cluster        b = GenCluster ProgramClass b

instance (Eq b) => Eq (GenClusterElement a b) where
    (==) x y = (dataPoint x) == (dataPoint y)

