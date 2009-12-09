module Svm where

import ClusterElement
import Data.SVM
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

type Blah = ClusterElement Int


---
---
---
v1 = IntMap.fromList [(1, 0.25), (2, 0.25)]
v2 = IntMap.fromList [(1, 0.5),  (2, 0.75)]
v3 = IntMap.fromList [(1, 0.5),  (2, 0.5)]

p  = [(1.0, v1), (2.0, v2), (2.0, v3), (1.0, v1)]
t  = train (CSvc 1.0) (RBF 1.0) p
d  = t >>= (\m -> return $ predict m v3)
cc  = crossValidate (CSvc 1.0) (RBF 1.0) p 2
  

