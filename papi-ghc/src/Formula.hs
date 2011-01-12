module Formula(
    Formula(..)
  , Expr(..)
  , eval
  , canEval
)
where

import qualified Data.Set as Set
import qualified Data.Map as Map

{----------------------------------------------------------
 - Data Types
 ---------------------------------------------------------}
data Formula = Formula String Expr 
  deriving (Show)

data Expr =
    Var String
  | Uop String (Double -> Double) Expr
  | Bop String (Double -> Double -> Double) Expr Expr
  | Const Double

instance Show Expr where
  show (Var s)         = s
  show (Uop s _ e)     = s ++ "(" ++ show e ++ ")"
  show (Bop s _ e1 e2) = "("++"("++show e1++")"++s++"("++show e2++")"++")"
  show (Const d)       = show d

type Env = Map.Map String Double

freeVars :: Expr -> Set.Set String
freeVars e = vars e
  where
  vars (Var s)         = Set.singleton s
  vars (Const _)       = Set.empty
  vars (Uop _ _ x)     = vars x
  vars (Bop _ _ e1 e2) = vars e1 `Set.union` vars e2


canEval :: Env -> Formula -> Bool
canEval env (Formula _ x) = 
  (freeVars x) Set.\\ (Map.keysSet env) == Set.empty


eval :: Env -> Expr -> Double
eval env expr = xeval expr
  where
  xeval (Var s)          = 
    case Map.lookup s env of Just d -> d; Nothing -> nan
  xeval (Uop _ op e)     = op (xeval e)
  xeval (Bop _ op e1 e2) = (xeval e1) `op` (xeval e2)
  xeval (Const c)        = c
  nan = 0/0



