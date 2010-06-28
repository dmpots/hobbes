module Formula
where


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




