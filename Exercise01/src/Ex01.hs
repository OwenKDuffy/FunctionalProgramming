{- duffyow@tcd.ie Owen Duffy -}
-- 16325830
module Ex01 where
import Data.List ((\\))

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !

type Id = String

data Expr
  = Val Double
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Dvd Expr Expr
  | Var Id
  | Def Id Expr Expr
  deriving (Eq, Show)

type Dict k d  =  [(k,d)]

define :: Dict k d -> k -> d -> Dict k d
define d s v = (s,v):d

find :: Eq k => Dict k d -> k -> Maybe d
find []             _                 =  Nothing
find ( (s,v) : ds ) name | name == s  =  Just v
                         | otherwise  =  find ds name

type EDict = Dict String Double

-- Part 1 : Evaluating Expressions -- (63 marks) -------------------------------

-- Implement the following function so all 'eval' tests pass.

-- eval should return Nothing if:
  -- (1) a divide by zero operation was going to be performed;
  -- (2) the expression contains a variable not in the dictionary.

eval :: EDict -> Expr -> Maybe Double
eval d e = case simp d e of
    Val a -> Just a
    otherwise -> Nothing

-- Part 2 : Simplifying Expressions -- (57 marks) ------------------------------

-- Given the following code :

simp :: EDict -> Expr -> Expr
simp d (Var v)        =  simpVar d v
simp d (Add e1 e2)    =  simpAdd d   (simp d e1) (simp d e2)
simp d (Sub e1 e2)    =  simpSub d   (simp d e1) (simp d e2)
simp d (Mul e1 e2)    =  simpMul d   (simp d e1) (simp d e2)
simp d (Dvd e1 e2)    =  simpDvd d   (simp d e1) (simp d e2)
simp d (Def v e1 e2)  =  simpDef d v (simp d e1) (simp d e2)
simp _ e = e  -- simplest case, Val, needs no special treatment

-- Implement the following functions so all 'simp' tests pass.

  -- (1) see test scripts for most required properties
  -- (2) (Def v e1 e2) should simplify to just e2 in the following two cases:
    -- (2a) if there is mention of v in e2
    -- (2b) if any mention of v in e2 is inside another (Def v .. ..)

simpVar :: EDict -> Id -> Expr
simpVar d v = case find d v of
    Nothing -> Var v
    Just a -> Val a

simpAdd :: EDict -> Expr -> Expr -> Expr
simpAdd d e1 (Val 0) = e1
simpAdd d (Val 0) e1 = e1
simpAdd d (Val a) (Val b) = Val (a + b)
simpAdd d e1 e2 = Add e1 e2

simpSub :: EDict -> Expr -> Expr -> Expr
simpSub d e1 (Val 0) = e1
simpSub d (Val a) (Val b) = Val (a - b)
simpSub d e1 e2 = Sub e1 e2

simpMul :: EDict -> Expr -> Expr -> Expr
simpMul d e1 (Val 1) = e1
simpMul d (Val 1) e1 = e1
simpMul d (Val a) (Val b) = Val (a * b)
simpMul d e1 e2 = Mul e1 e2

simpDvd :: EDict -> Expr -> Expr -> Expr
simpDvd d e1 (Val 1) = e1
simpDvd d e1 (Val 0) = Dvd e1 (Val 0)
simpDvd d (Val a) (Val b) = Val (a / b)
simpDvd d e1 e2 = Dvd e1 e2

simpDef :: EDict -> Id -> Expr -> Expr -> Expr
simpDef d v (Val a) e1 = simp (define d v a) e1
simpDef d v e1 e2 = Def v e1 e2
