{- duffyow Owen Duffy -}
module Ex02 where

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !


-- a binary tree datatype
data Tree k d
  = Br (Tree k d) (Tree k d) k d
  | Leaf k d
  | Nil
  deriving (Eq, Show)

type IntFun = Tree Int Int -- binary tree with integer keys and data

data Expr
  = Val Double
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Abs Expr
  | Sign Expr
   deriving (Eq, Show)



-- Part 1 : Tree Insert -------------------------------

-- Implement:
ins :: Ord k => k -> d -> Tree k d -> Tree k d
ins key value Nil = Leaf key value

ins insertKey insertData (Leaf currentKey currentData)
    | insertKey == currentKey = Leaf insertKey insertData
    | insertKey < currentKey = Br (Leaf insertKey insertData) Nil currentKey currentData
    | insertKey > currentKey = Br Nil (Leaf insertKey insertData) currentKey currentData

ins insertKey insertData (Br left right currentKey currentData)
    | insertKey == currentKey = Br left right insertKey insertData
    | insertKey < currentKey = Br (ins insertKey insertData left) right currentKey currentData
    | insertKey > currentKey = Br left (ins insertKey insertData right) currentKey currentData



-- Part 2 : Tree Lookup -------------------------------

-- Implement:
lkp :: (Monad m, Ord k) => Tree k d -> k -> m d

lkp (Leaf candKey candData) key
    = if key == candKey
        then return candData
        else fail ("Nothing")

lkp (Br left right brKey brData) key
    = if key == brKey
        then return brData
        else if key < brKey
            then lkp left key
        else if key > brKey
            then lkp right key
        else fail ("Nothing")

lkp Nil _ = fail ("Nil Node")
-- lkp _ _ = error "lkp NYI"

-- Part 3 : Instance of Num for Expr

{-
  Fix the following instance for Num of Expr so all tests pass

  Note that the tests expect simplification to be done
  only when *all* Expr arguments are of the form Val v

  Hint 1 :  implementing fromInteger *first* is recommended!
  Hint 2 :  remember that Double is already an instance of Num
-}

instance Num Expr where
  e1 + e2 = case (e1, e2) of
            (Val e1, Val e2) -> Val(e1 + e2)
            (_) -> Add e1 e2
  e1 - e2 = case (e1, e2) of
            (Val e1, Val e2) -> Val(e1 - e2)
            (_) -> Sub e1 e2
  e1 * e2 = case (e1, e2) of
            (Val e1, Val e2) -> Val(e1*e2)
            (_) -> Mul e1 e2
  negate e = case e of
            Val e -> Val(-e)
            (_) -> Sub 0 e
  abs e = case e of
            Val e -> if e < 0 then  Val(-e) else  Val(e)
            (_) -> Abs e
  signum e = case e of
            Val e ->
                if e < 0 then  Val(-1)
                else if e > 0 then  Val(1)
                else  0
            (_) -> Sign e
  fromInteger i = Val(fromInteger(i))
