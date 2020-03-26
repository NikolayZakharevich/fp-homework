module Block5
  ( eval
  , Expr(..)
  , ArithmeticError(..)
  , moving
  , Queue()
  , push
  , pop
  ) where

import Control.Monad.State (State, evalState, modify, state)
import Data.Maybe (fromMaybe)

data Expr
  = Const Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving (Show)

instance Eq Expr where
  Const a == Const b = a == b
  Add l1 r1 == Add l2 r2 = l1 == r1 && l2 == r2
  Sub l1 r1 == Sub l2 r2 = l1 == r1 && l2 == r2
  Mul l1 r1 == Mul l2 r2 = l1 == r1 && l2 == r2
  Div l1 r1 == Div l2 r2 = l1 == r1 && l2 == r2
  Pow l1 r1 == Pow l2 r2 = l1 == r1 && l2 == r2
  _ == _ = False

data ArithmeticError
  = DivisionByZero
  | NegativePower
  deriving (Show)
  
instance Eq ArithmeticError where
    DivisionByZero == DivisionByZero = True
    NegativePower == NegativePower = True
    _ == _ = False

eval :: Expr -> Either ArithmeticError Int
eval (Const x) = Right x
eval binop =
  let (op, x, y) = evalBinop
    in eval x >>= (\x' -> eval y >>= \y' -> op x' y')
  where
    evalBinop =
      case binop of
        Add a b -> (\x y -> Right (x + y), a, b)
        Sub a b -> (\x y -> Right (x - y), a, b)
        Mul a b -> (\x y -> Right (x * y), a, b)
        Div a b ->
          (\x y -> 
            if y /= 0 
            then Right (x `div` y) 
            else Left DivisionByZero, a, b)
        Pow a b ->
          (\x y ->
            if y >= 0
            then Right (x ^ y)
            else Left NegativePower, a, b)
        _ -> undefined

type Queue a = ([a], [a])

pop :: State (Queue a) (Maybe a)
pop = state pop'
  where
    pop' :: Queue a -> (Maybe a, Queue a)
    pop' ([], []) = (Nothing, ([], []))
    pop' (l, []) = (Just x, ([], xs))
      where
        (x : xs) = reverse l
    pop' (l, y : ys) = (Just y, (l, ys))

push :: a -> State (Queue a) ()
push x = modify $ \(l, r) -> (x : l, r)

moving :: Int -> [Int] -> [Double]
moving sz list = evalState (steps list 0 0 []) ([], [])
  where
    steps :: [Int] -> Int -> Double -> [Double] -> State (Queue Int) [Double]
    steps [] _ _ res = return $ reverse res
    steps (x : xs) cnt prev res = do
      let cntD = fromIntegral cnt
      let xD = fromIntegral x
      if cnt < sz
      then do
        let curr = (xD + prev * cntD) / (cntD + 1)
        push x
        steps xs (cnt + 1) curr (curr : res)
      else do
        el <- pop
        let elD = fromIntegral $ fromMaybe 0 el
        let curr = (xD - elD) / cntD + prev
        push x
        steps xs cnt curr (curr : res)
