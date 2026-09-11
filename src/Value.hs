module Value (Value (..), Env, equalValues) where

import Ast (Expr)
import Data.List (intercalate)
import qualified Data.Map as Map

data Value
  = VNum Integer
  | VBool Bool
  | VList [Value]
  | VClosure String Expr Env

type Env = Map.Map String Value

-- Language equality is deliberately separate from equality of host functions.
-- Functions are never equal, including to themselves; mixed types are unequal.
equalValues :: Value -> Value -> Bool
equalValues (VNum a) (VNum b) = a == b
equalValues (VBool a) (VBool b) = a == b
equalValues (VList a) (VList b) = length a == length b && and (zipWith equalValues a b)
equalValues _ _ = False

instance Eq Value where
  (==) = equalValues

instance Show Value where
  show (VNum n) = show n
  show (VBool b) = show b
  show (VList values) = "[" ++ intercalate ", " (map show values) ++ "]"
  show (VClosure name _ _) = "<closure: \\" ++ name ++ " -> ...>"
