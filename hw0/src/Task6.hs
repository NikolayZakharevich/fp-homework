module Task6
  ( expr1
  , expr2
  , foo
  ) where

import Data.Maybe (mapMaybe)
import Task1 (distributivity)

-- | Weak head normal form:
-- (Left ("harold" ++ " hide " ++ "the " ++ "pain"),
-- Left ("harold" ++ " hide " ++ "the " ++ "pain")).
expr1 :: (Either String String, Either String String)
expr1 = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

-- | Weak head normal form: False.
expr2 :: Bool
expr2 = null $ mapMaybe foo "pole chudes ochen' chudesno"

-- | If argument equals to 'o' character returns Just e^pi (23.1406926328...),
-- otherwise returns Nothing.
foo :: Char -> Maybe Double
foo char =
  if char == 'o'
    then Just $ exp pi
    else Nothing
