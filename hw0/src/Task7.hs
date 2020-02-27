module Task7
  ( expr1
  , expr2
  , expr3
  ) where

import Data.Either

type SFunc = String -> String

type SBiFunc = SFunc -> SFunc

type SListToSFunc = [String] -> String

type SFuncSPair = (SFunc, String)

type SFuncSPairMap = (SFuncSPair -> String) -> [SFuncSPair] -> [String]

-- | Returns False.
-- Initial expression:
-- `null . head $ map (uncurry id) [((++) "Dorian ", " Grey")]`
expr1 :: Bool
expr1 = nullHeadApplication mappedList
  where
    nullHeadApplication :: [String] -> Bool
    nullHeadApplication = application nullHead
      where
        application :: ([String] -> Bool) -> [String] -> Bool
        application = ($)
        composition :: (String -> Bool) -> SListToSFunc -> [String] -> Bool
        composition = (.)
        compositionWithNull :: ([String] -> String) -> [String] -> Bool
        compositionWithNull = composition (null :: String -> Bool)
        nullHead :: [String] -> Bool
        nullHead = compositionWithNull (head :: [String] -> String)
    mappedList :: [String]
    mappedList = mapMapper list
      where
        mapMapper :: [SFuncSPair] -> [String]
        mapMapper = mapAlias mapper
          where
            mapAlias :: SFuncSPairMap
            mapAlias = map
            mapper :: SFuncSPair -> String
            mapper = uncurryAlias idAlias
            uncurryAlias :: SBiFunc -> (SFunc, String) -> String
            uncurryAlias = uncurry
            idAlias :: SBiFunc
            idAlias = id
        list :: [SFuncSPair]
        list = [listFirstElement]
          where
            append :: String -> SFunc
            append = (++)
            appendToDorian :: SFunc
            appendToDorian = append ("Dorian " :: String)
            listFirstElement :: SFuncSPair
            listFirstElement = (appendToDorian, " Grey" :: String)

-- | Returns [(3,64)].
-- Initial expression:
-- `(\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)]`
expr2 :: [(Integer, Integer)]
expr2 = lambda list
  where
    lambda :: [Either Integer Integer] -> [(Integer, Integer)]
    lambda =
      \x ->
        let leftsAlias = lefts :: [Either a b] -> [a]
            rightsAlias = rights :: [Either a b] -> [b]
            first = leftsAlias x :: [Integer]
            second = rightsAlias x :: [Integer]
            zipAlias = zip :: [Integer] -> [Integer] -> [(Integer, Integer)]
            zipBoundFirst = zipAlias first :: [Integer] -> [(Integer, Integer)]
         in zipBoundFirst second
    list :: [Either Integer Integer]
    list = [listFirstElement, listSecondElement]
      where
        listFirstElement :: Either Integer Integer
        listFirstElement = Left plusResult
          where
            plus :: Integer -> Integer -> Integer
            plus = (+)
            appliedLeftOperand :: Integer -> Integer
            appliedLeftOperand = plus (1 :: Integer)
            plusResult :: Integer
            plusResult = appliedLeftOperand (2 :: Integer)
        listSecondElement :: Either Integer Integer
        listSecondElement = Right powerResult
          where
            power :: Integer -> Integer -> Integer
            power = (^)
            appliedLeftOperand :: Integer -> Integer
            appliedLeftOperand = power (2 :: Integer)
            powerResult :: Integer
            powerResult = appliedLeftOperand (6 :: Integer)

-- | Takes one argument and always returns True.
-- Initial expression:
-- `let impl = \x y -> not x || y in
--    let isMod2 = \x -> x `mod` 2 == 0 in
--      let isMod4 = \x -> x `mod` 4 == 0 in
--        \x -> (isMod4 x) `impl` (isMod2 x)`
expr3 :: Integer -> Bool
expr3 =
  let impl = implAlias
   in let isMod2 = isMod2Alias
       in let isMod4 = isMod4Alias
           in \x ->
                let xIsMod2 = isMod2 x :: Bool
                    xIsMod4 = isMod4 x :: Bool
                    implBoundFirst = impl xIsMod4 :: Bool -> Bool
                 in implBoundFirst xIsMod2
  where
    isMod2Alias :: Integer -> Bool
    isMod2Alias =
      \x ->
        let modAlias = mod :: Integer -> Integer -> Integer
            eqAlias = (==) :: Integer -> Integer -> Bool
            appliedLeftModOperand = (modAlias x) :: Integer -> Integer
            leftEqOperand = appliedLeftModOperand (2 :: Integer) :: Integer
            appliedLeftEqOperand = eqAlias leftEqOperand :: Integer -> Bool
         in appliedLeftEqOperand (0 :: Integer)
    implAlias :: Bool -> Bool -> Bool
    implAlias =
      \x y ->
        let orAlias = (||) :: Bool -> Bool -> Bool
            notAlias = (not) :: Bool -> Bool
            leftOrOperand = (notAlias x) :: Bool
            appliedLeftOrOperand = orAlias leftOrOperand
         in appliedLeftOrOperand y
    isMod4Alias :: Integer -> Bool
    isMod4Alias =
      \x ->
        let modAlias = mod :: Integer -> Integer -> Integer
            eqAlias = (==) :: Integer -> Integer -> Bool
            appliedLeftModOperand = modAlias x :: Integer -> Integer
            leftEqOperand = appliedLeftModOperand (4 :: Integer) :: Integer
            appliedLeftEqOperand = eqAlias leftEqOperand :: Integer -> Bool
         in appliedLeftEqOperand (0 :: Integer)
