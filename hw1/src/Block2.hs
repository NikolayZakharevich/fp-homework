module Block2
  ( splitOn
  , joinWith
  ) where

import Data.List.NonEmpty as NE hiding (head)

-- Splits string into parts on given delimiter. For example:
--
-- >>> splitOn '/' "path/to/file"
-- "path" :| ["to", "file"]
--
-- >>> splitOn '/' "///"
-- "/" :| ["/"]
--
-- >>> splitOn '0' "abacaba"
-- "abacaba" :| []
--
-- >>> joinWith '/' ("path" :| ["to", "file"])
-- "path/to/file"
--
splitOn :: Char -> String -> NonEmpty String
splitOn delimiter = foldr splitter ([] :| [])
  where
    splitter :: (Char -> NonEmpty String -> NonEmpty String)
    splitter char parts =
      case parts of
        ("" :| xs) ->
          if delimiter == char
            then [delimiter] :| xs
            else [char] :| xs
        (x :| xs) ->
          if delimiter == char
            then "" :| (x : xs)
            else (char : x) :| xs

-- Joins string parts with given delimiter. For example:
--
-- >>> joinWith '/' ("path" :| ["to", "file"]
-- "path/to/file"
--
-- >>> joinWith '/' ("//" :| ["/", "///"])
-- "////////"
--
-- >>> joinWith '/' ("path" :| [])
-- "path"
--
-- >>> joinWith '/' ("" :| []))
-- ""
--
joinWith :: Char -> NonEmpty String -> String
joinWith delimiter = foldr joiner ""
  where
    joiner :: (String -> String -> String)
    joiner current ""   = current
    joiner current done = current ++ [delimiter] ++ done
