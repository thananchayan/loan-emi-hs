module Utils
  ( readDouble
  , readInt
  , round2
  , parseCreditBand
  ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

import DataTypes (CreditBand(..))

trimSpaces :: String -> String
trimSpaces = dropWhileEnd isSpace . dropWhile isSpace

readDouble :: String -> Maybe Double
readDouble s =
  case reads (trimSpaces s) of
    [(x, "")] -> Just x
    _         -> Nothing

readInt :: String -> Maybe Int
readInt s =
  case reads (trimSpaces s) of
    [(x, "")] -> Just x
    _         -> Nothing

round2 :: Double -> Double
round2 x =
  let scaled :: Integer
      scaled = round (x * 100)
  in fromIntegral scaled / 100.0

parseCreditBand :: String -> Maybe CreditBand
parseCreditBand s =
  case trimSpaces s of
    "Good"    -> Just Good
    "Average" -> Just Average
    "Poor"    -> Just Poor
    _         -> Nothing
