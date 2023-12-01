module Calibration (solution) where

import Data.Maybe
import Definitions

part1 :: [String] -> Int
part1 = sum . map extract

part2 :: [String] -> Int
part2 = sum . map (extract . fixLine)

-- extract 2 digit number from the line
extract :: String -> Int
extract = (\digits -> head digits * 10 + last digits) . mapMaybe toDigit

toDigit :: Char -> Maybe Int
toDigit '0' = Just 0
toDigit '1' = Just 1
toDigit '2' = Just 2
toDigit '3' = Just 3
toDigit '4' = Just 4
toDigit '5' = Just 5
toDigit '6' = Just 6
toDigit '7' = Just 7
toDigit '8' = Just 8
toDigit '9' = Just 9
toDigit  _  = Nothing

-- convert spelled out digits to numerical digits
fixLine :: String -> String
fixLine ('o':'n':'e'        :xs) = '1' : fixLine ('e':xs)
fixLine ('t':'w':'o'        :xs) = '2' : fixLine ('o':xs)
fixLine ('t':'h':'r':'e':'e':xs) = '3' : fixLine ('e':xs)
fixLine ('f':'o':'u':'r'    :xs) = '4' : fixLine ('r':xs)
fixLine ('f':'i':'v':'e'    :xs) = '5' : fixLine ('e':xs)
fixLine ('s':'i':'x'        :xs) = '6' : fixLine ('x':xs)
fixLine ('s':'e':'v':'e':'n':xs) = '7' : fixLine ('n':xs)
fixLine ('e':'i':'g':'h':'t':xs) = '8' : fixLine ('t':xs)
fixLine ('n':'i':'n':'e'    :xs) = '9' : fixLine ('e':xs)
fixLine (x                  :xs) =  x  : fixLine      xs
fixLine []                       = []

solution :: Solution
solution = Solution part1 part2