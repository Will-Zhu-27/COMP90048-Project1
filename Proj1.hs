-- Author: Yuqiang Zhu
-- Purpose: COMP90048 Declarative Programming Project 1
--Implement your solution here
--SEE THE PROJECT CODING GUIDELINES ON THE LMS FOR DETAILS OF
--THE CRITERIA THAT WILL BE EMPLOYED IN ASSESSING YOUR CODE.
--Edit this file as you like; just keep the below declaration.

module Proj1 (Location, toLocation, feedback,
              GameState, initialGuess, nextGuess) where

import Data.Char

type Col = Int
type Row = Int
data Location = Location (Col, Row) deriving (Eq, Read)

instance Show Location where
    show (Location (col, row))
        = [toUpper (intToDigit (col + 9)), intToDigit row]

toCol :: Char -> Int
toCol x = (digitToInt x) - 9

isInBoard :: String -> Bool
isInBoard [col_char, row_char]
    = (elem col_char ['A'..'H']) && (elem row_char ['1'..'4'])
isInBoard _ = False

toLocation :: String -> Maybe Location
toLocation xs =
    if isInBoard xs
        then Just (Location (col, row))
        else Nothing
    where
        col = toCol (head xs)
        row = digitToInt (last xs)

distance :: Location -> Location -> Maybe Int
distance (Location (x1, y1)) (Location (x2, y2))
    |max_dis > 2 = Nothing
    |otherwise = Just max_dis
    where
        dis_col = abs (x1 - x2)
        dis_row = abs (y1 - y2)
        max_dis = max dis_col dis_row

result :: [Location] -> Location -> (Int, Int, Int)
result targets guess
    |elem (Just 0) dis_lst = (1, 0, 0)
    |elem (Just 1) dis_lst = (0, 1, 0)
    |elem (Just 2) dis_lst = (0, 0, 1)
    |otherwise = (0, 0, 0)
    where
        dis_lst = map (distance guess) targets

feedback :: [Location] -> [Location] -> (Int, Int, Int)
feedback _ [] = (0, 0, 0)
feedback targets (x:xs) = 
    let
        (n1, n2, n3) = feedback targets xs
        (r1, r2, r3) = result targets x
        in (r1 + n1, r2 + n2, r3 + n3)

data GameState = GG

initialGuess :: ([Location], GameState)
initialGuess = ([(Location (1,1))], GG)

nextGuess :: ([Location], GameState) -> (Int, Int, Int) -> ([Location], GameState)
nextGuess _ _ = ([(Location (1,1))], GG)