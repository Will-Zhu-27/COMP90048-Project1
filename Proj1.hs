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

inBoard :: String -> Bool
inBoard [col_char, row_char]
    = (elem col_char ['A'..'H']) && (elem row_char ['1'..'4'])
inBoard _ = False

data GameState = GG
                deriving (Eq, Show)

toLocation :: String -> Maybe Location
toLocation xs =
    if inBoard xs
        then Just (Location (col, row))
        else Nothing
    where
        col = toCol (head xs)
        row = digitToInt (last xs)

feedback :: [Location] -> [Location] -> (Int, Int, Int)
feedback _ _ = (1,1,1)

initialGuess :: ([Location], GameState)
initialGuess = ([(Location (1,1))], GG)

nextGuess :: ([Location], GameState) -> (Int, Int, Int) -> ([Location], GameState)
nextGuess _ _ = ([(Location (1,1))], GG)