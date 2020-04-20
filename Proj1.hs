-- Author:  Yuqiang Zhu <yuqiangz@student.unimelb.edu.au>
-- Purpose: COMP90048 Declarative Programming Project 1 
-- 
-- This Program is to try to guess the location of three ships on 4 * 8 grid. 
-- GameState stores possible combinations of locations. After geting each 
-- feedback, program filters combinations with the feedbcak and the next guess
-- is picked from the last combination until getting all right locations.

module Proj1 (Location, toLocation, feedback,
              GameState, initialGuess, nextGuess) where

import Data.Char
import Data.List

-- ************************* type definition *********************************
-- Location (col, row) represents a grid. To calculate data easily, col and 
-- row are Int, which respectively represent ['A'..'H'] and [1..4].
data Location = Location (Int, Int) deriving (Eq, Ord)

-- Define Location show, e.g. Location (1, 1) is shown as "A1"
instance Show Location where
    -- convert col and row to ['A'..'H'] and [1..4]
    show (Location (col, row)) = [chr (64 + col), intToDigit row]

data GameState = 
    GameState [(Location, Location, Location)] deriving (Show, Eq)


-- ************************* function definition *****************************
-- Give initial guess and initial GameState. The initial GameState is all 
-- possible combination of locations. The initial guess is the smallest of sum
--  of guess all combinationswhat I can find in the limit time.
initialGuess :: ([Location], GameState)
initialGuess = ([Location (1,1), Location (5,4), Location (8,2)], allCombos)

-- Generate all possible combinations of locations
allCombos :: GameState
-- As the coordinate is shown as int, list all possible combinations from
-- small to large of (col * 10 + row)
allCombos = 
    GameState [((Location (x1, y1)) ,(Location (x2, y2)) ,(Location (x3, y3)))
        | x1 <- [1..8]
        , y1 <- [1..4]
        , x2 <- [1..8]
        , y2 <- [1..4]
        , (x1 * 10 + y1) < (x2 * 10 + y2)
        , x3 <- [1..8]
        , y3 <- [1..4]
        , (x2 * 10 + y2) < (x3 * 10 + y3)
        ]

-- Convert string to Location, if it is not in the board, show Nothing. 
toLocation :: String -> Maybe Location
toLocation xs =
    if isInBoard xs
        then Just (Location (col, row))
        else Nothing
    where
        -- convert the first character of string to int, 64 is the last 
        -- ASCII of 'A'.
        col = (ord (head xs)) - 64
        -- convert the last character of string to int
        row = digitToInt (last xs)

-- Determine whether the string shows the location in the grid
isInBoard :: String -> Bool
isInBoard [col_char, row_char] =
    (elem col_char ['A'..'H']) && (elem row_char ['1'..'4'])
isInBoard _ = False

-- Get the feedback from guesses and targets. 
feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback _ [] = (0, 0, 0)
feedback targets (x:xs) = 
    let
        (n1, n2, n3) = feedback targets xs
        (r1, r2, r3) = singleResult targets x
        in (r1 + n1, r2 + n2, r3 + n3)

-- Calculate the distance of two locations. The distance is the max of col 
-- distance and row distance. Only care about <= 2 distance
distance :: Location -> Location -> Maybe Int
distance (Location (x1, y1)) (Location (x2, y2))
    |max_dis > 2 = Nothing
    |otherwise = Just max_dis
    where
        dis_col = abs (x1 - x2)
        dis_row = abs (y1 - y2)
        max_dis = max dis_col dis_row

-- The result of a guess location with targets location. 
singleResult :: [Location] -> Location -> (Int, Int, Int)
singleResult targets guess
    -- a right location
    |elem (Just 0) dis_lst = (1, 0, 0)
    -- a location which exactly one space away from targets
    |elem (Just 1) dis_lst = (0, 1, 0)
    -- a location which exactly two space away from targets
    |elem (Just 2) dis_lst = (0, 0, 1)
    -- a location which exactly two spaces away from targets
    |otherwise = (0, 0, 0)
    where
        dis_lst = map (distance guess) targets

-- NextGuess filters last GameState according to last feedback and guess, then
-- pick a guess from GameState as next guess
nextGuess :: ([Location], GameState) -> (Int, Int, Int) -> 
    ([Location], GameState)
nextGuess (guesses, game_state) result =
    ([l1, l2, l3], (GameState last_combos))
    where
        GameState last_combos = 
            filterByCombos game_state guesses result
        -- pick the first combos as next guess
        (l1, l2, l3) = head last_combos

-- Filter all combinations with previous guess and reuslts
filterByCombos :: GameState -> [Location] -> (Int, Int, Int) -> GameState
filterByCombos (GameState lst) guesses result =
    let
        last_combos = 
            filter 
                (\(l1, l2, l3) -> feedback [l1,l2,l3] guesses == result) lst
    in GameState last_combos



-- ****************************** test code **********************************
start :: [Location] -> Int
start targets =
    let
        (initial_guesses, initial_gamestate) = initialGuess
    in repeatGuesses targets initial_guesses initial_gamestate

repeatGuesses :: [Location] -> [Location] -> GameState-> Int
repeatGuesses targets guesses gamestate
    |(sort targets) == (sort guesses) = 1
    |otherwise = 
        let
            (next_guesses, next_gamestate) = nextGuess (guesses, gamestate) (feedback targets guesses)
        in 1 + (repeatGuesses targets next_guesses next_gamestate)

-- 对全部的位置进行测试
testProj :: [Int]
testProj = 
    let (GameState combos_lst) = allCombos
        lst = map (\(l1, l2, l3) -> [l1, l2, l3]) combos_lst
    in map (start) lst

testAverage :: Int
testAverage = 
    let
        lst = testProj
    in div (sum lst) (length lst)

testSum :: Int
testSum = sum testProj

testToLocation :: [Maybe Location]
testToLocation = map (toLocation) lst
    where lst = [[x,y] | x <- ['A'..'H'], y <- ['1'..'4']]