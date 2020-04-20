-- Author: Yuqiang Zhu
-- Purpose: COMP90048 Declarative Programming Project 1
--Implement your solution here
--SEE THE PROJECT CODING GUIDELINES ON THE LMS FOR DETAILS OF
--THE CRITERIA THAT WILL BE EMPLOYED IN ASSESSING YOUR CODE.
--Edit this file as you like; just keep the below declaration.

module Proj1 (Location, toLocation, feedback,
              GameState, initialGuess, nextGuess) where
import Data.Char
import Data.List

type Col = Int
type Row = Int
data Location = Location (Col, Row) deriving (Eq, Ord)

instance Show Location where
    show (Location (col, row)) = [chr (64 + col), intToDigit row]

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

feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback _ [] = (0, 0, 0)
feedback targets (x:xs) = 
    let
        (n1, n2, n3) = feedback targets xs
        (r1, r2, r3) = result targets x
        in (r1 + n1, r2 + n2, r3 + n3)

data GameState = GameState [(Location, Location, Location)] deriving (Show, Eq)

initialGuess :: ([Location], GameState)
initialGuess = ([Location (2,3), Location (3,3), Location (6,3)], allCombos)

allCombos :: GameState
allCombos = GameState [((Location (x1, y1)),(Location (x2, y2)),(Location (x3, y3)))
    | x1 <- [1..8]
    , y1 <- [1..4]
    , x2 <- [1..8]
    , y2 <- [1..4]
    , (x1 * 10 + y1) < (x2 * 10 + y2)
    , x3 <- [1..8]
    , y3 <- [1..4]
    , (x2 * 10 + y2) < (x3 * 10 + y3)
    ]


allLocations :: [Location]
allLocations = [Location (x, y) | x <- [1..8], y <- [1..4]]

nextGuess :: ([Location], GameState) -> (Int, Int, Int) -> ([Location], GameState)
nextGuess (guesses, (GameState combos)) result = ([l1, l2, l3], (GameState filter_combos))
    where
        GameState filter_combos = filterByCombos (GameState combos) guesses result
        (l1, l2, l3) = head filter_combos

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

-- 将剩下的组合用上一个guesses进行筛选
filterByCombos :: GameState -> [Location] -> (Int, Int, Int) -> GameState
filterByCombos (GameState lst) pre_guesses result =
    GameState (filter (\(l1, l2, l3) -> feedback [l1,l2,l3] pre_guesses == result) lst)

-- 用于根据猜的位置对GameState进行筛选
filterByLocation :: GameState -> [Location] -> Int -> GameState
filterByLocation gamestate [] _ = gamestate
filterByLocation (GameState combos_lst) loc_lst n = (GameState filter_lst)
    where filter_lst = filter (\x -> sameLocationNum x loc_lst == n) combos_lst

sameLocationNum :: (Location, Location, Location) -> [Location] -> Int
sameLocationNum (loc1, loc2, loc3) [] = 0
sameLocationNum (loc1, loc2, loc3) loc_lst =
    length (filter (`elem` loc_lst) [loc1, loc2, loc3])

-- 筛选出给定距离的给定交叉的位置，是否要去除掉猜的位置？？？
distanceFilter :: [Location] -> Int -> Int -> [Location]
distanceFilter loc_lst dis overlap_num = [Location (x, y) 
    | Location (x, y) <- allLocations
    , (length (filter (\loc -> (distance (Location (x,y)) loc) == Just dis) loc_lst)) == overlap_num]

distanceLocation :: Location -> Int -> [Location]
distanceLocation (Location (col, row)) dis = 
    [Location (col + x, row + y) | x <- [(-dis)..dis], (x + col) > 0 && (x + col) < 9, y <- [(-dis)..dis], (y + row) > 0 && (y + row) < 5, (max (abs x) (abs y)) == dis]