-- Author: Yuqiang Zhu
-- Purpose: COMP90048 Declarative Programming Project 1
--Implement your solution here
--SEE THE PROJECT CODING GUIDELINES ON THE LMS FOR DETAILS OF
--THE CRITERIA THAT WILL BE EMPLOYED IN ASSESSING YOUR CODE.
--Edit this file as you like; just keep the below declaration.

module Proj1 (Location, toLocation, feedback,
              GameState, initialGuess, nextGuess) where

data Location = Location Col Col
                deriving (Eq, Show)

data Col = A|B|C|D|E|F|G|H
                deriving (Eq, Show)

data GameState = GG
                deriving (Eq, Show)

toLocation :: String -> Maybe Location
toLocation _ = Just (Location A A)

feedback :: [Location] -> [Location] -> (Int, Int, Int)
feedback _ _ = (1,1,1)

initialGuess :: ([Location], GameState)
initialGuess = ([(Location A A)], GG)

nextGuess :: ([Location], GameState) -> (Int, Int, Int) -> ([Location], GameState)
nextGuess _ _ = ([(Location A A)], GG)