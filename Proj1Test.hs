--  File     : Proj1Test.hs
--  Author   : Peter Schachte
--  Purpose  : Test program for proj1 project submissions

module Main where

import Data.List
import System.Environment
import System.Exit
import Proj1 (Location, toLocation, feedback,
              GameState, initialGuess, nextGuess)


-- | Main program.  Gets the target from the command line (as three
--   separate command line arguments, each a note letter (upper case)
--   followed by an octave number.  Runs the user's initialGuess and
--   nextGuess functions repeatedly until they guess correctly.
--   Counts guesses, and prints a bit of running commentary as it goes.
main :: IO ()
main = do
  args <- getArgs
  case mapM toLocation args of
    Just target@[_,_,_] ->
      proj1test target
    _ -> do
      name <- getProgName
      putStrLn $ "Usage:  " ++ name ++ " p1 p2 p3"
      putStrLn "   where p1 p2 p3 are 3 different locations between A1 and H4"
      exitFailure


-- | Guess the given target, counting and showing the guesses.
proj1test :: [Location] -> IO ()
proj1test target = do
      let (guess,other) = initialGuess
      loop target guess other 1

-- | Given a target and guess and a guess number, continue guessing
-- until the right target is guessed.
loop :: [Location] -> [Location] -> Proj1.GameState -> Int -> IO ()
loop target guess other guesses = do
  putStrLn $ "Your guess #" ++ show guesses ++ ":  " ++ show guess
  let answer = feedback target guess
  putStrLn $ "    My answer:  " ++ show answer
  if answer == (3,0,0)
    then do
      putStrLn $ "You got it in " ++ show guesses ++ " guesses!"
    else do
      let (guess',other') = nextGuess (guess,other) answer
      loop target guess' other' (guesses+1)
