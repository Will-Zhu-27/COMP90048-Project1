--  File     : Proj1.hs
--  Author   : Peter Schachte
--  Purpose  : Guessing and feedback parts of a guessing game
--
-- This module implements the guessing and answering (feedback)
-- parts of the Proj1 game. Proj1 is similar to Battleship, but in
-- Proj1 the object is to guess the location of three ships in an 8
-- by 4 ocean grid.
--
-- This module exports functions initialGuess and nextGuess,
-- specifying our first guess of a trio of locations, and all
-- subsequent guesses, respectively, plus the GameState type, which
-- is used internally in this module to remember whatever needs to
-- be remembered between guesses. It also exports a Location type
-- representing a board location, a toLocation function to convert a
-- two character string to a location, and a feedback function to
-- provide the feedback for a guess given the actual target. The
-- Location type in an instance of the Show class, where show is the
-- inverse of toLocation.
--
-- The strategy employed in this code is to maintain a list of
-- possible guesses, that is, the guesses that haven't been ruled
-- out by the feedback we have received so far. Each time we need to
-- make a guess, we choose one of the still-possible guesses that
-- would be expected to leave us with the fewest remaining possible
-- guesses when we receive the feedback for it. We calculate this by
-- taking the weighted average length of the list of remaining
-- possible guesses over all combinations of possible guesses and
-- targets.

module Proj1 (Location, toLocation, feedback,
              GameState, initialGuess, nextGuess) where

import Data.List
import qualified Data.Map as Map
import Control.Applicative
import Data.Char
import Text.Read

----------------------------------------------------------------
--             The Guess, Location, Row and Column types
----------------------------------------------------------------

-- | A Guess is simply a list of locations.  This keeps things simple,
--   and would make it easier to generalize the code to handle more
--   ships on the grid at a time.

type Guess = [Location]

toLocation :: String -> Maybe Location
toLocation = readMaybe


-- | Our Location type.  Although this module's interface represents
--   locations as two-character strings, we define a proper type for
--   use within this module.  We make this type an instance of the
--   Read and Show type classes so we can use use 'read' and
--   'show' to convert to and from strings, which are used to
--   communicate with this module. We use the Bounded type class to
--   avoid explicitly listing the highest and lowest location in the
--   code, to make it easier to generalise the code later.

data Location = Location {column :: Column, row :: Row}
                deriving (Eq, Bounded, Ord)

instance Show Location where
  show (Location c r) = show c ++ show r

instance Read Location where
    readsPrec _ string =
        [(Location c r,rest) | (c,rest0) <- reads $ dropWhile (== ' ') string,
                               (r,rest)  <- reads rest0]

instance Enum Location where
  fromEnum (Location c r) =
    fromEnum c + fromEnum r * (1 + fromEnum (maxBound::Column))
  toEnum c = Location (toEnum $ c `mod` numCols) (toEnum $ c `div` numCols)
    where numCols = 1 + fromEnum (maxBound::Column)


-- | Our Column type.  This is in the Ord class because we sort them.

data Column = A | B | C | D | E | F | G | H
              deriving (Eq, Ord, Enum, Bounded, Show)

instance Read Column where
    readsPrec _ (char:rest)
      | 'A' <= char && char <= 'H' = [(toEnum (ord char - ord 'A'), rest)]
      | otherwise                  = []
    readsPrec _ []                 = []

-- | Our Row type.  This is in the Ord class because we sort them.

data Row = R1 | R2 | R3 | R4
            deriving (Eq, Ord, Bounded, Enum)

instance Show Row where
  show r = [intToDigit $ 1 + fromEnum r]

instance Read Row where
    readsPrec _ (char:rest)
      | '1' <= char && char <= '4' = [(toEnum (ord char - ord '1'), rest)]
      | otherwise                  = []
    readsPrec _ []                 = []


----------------------------------------------------------------
--                      Our External Interface
----------------------------------------------------------------

-- | Our exported GameState type, to remember what we need to know
--   between guesses.  This is simply a list of the remaining
--   possible guesses.

newtype GameState = GameState [Guess]


-- | The feedback we receive for our guesses:  a triple of the number
--   of correctly guessed locations, the number of guessed locations
--   that are one square away from a ship, and the number that are
--   two squares away.

type Feedback = (Int, Int, Int)


-- | Returns our first guess.  Because the bestGuess function is too
--   slow to be used on the full list of all possible guesses, we
--   hard code our initial guess.  The guess A1, H1, A3 is the
--   result of bestGuess on allPossibilities.  Note that this guess
--   occupies only the first and last columns, thus maximising the
--   possibility of making a guess that will allows us to eliminate
--   the greatest possible number of squares as possibilities.
initialGuess :: (Guess,GameState)
initialGuess = (read "[A1,H1,A3]", GameState allPossibilities)


-- | Returns our next guess, given the result of our last guess and
--   our GameState remembered from our last guess.  This first
--   narrows down the list of possible guesses based on the feedback
--   from our last guess, and then determines the best guess to make
--   based on the new list of possible guesses.

nextGuess :: (Guess,GameState) -> Feedback -> (Guess,GameState)
nextGuess (guess, GameState poss) answer =
  let poss'  = remainingPossibilities poss guess answer
      guess' = bestGuess poss'
  in (guess', GameState poss')


----------------------------------------------------------------
--                  Managing the list of possible guesses
----------------------------------------------------------------

-- | The list of all possible distinct guesses.  Since a probe is a
--   *set* of locations, we avoid repeating locations, and we keep the
--   locations in ascending order.

allPossibilities :: [Guess]
allPossibilities = combos 3

combos :: Int -> [Guess]
combos n | n < 1 = error "combos with non-positive argument"
combos 1 = map (:[]) [minBound .. maxBound]
combos n = [l:ls | ls <- combos $ n-1
                 , head ls > minBound -- ls is always non-empty
                 , l  <- [minBound .. pred $ head ls]]


-- | Returns the possible guesses remaining after considering the
--   feedback from a single guess.  This filters out any guesses from
--   the previous list of possibilities that are excluded because
--   they would have different feedback for the guess than the
--   feedback we actually received.

remainingPossibilities :: [Guess] -> Guess -> Feedback -> [Guess]
remainingPossibilities poss guess fback =
    filter ((==fback) . flip feedback guess) poss


----------------------------------------------------------------
--                      Computing Feedback
----------------------------------------------------------------

-- | Returns appropriate feedback for guess given target target.

feedback :: Guess -> Guess -> Feedback
feedback target guess = (right, oneAway, twoAway)
  where guess'      = nub guess         -- shouldn't be necessary
        distances   = map (\g -> minimum $ map (separation g) target) guess'
        right       = count (== 0) distances
        oneAway     = count (== 1) distances
        twoAway     = count (== 2) distances


-- | Returns the number of elements of the input list that satisfy
--   the specified predicate.

count :: (a -> Bool) -> [a] -> Int
count predicate = length . filter predicate


-- | The separation between two Locations. This is the maximum of
--   the number of rows and the number of columns between the two
--   locations.

separation :: Location -> Location -> Int
separation (Location c1 r1) (Location c2 r2) =
    max (abs $ fromEnum c1 - fromEnum c2) (abs $ fromEnum r1 - fromEnum r2)



----------------------------------------------------------------
--                   Choosing the best next guess
----------------------------------------------------------------

-- | Choose the best guess, given the list of remaining possible
--   cards. The approach is as described earlier: we pick a possible
--   card that gives us the lowest "expected outcome", where the
--   expected outcome is the average over all remaining possible
--   targets of the number of targets that will remain if we make that
--   guess for that target.

bestGuess :: [Guess] -> Guess
bestGuess poss = minimize (expectedOutcome poss) poss


--   Returns the average number of possible cards that would remain
--   after making the specified guess over all the possible targets
--   on poss.  We count the number of occur
expectedOutcome :: [Guess] -> Guess -> Double
expectedOutcome poss guess =
    let counts = Map.elems $ foldl countValue Map.empty
                 $ map (flip feedback guess) poss
    in  fromIntegral (sum $ map square counts) / fromIntegral (sum counts)


-- | Returns the square of the input.
square :: Num n => n -> n
square n = n * n


-- | Increment the counter for the specified t in the map, taking a
--   missing value for a key as zero.
countValue :: Ord t => Map.Map t Int -> t -> Map.Map t Int
countValue m k = Map.alter (((+1) <$>) . (<|> Just 0)) k m
     -- Here (<|> Just 0) maps Nothing to Just 0 and Just x to Just x.
     -- ((+1) <$>) adds 1 to the contents of a Maybe.


--   Returns element e of lst with the minimum value of f e
minimize :: Ord b => (a -> b) -> [a] -> a
minimize _ [] = error "minimising over an empty list"
minimize f (e:es) = minimize' f (f e) e es

minimize' :: Ord b => (a -> b) -> b -> a -> [a] -> a
minimize' _ _ m [] = m
minimize' f fm m (e:es)
  | fe < fm   = minimize' f fe e es
  | otherwise = minimize' f fm m es
  where fe = f e
