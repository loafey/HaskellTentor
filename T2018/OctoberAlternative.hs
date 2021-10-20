{- Example solutions to DIT440 exam October 2018.

Make use of the content at your own risk:
* Answers have not been checked against an answer sheet.
* The questions related QuickCheck properties in particular might be completely
  off.
-}

module October where

import Prelude
import Data.Maybe (fromMaybe, listToMaybe)
import Test.QuickCheck


--
--
-- QUESTION 1
--
--

-- Function given by the problem.

q1 :: Int -> [[Int]]
q1 n | n <= 0    = []
     | otherwise = replicate n 1 : q1 (n `div` 2)

{-

Example of how the answer can be reached:

  q1 4
= replicate 4 1 : q1 (4 `div` 2)                          | Expand | q1 4          (2nd case)
= [1, 1, 1, 1] : q1 (4 `div` 2)                           | Eval   | replicate 4 1
= [1, 1, 1, 1] : q1 2                                     | Eval   | 4 `div` 2
= [1, 1, 1, 1] : replicate 2 1 : q1 (2 `div` 2)           | Expand | q1 2          (2nd case)
= [1, 1, 1, 1] : [1, 1] : q1 (2 `div` 2)                  | Eval   | replicate 2 1
= [1, 1, 1, 1] : [1, 1] : q1 1                            | Eval   | 2 `div` 2
= [1, 1, 1, 1] : [1, 1] : replicate 1 1 : q1 (1 `div` 2)  | Expand | q1 1          (2nd case)
= [1, 1, 1, 1] : [1, 1] : [1] : q1 (1 `div` 2)            | Eval   | replicate 1 1
= [1, 1, 1, 1] : [1, 1] : [1] : q1 0                      | Eval   | 1 `div` 2
= [1, 1, 1, 1] : [1, 1] : [1] : []                        | Expand | q1 0          (1st case; base case)
= [1, 1, 1, 1] : [1, 1] : [[1]]                           | Eval   | [1] : []
= [1, 1, 1, 1] : [[1, 1], [1]]                            | Eval   | [1, 1] : [[1]]
= [[1, 1, 1, 1], [1, 1], [1]]                             | Eval   | [1, 1, 1, 1] : [[1, 1], [1]]

Answer: [[1, 1, 1, 1], [1, 1], [1], []]

-}

--
--
-- QUESTION 2
--
--

--
-- Example solution.
--

lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll key kvs = [ v | (k, v) <- kvs, k == key ]

--
--
-- QUESTION 3
--
--

-- Type aliases given by the problem.

type Age   = Int
type Year  = Int
type Month = Int
type Day   = Int

--
-- Example solution.
--

data Ticket
    = AdultTicket Period
    | ChildTicket Period Age

data Period
    = SeasonPass Year
    | DayPass Date
    -- It would also be possible to skip the Date type and define DayPass as:
    -- DayPass Year Month Day

data Date = Date Year Month Day

exampleTicket :: Ticket
exampleTicket = ChildTicket (SeasonPass 2021) 13

--
--
-- QUESTION 4
--
--

-- Data types given by the problem.

data Expr
    = Num Int
    | Add Expr Expr
    | Mul Expr Expr
    deriving (Eq, Show)

data Ex
    = NumEx Int
    | BinEx Op Ex Ex
    deriving Show

data Op
    = AddOp
    | MulOp
    deriving Show

--
-- Example solution.
--

convert :: Expr -> Ex
convert expr = case expr of
    Num n     -> NumEx n
    Add e1 e2 -> BinEx AddOp (convert e1) (convert e2)
    Mul e1 e2 -> BinEx MulOp (convert e1) (convert e2)

--
--
-- QUESTION 5
--
--

--
-- BEWARE: I have no idea if this even answers the question in the exam, make
--         of it what you want :~P
--

-- prop_lookup :: (Eq a, Eq b) => a -> [(a, b)] -> Bool
-- prop_lookup k kvs = lookup k kvs == listToMaybe (lookupAll k kvs)

-- `listToMaybe` is a safe version of `head` that returns `Nothing` if the input
-- list is empty. If successful, the returned value is wrapped in `Just`.

--
--
-- QUESTION 6
--
--

--
-- Example solution.
--

-- Definitions given by the problem.

type DayNumber = Int

guests :: DayNumber -> Int
guests _dn = undefined  -- No definition given

{- Problem doesn't give a definition, but here's an example for testing:

guests :: DayNumber -> Int
guests dn
    | dn < 1          = error "guests: input must be positive"
    | dn `mod` 7 == 0 = 0     -- Closed every 7 days
    | even dn         = 2001  -- Busy on even days
    | otherwise       = 69    -- Neither on odd days
-}

-- The days in the range for which the restaurant had no guests.
closedDays :: DayNumber -> DayNumber -> [DayNumber]
closedDays n m = [d | d <- [n..m], guests d == 0]

-- The days in the range with more than 2000 guests.
busyDays :: DayNumber -> DayNumber -> [DayNumber]
busyDays n m   = [d | d <- [n..m], guests d > 2000]

--
-- Example solution.
--

-- 1.

days :: (DayNumber -> Bool) -> DayNumber -> DayNumber -> [DayNumber]
days predicate start end = filter predicate [start .. end]

-- 2.

closedDays' :: DayNumber -> DayNumber -> [DayNumber]
closedDays' start end = days (\d -> guests d == 0) start end

busyDays' :: DayNumber -> DayNumber -> [DayNumber]
busyDays' start end = days (\d -> guests d > 2000) start end

--
--
-- QUESTION 7
--
--

-- Definitions given by the problem.

names :: [String]
names = ["alice", "bob", "dave"]

emailProviders :: [String]
emailProviders = ["gmail", "yahoo", "hotmail"]

--
-- Example solution.
--

-- Test it out:
-- ghci> generate spam

spam :: Gen String
spam = do
    name <- elements names
    emailProvider <- elements emailProviders
    num <- chooseInt (0, 999)

    return $ name ++ show num ++ "@" ++ emailProvider ++ ".com"

--
--
-- QUESTION 8
--
--

-- Definitions given by the problem.

data DTree
    = Decision Answer
    | Q Question DTree DTree
    --           ^Yes  ^No

type Question = String
type Answer   = String

ex :: DTree
ex = Q "Is it Raining?" wet notWet
  where
    wet    = Decision "Take the bus"
    notWet = Q "Is it more the 2km?" (Decision "Cycle") (Decision "Walk")

--
-- Example solution.
--

attributes :: DTree -> [(Answer, [(Question, Bool)])]
attributes dtree = branches [] dtree

branches :: [(Question, Bool)] -> DTree -> [(Answer, [(Question, Bool)])]
branches history dtree = case dtree of
    Decision ans      -> [(ans, history)]
    Q question yes no -> yesBranch ++ noBranch
      where
        yesBranch :: [(Answer, [(Question, Bool)])]
        yesBranch = branches (history ++ [(question, True)]) yes

        noBranch :: [(Answer, [(Question, Bool)])]
        noBranch  = branches (history ++ [(question, False)]) no

--
--
-- QUESTION 9
--
--

-- Definitions given by the problem.

type Maze = [(Position, [Direction])]
type Position = (Int,Int)

data Direction =  N | S | E | W
    deriving (Eq, Show)

exampleMaze :: Maze
exampleMaze =
    [ ((0, 0), [E]), ((1, 0), [S]), ((2, 0), [])
    , ((0, 1), [E]), ((1, 1), []),  ((2, 1), [])
    , ((0, 2), []),  ((1, 2), [])
    ]

goodPath, badPath1, badPath2, badPath3 :: [Position]
goodPath = [(0, 0), (0, 1), (0, 2), (1, 2), (1, 1)]
badPath1 = [(0, 0), (1, 0)]  -- crosses a wall
badPath2 = [(1, 2), (2, 2)]  -- goes outside the maze
badPath3 = [(1, 2), (2, 1)]  -- needs two steps

--
-- Example solution.
--

-- Helpers

-- | Return the opposite of a direction.
opposite :: Direction -> Direction
opposite d = case d of
    N -> S
    S -> N
    W -> E
    E -> W

-- | Return the direction to go from the first position to the second, or
-- Nothing if the positions are not adjacent.
direction :: Position -> Position -> Maybe Direction
direction (x1, y1) (x2, y2) = case (horizontal, vertical) of
    (Just d, Nothing) -> Just d
    (Nothing, Just d) -> Just d
    _                 -> Nothing
  where
    horizontal :: Maybe Direction
    horizontal
        | x1 - x2 == 1  = Just W
        | x1 - x2 == -1 = Just E
        | otherwise     = Nothing

    vertical :: Maybe Direction
    vertical
        | y1 - y2 == 1  = Just N
        | y1 - y2 == -1 = Just S
        | otherwise     = Nothing

validPath :: Maze -> [Position] -> Bool
validPath maze path = and $ zipWith tryStep path (drop 1 path)
  where
    isValidStep :: Position -> Position -> Bool
    isValidStep p1 p2 = fromMaybe False $ tryStep p1 p2

    -- Return Nothing if not adjacent or if one positionis outside the maze.
    -- Return Just False if the path is blocked by a wall.
    -- Return Just True if the positions form a valid path.
    tryStep :: Position -> Position -> Maybe Bool
    tryStep p1 p2 = do
        dir <- direction p1 p2
        walls1 <- lookup p1 maze
        walls2 <- lookup p2 maze
        pure $ dir `notElem` walls1 ++ map opposite walls2

    -- tryStep equivalent without do-notation (returns a Bool directly)
    _tryStep :: Position -> Position -> Bool
    _tryStep p1 p2 = case (lookup p1 maze, lookup p2 maze, direction p1 p2) of
        (Just ws1, Just ws2, Just dir) -> dir `notElem` ws1 ++ map opposite ws2
        _ -> False

