module T2020.January where

import Data.List(nub)
import Data.Maybe(isNothing)
--- Question 1 ---
q1 :: Int -> Int -> Int
q1 x 0 = x
q1 x y | even y = q1 (x+y) (y-1)
       | odd  y = q1 x     (y-1)
{-
q1 0 4 > even y = q1 (0+4) (4-1)
q1 4 3 > odd  y = q1 (4)   (3-1)
q1 4 2 > even y = q1 (4+2) (2-1)
q1 6 1 > odd  y = q1 (6)   (1-1)
q1 6 0 > 6

answer = 6
-}

--- Question 2 ---
-- TODO
allSameOrDifferent :: Eq a => [a] -> Bool
allSameOrDifferent []   = True
allSameOrDifferent list = unique || same
    where
        unique = (length $ nub list) == length list
        same   = (length $ nub list) == 1

prop_allSameOrDifferent :: [Bool]
prop_allSameOrDifferent = [
        p1,p2,p3,p5,p6
    ]
    where
        p1 = allSameOrDifferent [1,2,3]    == True
        p2 = allSameOrDifferent [1,2,2]    == False
        p3 = allSameOrDifferent [2,2,2]    == True
        p5 = allSameOrDifferent "function" == False
        p6 = allSameOrDifferent "Haskell"  == False

--- Question 3 ---
data SnocList a = Nil | Snoc (SnocList a) a

toList :: SnocList a -> [a]
toList Nil        = []
toList (Snoc s x) = toList s ++ [x]

prop_snocList = toList (((Nil `Snoc` 1) `Snoc` 2) `Snoc` 3) == [1,2,3]

--- Question 4 ---
filtermap :: (b -> Bool) -> (a -> b) -> [a] -> [b]
filtermap p f xs = [y | y <- mapped, p y]
    where mapped = [f x | x <- xs]
-- Original: filtermap p f xs = filter p (map f xs)

--- Question 5 ---
replace :: Int-> a -> [a] -> [a]
replace _ _   []       = []
replace 0 new (old:xs) = new:xs
replace i new (old:xs) = old:replace (i-1) new xs

modify :: Int -> (a -> a) -> [a] -> [a]
modify _ _ []       = []
modify 0 f (old:xs) = f old : xs
modify i f (old:xs) = old   : modify (i-1) f xs

prop_modify :: [Bool]
prop_modify = [p2]
    where
        --p1 = modify 6 toUpper "Happy new Year!" == "Happy New Year!" 
        p2 = modify 2 (*100) [5,6,7,8,9] == [5,6,700,8,9]

--- Question 6 ---
addingMachine :: Int -> IO ()
addingMachine i = do
    putStrLn $ "Sum so far: " ++ show i
    putStr "Enter next number: "
    inputString <- getLine
    let input = read inputString
    addingMachine $ input + i

--- Question 7 ---
prop_odd_not_even :: Int -> Bool
prop_odd_not_even x = odd x /= even x

--- Question 8 ---
data Tree a = Empty | Node (Tree a) a (Tree a)
              deriving (Eq, Show)

t :: Tree Int
t = Node (Node Empty 3 Empty) 8 (Node Empty 10 Empty)

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty        = Node Empty x Empty
insert x (Node l v r) | x < v     = Node (insert x l) v r
                      | otherwise = Node l v (insert x r)

prop_insert = insert 9 t == Node (Node Empty 3 Empty) 8 (Node (Node Empty 9 Empty) 10 Empty)

--- Question 9 ---
data ChessBoard = Board [[Maybe Piece]]
data Piece = CoolKid-- the details are irrelevant here
type Position = (Int,Int)
e2 = (4,6)
e4 = (4,4)

pieceAt :: ChessBoard -> Position -> Maybe Piece
pieceAt (Board rows) (x,y) = rows !! y !! x

replacePiece :: ChessBoard -> Position -> Maybe Piece -> ChessBoard
replacePiece (Board rows) (x,y) p = Board $ modify y (\r -> replace x p r) rows

move :: ChessBoard -> Position -> Position -> Maybe ChessBoard
move cb pos1 pos2 | isNothing (pieceAt cb pos1) = Nothing
                  | otherwise = Just $ replacePiece (replacePiece cb pos1 Nothing) pos2 (Just CoolKid)