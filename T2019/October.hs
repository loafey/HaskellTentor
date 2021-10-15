import Prelude         hiding (unwords)
import Data.List       hiding (unwords)
import Test.QuickCheck hiding (shuffle)



----------------------------------------QUESTION 1-------------------------------------------------

q1 ::Int-> [Int]
q1 1 = [1]
q1 n = n : q1 (next n)
    where    
        next n | odd  n = 3 * n + 1 
               | even n = n `div` 2

{-

q1 5  > 5 : q1 16                   next 5 = 3 * 5 + 1 = 16
q1 16 > 5 : 16 : q1 8               next 16 = 16 div 2 = 8
q1 8  > 5 : 16 : 8 : q1 4           next 8 = 8 div 2 = 4
q1 4  > 5 : 16 : 8 : 4 : q1 2       next 4 = 4 div 2 = 2
q1 2  > 5 : 16 : 8 : 4 : 2 : q1 1   next 2 = 2 div 1 = 2
q1 1  > 5 : 16 : 8 : 4 : 2 : [1]    next 1 = [1]   
Result: [5,16,8,4,2,1]

-}



----------------------------------------QUESTION 2-------------------------------------------------

unwords :: [String] -> String
unwords []     = ""
unwords [x]    = x
unwords (x:xs) = x ++ " " ++ unwords xs


prop_unwords :: Bool
prop_unwords = unwords [] == "" &&
               unwords ["Hello"] == "Hello" &&
               unwords ["Hello", "world"] == "Hello world" &&   
               unwords ["A", "good", "example"] == "A good example"




----------------------------------------QUESTION 3-------------------------------------------------

sortLines :: FilePath -> IO ()
sortLines file = do
     f <- readFile file
     let ans = unwords (sort (words f)) --Can be done using $ instead of parens
     writeFile ("Sorted" ++ file) ans 



----------------------------------------QUESTION 4-------------------------------------------------

data ChessBoard = Board [[Square]]

data Piece = Pawn | Knight | Bishop | Rook | Queen | King

data Color = White | Black

data ChessPiece = ChessPiece Piece Color

data Square = Square (Int,Int) (Maybe ChessPiece)

occupiedByWhiteKing :: Square
occupiedByWhiteKing = Square (0,5) (Just (ChessPiece King White)) 




----------------------------------------QUESTION 5-------------------------------------------------

doubleThree (x,y,z) = (x*2, y*2, z*2) 
squareThree (x,y,z) = (x^2, y^2, z^2)
revThree (xs,ys,zs) = (reverse xs, reverse ys, reverse zs)

mapThree :: (a -> b) -> (a,a,a) -> (b,b,b)
mapThree f (x,y,z) = (f x, f y, f z)




----------------------------------------QUESTION 6-------------------------------------------------

data Colour = Green | Yellow | Red
    deriving (Eq, Show)
data Tree   = Empty | Leaf Colour | Branch Tree Tree
    deriving (Eq, Show) 

autumnize :: Colour -> Tree -> Tree
autumnize _ Empty               = Empty
autumnize c (Leaf Green)        = Leaf c
autumnize _ (Leaf    _ )        = Empty
autumnize c (Branch left right) = Branch (autumnize c left) (autumnize c right) 


exampleTree :: Tree
exampleTree = (Branch (Leaf Green) (Branch (Leaf Yellow) Empty))

prop_autumnize :: Bool
prop_autumnize = autumnize Red exampleTree == Branch (Leaf Red) (Branch Empty Empty) 




----------------------------------------QUESTION 7-------------------------------------------------

f :: [Int] -> [Int]
f xs = xs

prop_sum_and_length :: [Int] -> Bool
prop_sum_and_length xs = length newList <= length xs && sum newList == sum xs 
    where
        newList = f xs




----------------------------------------QUESTION 8-------------------------------------------------


data Tree2 a = Empty2 | Node (Tree2 a) a (Tree2 a)
    deriving Show

--Node t1 x t2, the values in t1 are smaller than x and t2 are greater than x

member :: Ord a => a -> Tree2 a -> Bool
member v Empty2 = False
member v (Node left x right)
    | v < x     = member v left
    | v > x     = member v right
    | otherwise = True

prop_member x t = (member x t) == (x `elem` (flatten t))

flatten :: Tree2 a -> [a]
flatten Empty2 = []
flatten (Node lt x rt) = flatten lt ++ [x] ++ flatten rt




----------------------------------------QUESTION 9-------------------------------------------------

shuffle :: [a] -> Gen [a]
shuffle = shufHelp []

shufHelp :: [a] -> [a] -> Gen [a]
shufHelp xs []     = pure xs
shufHelp xs (y:ys) = do
    n <- choose (0, length xs)
    let ds = putAt y n xs 
    shufHelp ds ys 

putAt :: a -> Int -> [a] -> [a]
putAt x n xs = h ++ (x:t)
    where
        (h,t) = splitAt n xs

--generate $ shuffle <[a]> to test it
