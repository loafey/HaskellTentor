import Prelude                    hiding (unwords)
import Data.List                  hiding (unwords)
import Test.QuickCheck            hiding (shuffle)



----------------------------------------QUESTION 1-------------------------------------------------

q1 ::Int-> [Int]
q1 1 = [1]
q1 n = n : q1 (next n)
    where    
        next n | odd  n = 3 * n + 1 
               | even n = n `div` 2

{-

q1 5     ->     5 : q1 16                   -- next 5  = 16
q1 16    ->     5 : 16 : q1 8               -- next 16 = 8
q1 8     ->     5 : 16 : 8 : q1 4           -- next 8  = 4 
q1 4     ->     5 : 16 : 8 : 4 : q1 2       -- next 4  = 2
q1 2     ->     5 : 16 : 8 : 4 : 2 : q1 1   -- next 2  = 1
q1 1     ->     5 : 16 : 8 : 4 : 2 : [1]    -- next 1  = [1]   

Result: [5,16,8,4,2,1]

-}



----------------------------------------QUESTION 2-------------------------------------------------

--same thing as unwords in prelude
unwords :: [String] -> String
unwords []     = ""
unwords [x]    = x
unwords (x:xs) = x ++ ' ' : unwords xs


prop_unwords :: Bool
prop_unwords = unwords [] == "" &&
               unwords ["Hello"] == "Hello" &&
               unwords ["Hello", "world"] == "Hello world" &&   
               unwords ["A", "good", "example"] == "A good example"




----------------------------------------QUESTION 3-------------------------------------------------

sortLines :: FilePath -> IO ()
sortLines file = do
     f <- readFile file --save the contents of the file to f
     let ans = unwords (sort (words f)) --split word in to list, sort list, convert back to string 
     writeFile ("Sorted" ++ file) ans --write to new file named "Sorted<oldfile>"



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
autumnize _ Empty               = Empty --if empty do nothing 
autumnize c (Leaf Green)        = Leaf c --if green, change to color c
autumnize _ (Leaf    _ )        = Empty  --if not green, remove (change to Empty)
autumnize c (Branch left right) = Branch (autumnize c left) (autumnize c right) {-if branch, call it recursively
                                                                                  on children -}


exampleTree :: Tree
exampleTree = (Branch (Leaf Green) (Branch (Leaf Yellow) Empty))

prop_autumnize :: Bool
prop_autumnize = autumnize Red exampleTree == Branch (Leaf Red) (Branch Empty Empty) 




----------------------------------------QUESTION 7-------------------------------------------------

f :: [Int] -> [Int]
f xs = xs --some function f, details not important

prop_sum_and_length :: [Int] -> Bool
prop_sum_and_length xs = length newList <= length xs && sum newList == sum xs 
    where
        newList = f xs




----------------------------------------QUESTION 8-------------------------------------------------


data Tree2 a = Empty2 | Node (Tree2 a) a (Tree2 a)
    deriving Show

--Node t1 nodeValue t2, the values in t1 are smaller than x and t2 are greater than x

member :: Ord a => a -> Tree2 a -> Bool
member element Empty2 = False
member element (Node left nodeValue right)
    | element < nodeValue     = member element left   -- if the element we're checking is smaller than current node's value check left subtree only
    | element > nodeValue     = member element right  -- if the element we're checking is greater than current node's value check right subtree only
    | otherwise               = True                  -- if we're on a non-empty node and the element is neither greater or smaller it has to be the same

prop_member x t = (member x t) == (x `elem` (flatten t))

flatten :: Tree2 a -> [a]
flatten Empty2 = []
flatten (Node lt x rt) = flatten lt ++ [x] ++ flatten rt




----------------------------------------QUESTION 9-------------------------------------------------

shuffle :: [a] -> Gen [a]
shuffle = shufHelp [] 

--helper function
shufHelp :: [a] -> [a] -> Gen [a]
shufHelp xs []     = pure xs   --if list is empty we're done
shufHelp xs (first:rest) = do
    n <- choose (0, length xs) --pick a random number between 0 and end of list
    let updatedList = putAt first n xs      --put y at nth place in new list 
    shufHelp updatedList rest             --do recursively 

putAt :: a -> Int -> [a] -> [a] --put x at nth place in list xs
putAt x n xs = h ++ (x:t)
    where
        (h,t) = splitAt n xs

--generate $ shuffle <[a]> to test it
