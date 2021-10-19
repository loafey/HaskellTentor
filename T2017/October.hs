module T2017.October where

import Test.QuickCheck
import Data.List(isPrefixOf)
--- Question 1 ---
q1 :: [Int] -> Int
q1 [] = 0
q1 [x] = x 
q1 (x:_:xs) = max x (q1 xs)

{-
q1 (map abs [-1,-6,-5,7]) > q1 [1,6,5,7]
q1 [1,6,5,7]              > max 1 (q1 [5,7])
max 1 (max 5 (q1 []))     > max 1 (max 5 0)     > max 1 5   > max 1
-}

--- Question 2 ---

type WeekNumber = Int
rainfall :: WeekNumber -> Double    -- assume this function exists
rainfall n = undefined 

dryWeeks :: WeekNumber -> Int
dryWeeks n | n < 1          = 0
           | rainfall n < 5 = 1 + dryWeeks (n - 1)
           | otherwise      = dryWeeks (n - 1)

--- Question 3 ---
type Year   = Int
type Month  = Int
type Day    = Int
type Hour   = Int
type Minute = Int

data Date = Date Year Month Day 
data Time = Time Hour Minute

data BusTicket = SingleTicket Date Time | PeriodTicket  Date

--- Question 4 ---
data Expr = X | Num Int | BinOp Op Expr Expr
    deriving (Eq,Show)

data Op = Add | Mul | Subtract
    deriving (Eq,Show)

removeSub :: Expr -> Expr
removeSub X                      = X
removeSub (Num i)                = Num i
removeSub (BinOp Subtract e1 e2) = BinOp Add e1 (BinOp Mul (Num (-1)) e2)
removeSub (BinOp o e1 e2)        = BinOp o (removeSub e1) (removeSub e2)

prop_removeSub = removeSub (BinOp Subtract (Num 100) X) == BinOp Add (Num 100) (BinOp Mul (Num (-1)) X)

--- Question 5 ---
prop_isPrefixOf = "hell" `isPrefixOf` "hello"
               && []     `isPrefixOf` [1,2,3]
               && [1,2]  `isPrefixOf` [1,2]
               && not ([2,3] `isPrefixOf` [1,2,3])

prop_take :: Int -> String -> Bool
prop_take i s = take i s `isPrefixOf` s

--- Question 6 ---
data Suit = Hearts | Clubs | Diamonds | Spades
    deriving (Eq,Show)

data Rank = Numeric Int | Jack | Queen | King | Ace
    deriving (Eq,Show)
    
data Card = Card Rank Suit 
    deriving (Eq,Show)
    
isRed, isDiamond :: Suit -> Bool
isRed s             = s == Hearts || s == Diamonds
isDiamond s         = s == Diamonds

isAce, isLow :: Rank -> Bool
isAce r             = r == Ace
isLow (Numeric n)   = n < 5
isLow _             = False 

lowDiamonds cs = [Card r s | Card r s <- cs,  isLow r && isDiamond s ]
redAces cs     = [Card r s | Card r s <- cs,  isAce r && isRed s ]
lowRedCards cs = [Card r s | Card r s <- cs,  isLow r && isRed s ]

selectCards :: (Rank -> Bool) -> (Suit -> Bool) -> [Card] -> [Card]
selectCards fr fs cs = [Card r s | Card r s <- cs, fr r && fs s]

prop_selectCards cs = lowDiamonds cs == selectCards isLow isDiamond cs
                   && redAces     cs == selectCards isAce isRed cs
                   && lowRedCards cs == selectCards isLow isRed cs


--- Question 7 ---
quadlist :: Gen [Integer]
quadlist = do
    len <- arbitrary
    vectorOf (abs len * 4) arbitrary