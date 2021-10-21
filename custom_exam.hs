----------------
----------------
---QUESTION 1---
----------------
----------------

q1 :: Int -> [[Int]]
q1 n | n <= 0    = []
     | otherwise = replicate n 1 : q1 (n `div` 2)  

{-

Tip: check the type of replicate first. replicate :: Int -> a -> [a]

q1 4    -> replicate 4 1 : q1 2
q1 2    -> replicate 4 1 : replicate 2 1 : q1 1
q1 1    -> replicate 4 1 : replicate 2 1 : replicate 1 1 : q1 0
q1 0    -> replicate 4 1 : replicate 2 1 : replicate 1 1 : []

computing replicate ->    [1,1,1,1] : [1,1] [1] : []

Result: [[1,1,1,1],[1,1],[1]] 
-}



----------------
----------------
---QUESTION 2---
----------------
----------------

type WeekNumber = Int
rainfall :: WeekNumber -> Double    -- assume this function exists
rainfall n = n --so dryWeeks can be run

dryWeeks :: WeekNumber -> Int
dryWeeks n | n < 1          = 0
           | rainfall n < 5 = 1 + dryWeeks (n - 1) --if the rainfall is greater >= 5 then increase the count of dry weeks
           | otherwise      =     dryWeeks (n - 1) --keep looking at earlier weeks

----------------
----------------
---QUESTION 3---
----------------
----------------

sortLines :: FilePath -> IO ()
sortLines file = do
     f <- readFile file --save the contents of the file to f
     let ans = unlines (sort (lines f)) --split word in to list, sort list, convert back to string 
     writeFile ("Sorted" ++ file) ans --write to new file named "Sorted<oldfile>"



----------------
----------------
---QUESTION 4---
----------------
----------------

data Expr = X | Num Int | BinOp Op Expr Expr
    deriving (Eq,Show)

data Op = Add | Mul | Subtract
    deriving (Eq,Show)

removeSub :: Expr -> Expr
removeSub X                      = X
removeSub (Num i)                = Num i
removeSub (BinOp Subtract e1 e2) = BinOp Add (removeSub e1) (BinOp Mul (Num (-1)) (removeSub e2) --(5 - 3) => (5 + (3 * -1))
removeSub (BinOp o e1 e2)        = BinOp o (removeSub e1) (removeSub e2) --recursively call the function on the children

prop_removeSub = removeSub (BinOp Subtract (Num 100) X) == BinOp Add (Num 100) (BinOp Mul (Num (-1)) X)



----------------
----------------
---QUESTION 5---
----------------
----------------

replace :: Int-> a -> [a] -> [a]
replace _ _   []       = []
replace 0 new (old:xs) = new:xs
replace i new (old:xs) = old:replace (i-1) new xs

modify :: Int -> (a -> a) -> [a] -> [a]
modify _ _ []       = []
modify 0 f (old:xs) = f old : xs --if we're at the correct index, apply f on old and add it back in the list
modify i f (old:xs) = old   : modify (i-1) f xs --if we're not at the correct index keep looking

prop_modify :: [Bool]
prop_modify = [p2]
    where
        --p1 = modify 6 toUpper "Happy new Year!" == "Happy New Year!" 
        p2 = modify 2 (*100) [5,6,7,8,9] == [5,6,700,8,9]



----------------
----------------
---QUESTION 6---
----------------
----------------

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



----------------
----------------
---QUESTION 7---
----------------
----------------

names, emailProviders :: [String]
names = ["alice", "bob", "dave"]
emailProviders = ["gmail", "yahoo", "hotmail"]

spam :: Gen String
spam = do
    name         <- elements names --pick one element from the list names
    email        <- elements emailProviders --pick one element from the list emailProviders
    number       <- choose (0,99) :: Gen Int --random int in the range of 0 - 99
    numberOrNot  <- elements ["", show number]
    let complete = name ++ (show number) ++ "@" ++ email ++ ".com" --combine to one complete emailaddress
    pure complete --wrap it with Gen monad. return complete works aswell, pure == return (ish)



----------------
----------------
------EXTRA-----
----------------
----------------

addingMachine :: Int -> IO ()
addingMachine i = do
    putStrLn $ "Sum so far: " ++ show i 
    putStr "Enter next number: "
    inputString <- getLine --get input from the user, extract the value from IO String to String
    let input = read inputString --read the string input as an int (ghc knows it's an int because of the next line)
    addingMachine $ input + i --recursively run the function with the added number
