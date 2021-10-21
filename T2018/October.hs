import Test.QuickCheck


---------------------------------------------QUESTION 1--------------------------------------------

q1 :: Int -> [[Int]]
q1 n | n <= 0    = []
     | otherwise = replicate n 1 : q1 (n `div` 2)  

{-

Tip: check the type of replicate first. replicate :: Int -> a -> [a]

q1 4    -> replicate 4 1 : q1 2
q1 2    -> replicate 4 1 : replicate 2 1 : q1 1
q1 1    -> replicate 4 1 : replicate 2 1 : replicate 1 1 : q1 0
q1 0    -> replicate 4 1 : replicate 2 1 : replicate 1 1 : []
Result: [[1,1,1,1],[1,1],[1]] 


-}





---------------------------------------------QUESTION 2--------------------------------------------


lookupAll :: Eq a => a -> [(a,b)] -> [b]
lookupAll key [] = []
lookupAll key ((k,v):kvs) 
  | key == k  = v : lookupAll key kvs
  | otherwise = lookupAll key kvs

test = [("lucky",6),("unlucky",7),("lucky",8),("beastly",666)]

prop_lookupTest = lookupAll "lucky" [("lucky",6),("unlucky",7),("lucky",8),("beastly",666)] 
                  ==
                  lookupAll' "lucky" [("lucky",6),("unlucky",7),("lucky",8),("beastly",666)]


lookupAll' :: Eq a => a -> [(a,b)] -> [b]
lookupAll' key list = [v | (k,v) <- list, k == key] {-only save the value (v) 
                                                    but only when the key is correct (k == key) -}




---------------------------------------------QUESTION 3--------------------------------------------


data Ticket = Seasonal PersonType Year | Daily PersonType Year Month Day

--Either an adult or a child with a specified age
data PersonType = Adult | Child Age

type Age   = Int 
type Year  = Int 
type Month = Int
type Day   = Int


--Ticket for a child of age 13 for the 2021 season
exampleTicket :: Ticket
exampleTicket = Seasonal (Child 13) 2021





---------------------------------------------QUESTION 4--------------------------------------------

data Expr = Num Int | Add Expr Expr | Mul Expr Expr
    deriving (Show, Eq)

data Ex = NumEx Int | BinEx Op Ex Ex
    deriving (Show, Eq)
data Op = AddOp | MulOp
    deriving (Show, Eq)


convert :: Expr -> Ex
convert (Num n)          = NumEx n --base case 
convert (Add left right) = BinEx AddOp (convert left) (convert right) --convert recursively
convert (Mul left right) = BinEx MulOp (convert left) (convert right)



---------------------------------------------QUESTION 5--------------------------------------------

prop_lookup :: (Eq a, Eq b) => a -> [(a,b)] -> Bool
prop_lookup key map = case lookup key map of
                        --looking up in an empty list should give empty list or nothing
                        Nothing -> lookupAll key map == [] 
                        (Just x) -> head (lookupAll key map) == x 
                        --finding an element with lookup should be the 
                        --same element as the first lookupAll finds





---------------------------------------------QUESTION 6--------------------------------------------

type DayNumber = Int

closedDays, busyDays :: DayNumber -> DayNumber -> [DayNumber]
closedDays n m = [d | d <- [n .. m], guests d == 0]
busyDays n m = [d | d <- [n .. m], guests d > 2000]

guests d = d --not relevant

days :: (DayNumber -> Bool) -> DayNumber -> DayNumber -> [DayNumber]
days p n m = [d | d <- [n .. m], p d]

closedDays' = days (== 0)  
busyDays'   = days (>2000) 


---------------------------------------------QUESTION 7--------------------------------------------

names, emailProviders :: [String]
names = ["alice", "bob", "dave"]
emailProviders = ["gmail", "yahoo", "hotmail"]

spam :: Gen String
spam = do
    name <- elements names --pick one element from the list names
    email <- elements emailProviders --pick one element from the list emailProviders
    number <- choose (0,99) :: Gen Int --random int in the range of 0 - 99
    numberOrNot <- elements ["", show number]
    let complete = name ++ (show number) ++ "@" ++ email ++ ".com" --combine to one complete emailaddress
    pure complete --wrap it with Gen monad. return complete works aswell, pure == return (ish)


--To test: generate spam

---------------------------------------------QUESTION 8--------------------------------------------


data DTree = Decision Answer | Q Question DTree DTree
    deriving (Show,Eq)

type Question = String
type Answer   = String


attributes :: DTree -> [(Answer,[(Question,Bool)])]
attributes t = map reverseSndOfTuple $ listAllDecision t []
    where        
        --collect all decisions and answers in a list. 
        --All answers in a left subtree are true and right are false.
        --using a buffer to help
        listAllDecision :: DTree -> [(Question, Bool)] -> [(Answer, [(Question, Bool)])]
        listAllDecision (Q ques left right) buffer  = listAllDecision left ((ques, True):buffer) ++ 
                                                      listAllDecision right ((ques, False):buffer)
        listAllDecision (Decision answer)   buffer  = [(answer, buffer)]
        
        reverseSndOfTuple :: (a,[b]) -> (a,[b])
        reverseSndOfTuple (x,y) = (x, reverse y)


ex = Q "Is it raining" wet notWet
    where 
        wet    = Decision "Take the bus"
        notWet = Q "Is it more than 2km" (Decision "Cycle") (Decision "Walk")

prop_attributes :: Bool
prop_attributes = 
    attributes ex == 
            [ ("Take the bus", [("Is it raining", True)]),
              ("Cycle", [("Is it raining", False), ("Is it more than 2km", True)]),
              ("Walk", [("Is it raining", False), ("Is it more than 2km", False)])
            ]
