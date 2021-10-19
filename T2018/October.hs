
data DTree = Decision Answer | Q Question DTree DTree

type Question = String
type Answer   = String


attributes :: DTree -> [(Answer,[(Question,Bool)])]
attributes (Q question left right) = undefined 



ex = Q "Is it raining" wet notWet
    where 
        wet    = Decision "Take the bus"
        notWet = Q "Is it more than 2km" (Decision "Cycle") (Decision "Walk")

prop_attributes :: Bool
prop_attributes = attributes ex == [
                                    ("Take the bus", [("Is it Raining", True)])
                                    ("Cycle", [("Is it raining", False), ("Is it more than 2km", True)])
                                    ("Walk", [("Is it raining", False), ("Is it more than 2km", False)])
                                   ]
