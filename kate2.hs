data NestedList a = Elem a | List [ NestedList a ] deriving (Show , Read )
-- this creates a data type known as nestedList which contains two possible types both of which can be in the same haskell list
-- remember that haskell lists take only one type
removeNth  [] n = [] ; removeNth xs n = ( foldr (\ x acc -> if ( ((length xs)  - ((length acc ) + 1 )) == n ) then acc else x:acc ) [] xs )
-- in the code above once foldr meets the condition it stops , doesnt skip the element and continue , the code was meant to remove the element at the supplied index

adderOneMore [] = [] ; adderOneMore xs = adderOneMore (xs ++ [((last xs) + 1)])

take 10 ( adderOneMore [1])
-- the code above does not terminate , meaning a function that recursively calls itself does not produce a lazy sequence
-- the only way a recursive call terminates is by way of an edge case and never by a take n
-- any predicate testList
-- when some functions used in some high order functions are redefined the higher order function will still hold on to the old definitions and
-- if it is to reference the new definitions then it has to be reentered into the repl
-- the predicate in a list comprehension shouuld test the evalation before | or any drawn variables
