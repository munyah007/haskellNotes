myMax [x] = x; myMax (x:y:xs) = if (myMax [x])> (myMax [y]) then (myMax ( x:xs ) ) else (myMax ( y:xs ) )

myMax [ 1,2,300,4,5,6]

theirMax [x ] = x ; theirMax  (x : xs ) |

replicare' 1 x = [ x ] ;replicare' n x = x : replicare' (n-1 ) x

 recTake 0 ( x:xs) = [] ; recTake n ( x:xs) = x :  recTake (n-1) xs 

recTake 12 (replicare' 9 8)
 recTake 0 ( x:xs) = [] ;  recTake n [] = [] ;recTake n ( x:xs) = x :  recTake (n-1) xs 

myRecRev [] = [] ; myRecRev ( x:xs) = myRecRev xs ++ [x]

myRecRev [ 1,2,3,4,5]

myZip xs [] = [] ;myZip [] xs = [] ; myZip (y:ys) (x:xs ) = ( y , x ) : myZip ys xs

myZip [ 1,2,3,4,5] [ 11,12,13 ]

recElem el [] = False ; recElem el ( x:xs) = if (el == x) then True else recElem el xs
removeFirstMatch el [] = [] ;removeFirstMatch el (x : xs )  = if  (el == x) then xs else (x : (removeFirstMatch el xs))

sorter [] = [] ; sorter xs =    ( sorter (removeFirstMatch (myMax xs) xs) )  ++ [(myMax xs)]

sorter [ 1,5,6,3,4,7,7,3 , -1 ]

function composition is defined in such a way that it takes only one parameter 

ie f.g = \x -> f ( g x) 

so sum ( replicate 5 ( max 6.7 8.7 )) shoud be writen as sum.replicate 5 . max 6.7 $ 8.7 or (sum.replicate 5 . max 6.7 ) 8.7