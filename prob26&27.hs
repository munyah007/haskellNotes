
powerset [] = [[]] ; powerset (x:xs) = (map (x:) (powerset xs)) ++ (powerset xs)


getIndicatedIndeces motherSet indeces = ( foldr (\ x acc-> ((motherSet !! x) : acc)) [] indeces )

getIndicatedIndeces [ 'a' , 'b','c','d' ] [ 0 , 2 ]

kCombinationsFromNthings xs k  = [(getIndicatedIndeces xs y) | y<- [ x | x <- (powerset [ 0..((length xs)-1)]) , ((length x) == k)] ]

kCombinationsFromNthings [ 'a' , 'b','c','d' ] 3

notInXs test xs = ( not (any ( `elem` xs ) test))


disJointGroups xs a b c = [ [ x , y  , z ] | x<-(kCombinationsFromNthings xs a) ,y <- (kCombinationsFromNthings xs c) ,z<- (kCombinationsFromNthings xs b)  ,notInXs x y , notInXs x z , notInXs y z] 
 
length (disJointGroups "abcdefghi" 2 2 5 )
 
