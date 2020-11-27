isPrime x = (not ( any ( == 0 ) [ ( mod x y ) | y <- [ 2..( x-1 )]] ))

factors x = [ y | y <- [1..x] , ((mod x y ) == 0 )]
gcd x y =  last (Data.List.sort (Data.List.intersect (factors x) (factors y)))
coprime x y = ((gcd x y)==1)

coprime 35 64

eulerTotientPhi x = length [ (coprime x y ) | y <- [ 1 ..(x-1)] , ((coprime x y )==True)  ]

eulerTotientPhi 10

primeFactors x  = Data.List.sort ([  y  | y <- ( factors x) , (isPrime y ),( y/=1)])


divideByAlist _ [] = []; divideByAlist y ( x: xs ) | y==x = (x : divideByAlist ( div y x )  xs) |((primeFactors ( div y x)) !! 0) == x = (x : divideByAlist ( div y x ) (x : xs )) | otherwise = (x : divideByAlist ( div y x )  xs)


primeFactorsWithMultiplicity x =  [ (head y , length y) | y <- Data.List.group (divideByAlist x  (primeFactors x )) ]

primeFactorsWithMultiplicity 8 


improvedTotient x = (foldl (\ acc x -> (((( fst x)-1)*( fst x))^((snd x)-1) ) * acc ) 1 (primeFactorsWithMultiplicity x))

primeFactorsWithMultiplicity 10

improvedTotient 10

primesInRange x y = [ z | z <- [x .. y], (isPrime z)]

primesInRange 10 20

powerset [] = [[]] ; powerset (x:xs) = (map (x:) (powerset xs)) ++ (powerset xs)


getIndicatedIndeces motherSet indeces = ( foldr (\ x acc-> ((motherSet !! x) : acc)) [] indeces )


kCombinationsFromNthings xs k  = [(getIndicatedIndeces xs y) | y<- [ x | x <- (powerset [ 0..((length xs)-1)]) , ((length x) == k)] ]
goldBach x = [ z |z <-(kCombinationsFromNthings (primesInRange 2 x ) 2 ), ( sum z)==x ]

goldBach 28


evensInRangeAndgoldBach x y = [ (k , (goldBach k )) | k <- [ x..y ] , ( even k )]

evensInRangeAndgoldBach 9 20

bothGreater z [ x ,y ] =   ( x > z ) && ( y > z)

evensInRangeAndgoldBach' x y z =  [ (fst k , (filter (\ p -> bothGreater z p) (snd  k ))) | k <-(evensInRangeAndgoldBach x y) , (any (\ t -> (bothGreater z t )) ( snd k ) ) ]


evensInRangeAndgoldBach' 4 2000 50   ((fst k) , ( filter (\ x -> ((x !! 0 ) >z) && ((x !! 1 ) > z)) (snd k) )) 








