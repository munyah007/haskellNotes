// compiling an executable - stack ghc < filename with extension > // do this in a fresh cmd without gchi loaded
// running a compiled executable -- stack runghc <name> // do this in a fresh cmd without gchi loaded
// simple file reading and file writing 
 main = do 
        inputStr <- readFile "input.txt"
        writeFile "output.txt" ( map Data.Char.toUpper inputStr )

// before creating an executable first load the file for it to compile , if it loads successfully then bulid the exec, to make what the code in the compiled file
happen you will then have to run the executable

// Files can also be read and run directly from the command prompt without the need for an executable
input <- readFile "kate" - binds the read file to the input variable input, that input word there can be any other word 
// in other cases where the structure of input is not so clear you have to read it explicitly let kik = (read input) :: String
// writing a file is writeFile ( show d1 ) -- where d1 is a defined data structure
// maximum of a list
maxyt :: (Ord a) => [a] -> a ;maxyt [] = error "empty list";maxyt [x] = x;maxyt (x:xs) | x > maxyt xs  = x |otherwise    =  maxyt xs


  // replicator
  
repst:: (Eq i , Num i ) => i -> a -> [a]; repst 1 el  = el:[] ;repst n el   = (el:(repst(n-1) el))
  
  // taker function
  takesT n (x:xs)   |  n <= 0 = [] | ((x:xs)==[]) = [] | n >=  ( length (x:xs) ) = (x:xs) | otherwise = (x : takesT (n-1) xs) 

takesT (-1) [ 1 , 2 , 3 , 4 , 5, 6 , 7]

// reverser function

rev [] = []
rev xs = [ (xs !! y ) | y <-  [((length xs) - 1 ) , ((length xs) - 2 )..0]]

// element function
comps  el xs = [(if (el == x) then 1 else 0)| x <- xs ]

ely el xs  =   if ((sum  (comps el xs )) > 0 ) then "Yes" else "No"

ely 2 []

// recursive element function
  
 eleM a [] = False ; eleM a (x :xs )  | ( a== x ) = True  | otherwise  = eleM a xs

// sorting function 
 maxRemovesyzc [x] = [];maxRemovesyzc xs = [ x  | x <- xs , (x /= (maxyT xs ))]
 
 

 sorteZp [x] = [x];sorteZp  xs = ((maxyt xs) : (sorteZp (maxRemovesyzc xs)))
pairSt [ ] [ ] = [ ]; pairSt (x: xs) (y: ys) = ( x , y ) : pairSt  xs ys




mult x y = x * y

zipWithtt f xs ys = [ f (fst x) (snd x ) |  x <- (pairSt xs ys) ]

zipWithtt mult [ 2,2,2] [ 4,4,4]
 
largestDiV =  head (filter p [ 100000 , 99999 ..]) where p x = ((mod x 3829) == 0);
length [ x | x <- [ 1 .. 100000], (mod x 3829) == 0]
// the code below will create a list that doesnt have a closing bracket and which crashes the repl
sum [x ^ 2 | x <- [ 1,2..] , x^2 <= 10000]

 colg  1 = [1]; colg x | odd x = x :colg ((x * 3 ) + 1) |even x = x :colg (div x 2);
  
  length  [ colg x | x <- [ 1 .. 100] ,  (length (colg x )) > 15 ]

colg 30

 elem' el xs  = foldl  (\ ==) el xs     

els y ys = foldl (\ acc x  -> if ( x ==  y) then True else acc ) False ys
  eleMine y  [ ] = False ;eleMine y (x:xs)  = if (y == x ) then True  else eleMine y xs

myMapp f xs = foldr (\ x acc -> (f x ): acc)  [] xs
// NB main difference between foldr and foldl ia that foldl has the parameter from list as the second para and foldr has it as the first

   revvvy [x] = [x ] ; revvvy (x:xs ) = revvvy xs ++ [x]
 // my Mapp using left fold
 myMappL f xs = revvvy (foldl (\ acc x -> (f x ): acc)  [] xs)

myMax xs = foldl (\ acc  x -> if (acc > x ) then acc else x ) (head xs) xs 

myRiv  xs = foldl (\ acc x -> x : acc ) [] xs 

myProd xs = foldl (\ acc x -> x * acc ) 1 xs 

myFilter f xs = foldr (\  x acc -> if (f x) then x : acc  else  acc ) [] xs
 myHead  xs   = foldl1 ( \ x _ -> x ) xs 
 myLast xs   =  foldr1 ( \ _ x -> x ) xs 

length (takeWhile (< 1000 ) (scanl (+) 0 (map (\ x -> x^2 ) [1..100] )))

// right associative (z(g(f x))), left associative (((f x) d)e)


 subListSearchst subL [] = False; subListSearchst subL xs | ( head subL ==  head xs ) =  if (fst (splitAt  (length subL) xs )) == subL then True else   subListSearchst subL (tail xs)|otherwise = subListSearchst subL (tail xs) 


findAdDp key xs = (if  (elem key (map (\ x -> fst x) xs ) ) then [ snd x | x <- xs , fst x == key ] else [7777] )
findT  key xs  = [ snd x | x <- xs , fst x == key ]

 // the if has to return the same type in both branches or else it wont evaluate

// my map creator

mapCreator []   = Data.Map.empty ; mapCreator ( (b , a):xs ) =Data.Map.insert b a mapCreator xs // does not work
mapCreates  = foldl ( \ acc (  x , y )  -> Data.Map.insert x y acc) Data.Map.empty  // foldl has the argument from the coll on the second psn

Data.Map.lookup "a" (mapCreates [ ("a" , 1), ("b" , 2)] )

let set1 =  Data.Set.fromList "KateHlomaniNokutendaK"
let set2 =  Data.Set.fromList "KateHlomani"

Data.Set.isProperSubsetOf set2 set1 
 data Car = Car { company :: String , owner :: String , puchasePrice :: Int }

let mine = Car { company = "Benz" , owner = "Hlomani" , puchasePrice = 100000 }

//the fields are the functions and the instantiations are the arguments , eg do company mine to get Benz

Data.Map.lookup "c" (Data.Map.fromList [ ("a" , 1) , ("b" , 2)])

multsb x = x -1 
 // the next 2 functions work together to implement character to upper case 
retTailWhenEqEncountered  el (x : xs ) = if el==x  then  xs else  (retTailWhenEqEncountered  el xs )

charToUp cha = if (elem cha [ 'A' ..'Z' ] ) then cha else (  [ 'A' ..'Z' ] !! ((26 - (length (retTailWhenEqEncountered  cha  [ 'a'..'z']))) -1 ) )




lowerLeters str = (foldl (\ acc x -> if (elem x [ 'a'..'z']) then  (acc + 1 ) else acc ) 0 str )

maxFromFold xs = (foldl (\ acc x -> if (acc > x )  then acc else x ) (head xs ) xs )

prac 1 = 1 ; prac x = x * prac (x-1)

myFibo 1 = 1;myFibo  2 = 1 ;myFibo x = (myFibo (x- 1) + myFibo (x-2))

multz 1 b = b ; multz a 1 = a ; multz a b = b + multz (a-1) b

myMap f [] = [] ; myMap f (x:xs) =  (f x ) : myMap f xs

myMap ( \ x ->  x - 10 ) [ 1,2,3,4,5]


// The functions below create a palindrome all contribute to the one that test palindromy in a passed list

myRiv  xs = foldl (\ acc x -> x : acc ) [] xs 
palindromer [] = [] ; palindromer  xs = xs ++ (myRiv xs )
takerFirstHalf [] = []  ; takerFirstHalf xs = (foldl (\ acc x -> if  (fromIntegral (length acc ) ) == ((fromIntegral (length xs ) )/ 2 ) then acc else x : acc ) [] xs )
takerLastHalf [] = []  ; takerLastHalf xs = (foldr (\  x  acc -> if  (fromIntegral (length acc ) ) == ((fromIntegral (length xs ) )/ 2 ) then acc else x : acc ) [] xs )
paliTester [] = [] ; paliTester xs = if (takerLastHalf xs ) ==  (takerFirstHalf xs ) then "Yes its a Palindrome" else "No it aint"

// the two functions below sort a list by starting from the smallest
  minMum xs =  foldl ( \ acc x -> if ( acc < x ) then acc  else x ) (head xs ) xs 
  sorterZ [] = [] ; sorterZ [x] = [x] ; sorterZ (x :xs ) =  [(minMum  (x: xs ))] ++ sorterZ [ y | y <- (x: xs ) , y /=  (minMum  (x: xs )) ]
// the two functions below sort a list of lists on the basis of length 
minMumL xs =  foldl ( \ acc x -> if ( (length acc ) < (length x )) then acc  else x ) (head xs ) xs 

sorterY [] = [] ; sorterY [x] = [x] ; sorterY (x :xs ) =  [(minMumL  (x: xs ))] ++ sorterY [ y | y <- (x: xs ) , y /=  (minMumL  (x: xs )) ]

// The functions below are solutions to number 10, 11 , 12 of Real world Haskel page 110

data Point = Point ( Int , Int ) deriving (Show , Eq )

pointX ( Point ( x , y )) = x
pointY ( Point ( x , y )) = y

data Directions =  Straight   Point  Point  Point  |Left   Point  Point  Point | Right  Point  Point  Point deriving ( Show , Eq )
 
grad p1 p2 = (fromIntegral (( pointY p1 ) - ( pointY p2 ))) / (fromIntegral (( pointX p1 ) - ( pointX p2 )))

direcCalc p1 p2 p3 | (grad p1 p2) == (grad p2 p3) =  Straight p1 p2 p3 | ((grad p1 p2 ) * (grad p2 p3 ) ) < 0 = Left  p1 p2 p3 | otherwise = Right p1 p2 p3

tripler ( x:y:[]) = [] ; tripler ( x:y:z:[]) = [[ x, y ,z ]] ; tripler ( x:y:z:ds) = [ x,y ,z ] : tripler ( y : z :ds )

groupAndDirectionCalc xs = map (\[ x , y  , z ] -> direcCalc x y z ) (tripler  (map (\ x -> Point x ) xs ) )

myTakeWhileR f [] = [] ; myTakeWhileR f (x:xs ) = if  ( f x )  then ( x : myTakeWhileR f xs ) else  myTakeWhileR f [] 

myGroupBy f [] = ([],[]) ; myGroupBy f xs = (foldl (\ acc x -> if (f x ) then (( x : ( fst acc)) , (snd acc)) else ((fst acc) , (x : (snd acc)))  ) ([],[]) xs )
begSpaceRemover (x : xs ) = if (isAspace x) then (begSpaceRemover xs) else (x : xs ) 
myWords xs =  let ( pref , suf ) = (break (\ x -> x==' ')  xs ) in  if ((length suf )== 0 ) then (pref : []) else (pref : myWords  suf)
NB "" is considered as a list of characters with length 0 it will not match []
// A single element list matches the ( x : xs ) pattern were the xs is []
// below is an easy way to construct working case and let  constructs
fd x = case x of { 0 -> 1 ;   1 -> 10 ; _ -> 100 }
fdl x = let { z = 2 * x ; y = x ** 2 } in y * z

remember f.g = (f.g )arg_here  or f (g x)

-- remember the two directions of recursion , forward in a generalised fashion then backwards with specific

