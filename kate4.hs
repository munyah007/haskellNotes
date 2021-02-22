--Integral includes Int and Integer 
--Num is the numeric type class whose members have the property of being able to act like numbers, includes integers and all real numbers 
--fromIntegral takes an integer number into a more general Num

newtype MyInt = MyInt Int deriving(Eq )

instance Ord MyInt where { MyInt i < MyInt j | odd i && odd j = i < j| even i && even j = i < j | even i  _ = True | otherwise = False}

   where odd x = (x `mod` 2)== 0
         even  = not.odd

-- class Entity a where 
getAge :: a -> Int
getFams :: a -> Int

We are defining a typeclass Entity with two methods ie , getAge and getFams ---- in other words we are saying here 
for a type 'a' to be an instance  of the class Entity it should have defined a function getAge  whichc when fed a returns Int

a type a is an instance of the Entity class if we can apply any of the two methods or functions on it

data Kiks = K { age, fams ::Int} -- this provides a way of avoinding repetition of Int , this case here shows that all before are of type Int

making Kiks an instance of Entity will then be done as follows :

instance Entity Kiks  where
getAge x = age x 
getFams x = fams x 

Note that it is assumed by default that the argument fed into the functions under instance declaration is the specific type we are making an instance 
thus by default kiks will be bound to x here 

if we do class Eq a => Entity a where  {getAge :: a -> Int ;getFams :: a -> Int}


here we are subclassing Entity to the Equatable , meaning before we make any type an instance of Entity it should be an instance of Equatable first

The monad class has the following class definition :

class Monad m where 
return :: a -> m a
fail:: String -> m a
(>>=) :: m a -> ( a -> m b) -> m b
 (>>) :: m a -> m b -> m b

>> can be defined as a >> x = a >>= \ _ -> x 
>> executes the action to its left but doesnt  pass it as an argument to the function in its front , ie if a was putStrLn " " it would do so 
but would not pass it as an argument to the next function which would just execute x 



x <- f 
g x   in the do notation is simply the same as f >>= \ x -> g x -- what is contexted or boxed inside f is what is fed into the function that comes after >>=
translataion rules of the do 
 do { e , es } -> e >> do { es } -- says if we have a do notation which has an action e followed sequence of actions es , we perform the first action e 
throw the result away -- ie here is where we bind it , then continue with more actions ie es

do { let decls , es } let decls in do {es} -- this shows how lets are handled inside a do , ie they are lifeted out , dealt with and then the results 
are used inside the do block
 do { p <- e ; es } let ok p = do {es } ; ok _  = fail "----" in e >>= ok --- if we bound something to ok then continue as usual else if there is no p
then error message , what we have here is a  pattern matched function description of ok

alternative formulation is 
 do { p <- e ; es }  -> e >>= \ p -> es , we run the action , give the result the name p and send the result into es

eg  foo = do ( 'a':b':c': x : xs ) <- getLine 

here we are pattern matching the string we get from getLine  and assuming it should start with "abc" but if thats not the case it will throw an 
ordinary compiler failure , but we could specify it using the do  above this one  , however in this case since are inside a monad we could specify the failure message , in the "....."

cross f g = do { a <- f ; b <- g ; return ( a,b)}
 
will produce a result similar to [ (a,b) | a <- f , b <- g ] we are just above treating lists as monads 

when the same cross function is used with Just 'a' , Just 'b'  Just ('a' , 'b') -- in this case it will be conforming to the behaviour of the maybe monad 

the implementation of that function without any explicit type declaration will make the compiler demand that the inputs f nd g be monads , for then they are the only
ones on which it will make sense to use  do and <- . Ie from the implementation the compiler implies the following type signature 
Monad m => m a -> m b - m ( a , b )

the type signature for liftM is (a -> b) -> m a -> m b --- it takes a normal function which takes a non monadic a to ptoduce a non monadic b and 
applies it to a monadic a to produce a  monadic b
>>= has a type signature  (a -> m b) -> m a -> m b
same applies with =<<  but this one takes arguments in a different order : (putStrLn =<< readFile "foo"   )
mapM :: ( a -> m b) -> [ a ] -> m b

mapM_ :: ( a -> m b) -> [ a ] -> m ()

filterM :: ( a -> m Bool ) -> [ a ] -> m [a]
sequence :: [m a] -> m [ a ]
sequence_ :: [m a] -> m ()
both sequences prints a sequence of actions , eg sequence [print 1 , print 2 ]....they can still be used for other things i believe
foldM:: (a -> b -> m a) -> a -> [b] -> m a
when:: Bool -> m() -> m()
join:: m (m a ) -> m a
all these are in the module Monad/Control.Monad

MapM_ print [ 1 ,2,3]
prints a list of things onto the screen

foldM (\ a b -> 
              putStrln (show x  ++ " + " ++ show " b  ="  ++ show ( x + b) ) >>
                return ( a + b ) ) 0 [ 1..5]

shows that in foldM a binds to the current state, this also shows that MapM requires that the return value be wrapped in a monadic context, ie the intermediate function should return a monadic value
code that prepends each line in a file with a number n 

prepender =  do 
 text <- readFile
 let l = lines text
 let pp = ZipWith (\ a b -> show a ++ ' ':b ) [ 1..] l
 MapM putStrLn pp
 see the function fed into MapM - it takes a normal argument and returns a monadic context

this code can be shortened by liftM as follows

prepender =  do 
    l   <- liftM   lines readFile ""
 
 let pp = ZipWith (\ a b -> show a ++ ' ':b ) [ 1..] l
 MapM_ putStrLn pp

the when function only executes when the condition is met 

MapM_ (\ l -> (when (not $ null l)) (putStrLn l) ) [ ] note the intermediate function takes a normal argument to return a monadic s is required by 
the type signature of MapM_

class Monad m => MonadPlus m where 
mzero:: m a
mplus :: m a -> m a -> m a
Monad module ought to be imported first
 for lists mplus is like ++ , for Maybe it is Nothing if both are nothing otherwise it is the first Just value

in case of lists the mzero is the empty list , in case of Maybe it is Nothing

instance MonadPlus Maybe where 
mzero = Nothing 
mplus Nothing y = y
mplus  x  _  =  x (remember the first Just values )

class MonadTrans t where 
lift :: Monad m => m a -> t m a

parse (char 'a') "stdin" "a"
Right a -- see the monads here , char 'a' ,"stdin" "a" , Right a

parse (char 'a') "stdin" "ab"
Right a

parse (char 'a') "stdin" "b"

Left  "stdin" (line 1 , column 1) :
unexpected "b"
expecting "a"

parse ( char 'H'>>  char 'a' c>>  char 'l')  "stdin" "Hal"
Right 'l'

Nb remember with Either Right represents success 

parse ( char 'H'>>  char 'a' c>> char 'l')  "stdin" "Hap"
Left  "stdin" (line 1 , column 3 ) :
unexpected "p"
expecting "l"

intList :: CharParser st [Int]

intList  = do 
char '['
intList' <|> ( char ']' >> return [])
 where  intList' = do 
       i <- Int
       r <- ( char ',' intList' ) <|>
       ( char ']' >> return [])
       return (i:r)

To parse is to read into the intended structure

parse intList "stdin" "stdin" "[ 1 ,2,3,4]"

Below is my of list that parses a list of integers 

takesNumberOut n [] = n ; takesNumberOut n (x:xs) = if not (Data.Char.isNumber x)  then n else takesNumberOut (n + 1) xs
myNumListParser (x:xs ) = case x of { '[' -> myNumListParser xs;',' -> myNumListParser xs; ']' -> [];' ' -> myNumListParser xs; _  -> (read (take (takesNumberOut 0 (x:xs )) (x:xs )) ::Int):(myNumListParser (drop (takesNumberOut 0 (x:xs )) (x:xs )));} 

-- above wont perform well with myNumListParser "[ 1,2,3, a,4,5]" , its just a matter of dealing with the characters in the logic

parse intList "stdin" "stdin" "[ 1 ,2,a,4]" will throw an error