Thinking of type constructors as boxes that hold a single or many values , eg [] or Maybe a , functors apply a function to the contained value or vaues
eg f (t a) -> t ( f a).


What we make instance of the Functor class is a type constructor ; ie when  we do that we then go on to define implementations of functions
of  the functor type signature ( a -> b) -> f a -> f b
Say we had just created the list type constructor and wanted to make it instance of Functor 

instance Functor [] where
fmap [] g = [ g a ] -- in this case g is that function whose type signature is a -> b

Making a type constructor ( not value constructor ) an instance of the Functor typeclass enables us at the outset to define functions that can reach 
inside its  boundaries and operate on its contents
the type constructor that we make an instance of the Functor class has to take ony one type , if it takes more than one we have to partially apply it so that 
it ends up taking one type around which the functor definition will dwell
Note what we make the instance of the functor is a type constructor not a concrete type
when making a type constructor an instance of the Functor type class the neccessary implementations to be defined are such as cover 
all the possible value constructors that exist for that particular type constructor 
ie - if we were making Maybe [ not Maybe a  which is a concrete type ] an instance of the Functor type class then the neccessary implementations of fmap
would be how it should behave when it encounters Just a or Nothing , which are the exhaustive set of all the values that the Maybe a type can take

in the type signature of fmap in the functor class ie (a->b) -> f a -> f b
what we are making instance of the type class is the type constructor f ,
Functors are effectively  types that can take many forms - ie polymorphic or that can be boxes containing a concrete type 
The IO is a type constructor and as such can be made an instance of the Functor type class
return is a function that wraps a resut in an IO context
fmap of functors has different ways of implementation which depend on the specifically defined way/s of implementation as defined when the particular type
constructor is made an instance of the functor eg:
instance Functor IO where
 fmap f action = do 
         result <- action
         return ( f result ) <- binds what is inside the action box to the result, return produces the result of  f result in a functor context

calling a function over a functor should just map the function over the functor and nothing more
Functor laws  :
If we map the id function over a functor then the functor we get should be as the original functor ie fmap id (t a) must be equal to  id (t a )


fmap (f.g ) must be equal to fmap f.fmap g

remember (f.g) x = ( f (g x))

fmap is a polymorphic function applicable to all the types that are an instance of the functor type class, its applicability follows the laws that
are defined when the particular type under consideration is instantiated , ie fmap behaves differently depending on what type it encounters
For a type to be a functor it shouldnt just be a part of the functor class but it should obey the functor laws 
Using a type class that does not satisfy both functor laws as a functor will result in faulty code
fmap (+ 3) (* 3) is the same as (+ 3).(*3) its composing which is the same as (\ x -> (( x * 3 ) + 3 ))
ie when fmap is called with functions its the same as function composition

Applicative functors become necessary when the stuff stored in our box is a partially applied functions which we have to relate with other functors 
whose contents can be parameters to our box of partially applied functions - a process which fmap cannot achieve
Making type constructors instances of the functor class is just a matter  of showing how fmap can be applied to them with a function f

the type of (++ "Kate") is ( [Char]->[Char]) the ( shows its a function , and the whole statement reads like it is a function that takes a
list  of characters and returns a list of characters
:t fmap compare "A LIST OF CHARACTERS" is [ (Char -> Ordering) ] shows that the result is a list of functions which take a Char to produce an ordering
For something to be made an instance of the applicative functor class it first has to be a an instance of the functor type class ( ie there is a class constraint)
class (Functor f ) => Applicative f where
pure :: a > f a
<*> :: f (a -> b ) -> f a -> f b - note here the function is wrapped in the constructor, ie the partially applied function
the function defined as pure in the instantiation must conform to the pure type signature , ie - it must take a concrete type a and produce the type f a
the function defined <*> must take a  function with the signature ( a -> b ) and the type f a to produce f  b

Type constructors can also be partially applied , ie let k = Just 

mean k is a Just ready to take a value, ie k 2 would yield Just 2

In making a type class an instance of the functor or applicative functor the implementations focus on the value constructors of that type class
 typical implementation of <*> is <*> Just f something = fmap f something , here <*> takes the function inside the Just and then uses a functor
approach to apply it to what is inside another functor ( the something there has to be a functor )

f <*> x = fmap f x

pure f <*> y <*> x allows us to use parameters wrapped in functors on a function that otherwise expects parameters not wrapped in functors

Note pure wraps f in a wrapper necessary because <*> requires that the function be wrapped 
the pure there is sort of a way of wraping the f in the functor first because <*>  takes a function that is inside a functor, without the pure the
same code can be written as fmap f y <*> x - remember fmap wraps the constructor on the function thus once it is wrapped now <*> is applicable
the same feat can be accomplished with f <$> x , it translates to fmap f x

f x y z is function call for f when the parameters are non applicative functors , but if they are then f <$> x <*>y<*> z , the <*> is neccesitated by the need
to re use partially applied  wrapped function ( use with a single wrapped functor produces a wrapped function
 and once wrapped normal fmap no longer works without pattern matching hence the need to go applicative )
 which still has to reach into another wrapper , if it was a case of one functor parameter simple f map would suffice
<*> is left associative ie it eats from the left

pure t is defined differently for different contexts so when the result is not used 
then fmap f x would do
IO actions are applicative functors , ie getLine acts as a box conraining what it fetches

return takes an expression and wraps it in an output sense--- its the only vehicle capable of going into outer space , so return (f x ) qualifies that as something
that can be taken to the outside world

NB : f <*> g  = \x -> f x  ( g x) in dealing with functions as applicative functors this is the first important definition

 (+) <$> (+3) <*>(*100) $5 develops to   (+) ((+ 3 ) 5 ) ( (* 100 ) 5 ) this leads  to the next  concept that guides the application of  of 
functions as applicative functors

use of <*> with functions creates a function which applies one argument -- after $ to all that comes after <$> and then takes all the results
as arguments to the function  that is before <$>

 
there are two instances of applicative functors - data types and partially applied functions



doing k <$> f <*> g  creates a function that will call k with the eventual results of f and g , ie we are using k on the future return values of f and g.
 

This here concerns the use of functions as applicatives - the normal thing is to use a partially applied function and another wrapped value
 Normal non applicative functors seek to apply a wrapped value or values  to an unwrapped function to produce a wrapped value of course
Applicative functors seek to apply mostly a partially applied function wrapped in a box and  produced as a result of the normal
functor operation given above-- fmap produces a wraped value and if such a value were a partially applied function yet to  be applied to another boxed or wrapped parameter
then we would have to resort to applicatives ie <*> and <$>, if the parameter to be applied were not wrapped yet the function was wrapped then  to apply applicatives we woud still
 need to wrap it using normal type constructor used in instantiating the applicative functor

for lists the instantiation is instance Applicative [] where 
                                  pure x = [x]
                             fs <*> xs = [ f x | f <- fs ,x<-xs ]
remebering that in a list comprehension of that kind each element on the left list ie fs is applied to every element on the right list
ie - its a smart way of generating all possible combinations in this case of  two member teams 

part of the the implementation of ZipList is 

ZipList fs <*> ZipList xs = ZipList (zipWith ( \ f x  -> f x ) fs xs )

here the type constructor is ZipList so boxing normal lists into Ziplist will save the trouble of having to write the zipWith function 

With  normal functors we can only map a function over a single functors whereas with applicative functors we can map a function over several functors

<$> is effective for producing in functor context a partially applied function which the only way that it can be applied is when it is applied 
to a functor type argument using the special extractor ie <*>

functions like liftA2 and <*> are only applicable to applicative functor  types , that is why when we want to use [] as a parameter to these we have to prefix it with
pure , that makes it an applicative functor and thus usable with both the noted functions

One simple way to take is sequence Creates a function that applies the same argument far right argument to all partially applied functions in a list then presents the results 
in a list 
its a rule that f <$> g <*>h<*>t  creates a function that needs the same argument same  f ( g same ) (h same ) (t same ) 
((+) <$> (+4) <*> (+6))  4 gives 18 and its similar to (sum (sequenceA [ (+4) , (+ 6) ]) 4) - easiest way to take it is underneath it sequenceA uses liftA2 and 
underneath it liftA2 uses <$> and <*> which have the result in first case - ie creates a function where u can apply the same argument to all the functions
 then finally you apply <$>, its a feat that can be easily implemented using usual map 
The major reason we make types instances of a certain type class is because we want them to conform to a certain behaviour ( why not just
use basic pattern matching why go through the trouble ?? question to Hilary, is it jsut to avoid possible bugs )
the newtype keyword is used when we want to make a new type that is just the result of wrapping an existing type, the new type should just have 
one parameter and one value constructor which has one field 
A monoid is when you have a binary associative function.....and a value ( dubbed the identity value )
 which when used with that function and another value always acts as an identity function

On the fail section of the monads in the implementaion , the fail section is implemented for the specific context in which the class in question will have failed 
>>= extracts what is inside a box and applies it to a function that takes it in that extracted form , to produce a response that is wrapped in the same context
the syntax of >>= requires that the argument or parameter goes to the left and the function :: ( a -> f b) goes to the right and remember to the said parameter is
:: f a ... f here is a type constructor
with a do block  each line must feature a monadic value
Monadic  values are values with a possible context of failure

In using a Do block , the final result has to be wrapped in the same context as the value fed into the variable(say x) by <-, when the final expression is not of that 
type it violates the monadic expectation .... ie it has to conform to the monadic type signature, so when the function produces a return value that is not wrapped you have to wrap it by 
using return ......... ie - the following are the same [kkkkk = do  x   <- Just 9 ;  return ( x > 10 ) ] [ kkkkk = do  x   <- Just 9 ;  Just ( x > 10 ) ]
the [] are just for separation.... what the return does is wrap in the appropriate context according to the monads in operation , its kinda similar to read 
 which can pick what type to read as from the context of the expression

In a do block the <- has the effect  of binding the contents of the wrapper to what is on the left side of <- , ie x <- Just 4 , binds 4 to x 
not Just 4 to x

Functors preserve the context of arguments we apply to a particular function, - context here talks about the boxing concept

the contextual application of appicative functors in general is we have a function (a -> b) and a type f a and we want to end up with type f b , they also apply in the 
context of partially applied functions with a context being applied to parameters with contexts
pure is like return --- pure wraps a value into context , its usually as a way of preparing a value that has to be fed into a function that takes 
arguments of a given context.... or presenting the value computed but the computing function is expected to yield a particular type  with a context

<*> takes a wrapped semi applied function and applies it to a wrapped argument ( yet the semi applied function needs 
 the contents of the wrapped argument ) --- ie its some form of encapsulated abstracted pattern matching
in the same context >>= takes a wrapped value and applies it to a function that takes a normal argument to produce a wrapped response
Remember a typeclass is essentially a group of functions so Normal functor consists of fmap, Applicative functor uses <*> and <$>, Monad typeclass consists
of >>= ; >> 

what makes it clear what type class we are using with a particular wrapper is the functions we use ( because type classes are essentially a group of functions )

the function on to which the monadic >>= is applied has to have a type signature ( a -> f  b ) where we are applying/ or feeding  f a  to the 
 function -- thats  the only way it can work

x >> y = x >>= \_ -> y , ie we are applying the normal monadic operator to a monad x (  the type sig requires x be a monad) to a function which 
whatever is applied  to it always produces y which too is a monad -- in the operations of >>= this means this will always produce y ??

The maybe type class should get introduced in a context where in a computation it can be classified as success or failure 

In cases we are running a series of computations which become meaningless at failure the coolest way is to use Maybe ....once it returns a Nothing it then propagates to the 
end indicating a falure at some point --- this short circuiting is also possible with basic recursion 

Monads work well in a context where we chain a lot of operations where each operation has the possibility of failure and once there is faiure at some point we want the 
process to short circuit

>>= processes from the left see below

Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))  

from the left in a monadic context 3 binds to x , the return value of this second most left function is what will now count as y , which is "!" but since the inner most 
function is inside the scope of the outermost where 3 is bound to x  is still in scope
the following is the way to do a do in my ghci 
foo = do  x <- Just 3  ; y <- Just "!"  ;  Just (show x ++ y)  

in a do notation <- eats a value in a context , ie if the value is not wrapped use  <- return ---- to wrap it 

its like we cant do <pure value > >>= because that is not what >>= takes , it really should be return < pure , unwrapped value >  >>=

The  advantage of these monadic and applicative functions is chaining of wrapped values  which maybe difficult without them 

>>= when used with list is usefull for a  scenario where we have a list of values and each of those has to be mapped in such a way that it produces a list of 
its own , - in light of this statement the monadic implementation of >>= in list makes sense ie  xs >>= f = concat (fmap f xs) , eg think of a case 
where f is defined as \ x -> [ -x , x ] such that each x of the original xs is mapped into a list

because the context of list is non deterministic , ie they represent the possible outcome of a value which can take one of many possible values , for this
reason any function which produces a list as  its result is said to be non deterministic.... and so the usage of >>= with lists is especially for cases 
where we are maping the list with a non deterministic function

[1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch) this is best understood as let n take the values [ 1,2 ] and ch [ 'a' , 'b' ] then of course return the 
 return ( n , ch ) for all the possible combinations of n and ch  return here is in the monadic sense of lists by impication

thus the above can be represented as  kik = do 
					n <- [ 1,2 ]
                                        ch <- [ 'a' , 'b' ]
                                          return ( n , ch )
viewed in a non deterministic context where n <- [ 1,2] is interpreted in a non deterministic context ie let n be 1 or 2 

the same can be achieved by defining a function tupler , then using list in an applicative context as shown below 
tupler =  \ x y -> [ ( x , y )]

concat (tupler <$>[ 1,2 ] <*> [ 'a' , 'b' ]) here its the use of lists in an applicattive functor context

thus what the monadic seems to have accomplished here is just a different syntatic presentation else whats done here can be done in other ways
good ole list comprehensions could stil do all this again [ (n,ch) | n <- [1,2], ch <- ['a','b'] ]  

given a Knight position on a chess board ie cr , we want to create a function that that gives all possible that our knight  can take while at the same time 
filtering positions not on the board 

knightPositions ( c , r ) =  [ ( c' , r') | ( c' , r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2) ] 
, (c' `elem` [ 1..8]) , (r' `elem` [ 1..8]) ]

the same can be accomplished by 

moveKnight (c,r) = filter onBoard  
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
    ]  
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8] 

if we wanted to see where the knight could be in 3 moves we could use the following function

in3 start = do   
    first <- knightPositions start  
    second <- knightPositions first  
    moveKnight second  

the above looks cleaner , in light of the first method we would have to 

map knightPositions $ map knightPositions (knightPositions ( c , r ) ) 

 this gives us all possible 
positions after the third move.. the monadic of lists
 is best done using the do contstruct , the usage of >>= is simplified in that specific context by the use of do

the following Monadic context does the job again 

in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight  
return start wraps whats bound in start into a monadic context as required by the argument ffed into >>=
*Monad laws*
return x >>= f must be the same as f x , this is called the left identity

right identity rule says if we have a monadic value m and we feed it using >>= to return it shoud give m , this is called the right identity

Associativity rule says that the order of chaining in chained monadic values the final result must not affect

ie m >>= g >>= f must be the same as m >>= (\x -> g x >>= f )

m >>= return must be m - easiest way to look at this is is >>= causes the value inside the monadic context to be applied to return 

effectively  m >>= f = concat (fmap f m)


ie a normal monadic operation where return is that function which takes x yet to which is being applied f x where f is a type constructor

[] acts a  functor when we use fmap ,as an applicative functor when we use <*> and as a monad when we use >>=, the behaviour when applied with those 
various functions is what is important , the same thing applies to Maybe 