Data.List.findIndex (==4) [ 11,1,1,2,1,1,2,2,2,2,3,4,5,6]

myGrouper  [] = [] ; myGrouper (x:ys) = (x:(takeWhile ( x == ) ys)) : myGrouper (dropWhile ( x == ) ys )

myGrouper [ 11,1,1,2,1,1,2,2,2,2,3,4,5,6]

ceaserCipherMimic shift message = let { corded = map Data.Char.ord message ; shifted = map (+ shift) corded } in map Data.Char.chr shifted

ceaserCipherMimic 5 "My name is Kate Hlomani"

let encrypted = ceaserCipherMimic 5 "My name is Kate Hlomani"
ceaserDecrypter shift encrypted =  let { corded = map Data.Char.ord encrypted ; unshifted = map (\x -> (abs (subtract shift x))) corded } in map Data.Char.chr unshifted

ceaserDecrypter 5 encrypted

apply compare Data.Function.on ----- for that series of ...By functions that relies on comparison
consider as equal two things that meet the following conditions - (==)  Data.Function.on  ------- for that series of ...By functions that relies on Eqality eg groupBy
 f (g (z x)) is equal to (f.g.z) x which is equal to f.g.z $ x but all not equal to f.g.z x

if you just say import ..... it means you can call for the functions of the imported module without qualifying thembut there is a clash risk
to avoid the clash risk however do import qualified ...... as .... in which case you always need to qualify when you call
..A type class is a group of interelated functions , and instances of that type class are any data types where it makes sense to apply those functions
.. To convert a non string value into a sting use show .... , NB remember in haskell only same data types can be processed togetherv , and so to make 
 say Int part of a string we have to convert In to string using show 
When something is right associative it means  the innermost bracket should be to the right - ( d ( c (a b) ) )
When something is left associative it means  the innermost bracket should be to the right - (((a b) c) d) -- like haskell functions
The fields on a value constructor represent two things - the type and the argument ie cant do data B a =  C b a Int , because b is not defined 
however is a is . Its like in a value constructor its a unction where the arguments have to be defined not in general form but by specifying their positions and types 
so data B a = C Int String a will work because the other two are already defined in haskell
class Eq a where - here we are creating a new type class called Eq applied on a type a (polymorphic ), its like we are creating a function called  Eq with a parameter a
class Eq a where 
(==):: a -> a -> Bool 
(/=):: a -> a -> Bool
x == y = not ( x == y )
x /= y = not (x /= y)

As part of making a specific class an instance of this class we will have to clearly define what specific conditions of the value of the said class
constitute the values specified in the type class
eg 
data TrafficLights = Red | Yellow | Green

instance Eq TrafficLights where

Red == Red = True
Yellow == Yellow = True
Green == Green = True 
_ == _  = False 

We are now defining what constitutes  ==  when dealing with this new data type, ie the assumption here is  == is not known
so the statement reads return the Boolean value True in a case where the == function is only used with the defined instances



instance Eq TrafficLights where -- in this case we are making a specific newly created type called traffic light an instance of the Eq class

instance Show TrafficLight where

show Red = "Red Light"

Here we are saying when you get the expression show Red with respect to this data type return this 

Maybe is a type constructor 

When defining instances of a type class there has to be specific instances of the generic type signature defined in the mother  function

instance YesNo Bool where
yesno = id 

Here we are saying make Boolean an instance of YesNo typeclass , and when Boolean is being treated in the context of yesNo class then return id function -
which is afunction which takes a single argument

instance YesNo (Maybe a ) where
yesNo ( Maybe _ ) = True
yesNo Nothing    = False 

We are saying treat  the type ( Maybe a) as an instance of the YesNo typeclass and when you do that  this is how you treat it 
{--- as described in the specific functions , and the treatment has to be in line with the type signature given in the class declaration of the typeclass
 to which we are making the type an instance of }

functor is for things that can be mapped over eg list

fmap:: ( a -> b ) -> f a -> f b 
f a is a way of saying its a type constructor

functor wants a type constructor that takes one parameter only and not a concrete type

Functors are important when dealing with types that can take two values of separate properties ,
making a value instance of the functor allows a function to be applied to the inner elements 