not that the following fibbonaci works

myfib n | n == 1  = 1 | n == 2 = 1 | otherwise = myfib (n-1) + myfib (n-2)

but the following , for any values of n other than 1 or 2 will throw a stack overflow exception

myfib n | n == 1  = 1 | n == 2 = 1 | otherwise = myfib n-1 + myfib n-2

most likely because it will be interpreted as (myfib n) - 2 

below works
-------------------------------------------------------------
module Main 
    where

import System.IO

main = do

hSetBuffering stdin LineBuffering
putStrLn "Please enter your name :"
name <- getLine
putStrLn ("Hello " ++ name ++ " , how are you? ")

-------------------------------------------------------
the code below works 
----------------------------------------------------------
module Main 
    where

import System.IO

 

main    = do

     hSetBuffering stdin LineBuffering

     putStrLn "Enter a Number : "

     num <- getLine 
     if read num == 0 then  return [] else fmap ((:) num)  main 

------------------------------------------------------------------

it seems when working with IO  do can only return [String]

-----------------------------------------------------------------------
with monad we have a contextualised (boxed) argument that we want to feed into an argument that takes a normal argumet
and the synax is < contextualised_arg> >>= function , note that the function must produce a contextualised result

this concept is well represented below , in a code which works . The code also shows how a do block allows the recursion of half a function allowing
it to only go beyond a stage in its execution after some conditions are met . See how in getter return call is a part of the function to be executed but it only executes after half the function has recursed itself to the same point
-----------------------------------------------

module Main 
    where

import System.IO

shower  [] = putStrLn "And we are done" ;shower  (x:xs) = do putStrLn ( x  ++ " is in the set ");shower  xs

     
       
getter    = do

     hSetBuffering stdin LineBuffering

     putStrLn "Enter a Number : "

     num <- getLine
     coll <- ( if read num == 0 then  return [] else fmap ((:) num)  getter )
     return coll
    
    
main = getter >>= shower

------------------------------------------------------------------------------------

A type variabe is that thing that evaluates to a type eg [ a ]  a is a type variable because it can be used 
to contain variable types
A type class is essentially a group of interelated functions
when all functions of a particular type class are implementable for a particular type , and it is instantiated for that class then 
we say the type is an instance of the type clss
\ is used for escaping characters , ie for nullifying their value in a context
Num a => class constraint means a type a whichis an instance of the class Num

------------------------------------------------------------------------------
main = do
   s <- readFile <some file>
   let i = f s
   putStrLn (show  i)

note how the let inside a do block does not have an in

--------------------------------------------------------------------------------------
:set +s sets up gchi to give execution time

-------------------------------------------------
data Quadr a b = Q a a b b 

note on the left a and b are the types that will be present not necessarily their postions , 

now on the right side the letters represent two things , the exact arguments needed and their respective position
on the right you can only use what has been defined in the left and any undefined but built in types 
the a and b on the left are called type parameters 

---------------------------------------------------
Either a b is used for something that can take a value type a or b

A recursive datatype is one whose definition is based on itself

eg data List a = Nil | Cons a (List a)
in other words Cons is a constructure whose type is in the type List a but made of a value of type List a and a
------------------------------------------------------------------------

do is a way of equencing things or of running things in a sequence

use a do in every branch of code whose execution should take a sequential form

---------------------------------------------------------------
in the code below note the use of a do block with curlies
-------------------------------------------------------
module Main 
    where

import System.IO


     
       
main    = do

     hSetBuffering stdin LineBuffering

     putStrLn "Enter your name : "

     name <- getLine
     
     case name of { "kate" -> do {
                                 putStrLn "Hello sweetheart";
                                                   main };
                       "mum" -> putStrLn "Hello mum" ;
                  "jessica" -> putStrLn "Hello love" ; _ ->putStrLn "who are you ?" }
                     
                     
 ---------------------------------------------------------------------------------------------------------  
    the code below works

---------------------------------------------------------------------------------------------------
module Main 
    where

import System.IO


main    = do

     hSetBuffering stdin LineBuffering

     putStrLn "Do you want to [read] or write a File "

     comand <- getLine
     
     case comand of { "read" -> do {
                                 putStrLn "Enter Name of file with extension";
                                 fileName <- getLine;
                                 message <- readFile fileName; 
                                 putStrLn message;
                                      };
                       "write" ->do {
                                 putStrLn "Enter Name of file with extension";
                                 fileName <- getLine;
                                 putStrLn "Enter the text to be writen";
                                 text <- getLine;
                                 writeFile fileName text;
                                   
                                           main };}
--------------------------------------------------------------------------------------------------------------
If and when we do 

module Cards ( Cards() ,   
               Deck())
 we are simply saying into this module we are exporting the Cards() type without exporting the constructors that are found in that 
type.

People who use this module will not be able to access any functions that are not specified.Nb we can hide a specific functions which if
tampered with will mess up our programme  ,just as we can hide any specific implementations we do not want anyone to see

to export constructors do 
 
module Cards ( Cards(..) ,   
               Deck(..))

Module name and File name should be the same , ie Card module should have the name Card.hs


import qualified Cards 

means you always when using the Cards module you have to specify the whole name, you can also add as 

import qualified Cards(deal)

means you are importing only the deal function from the Cards module 

import qualified Cards hiding (deal)

where has a localised scope

guards should always be in the last version of a function , ie they cannot have pattern matching after them ....it wont be evaluated if the guards were to fail

instance Eq Colour  where
        Red ==Red = True

For the Equatable class minimal complete Definition simply takes the form of defining what should constitute equality as above

if you have a datatype constructor  to update x to z and a to b y{x = z , a = b } as shown below
--------------------------------------------------------------------------------------------------------------------------------------------

data Student = Profile { name :: String , age ::Int} deriving ( Show , Read , Eq  )

let  kate = Profile{ name = "Kate" , age = 3 }  constructing an instance 

 age kate -- as shown by this the fields are accessor functions
 kate{ age = 5 } this works too , returns kate unchanged on all other aspects save the age field , however the instance of kate still maintains its immutability
ie even after this update if you call kate it still comes back with age 3

 
-----------------------------------------------------------------------------------------------------------

zip lis [ 1......] is better than zip lis [ 1 ..(length  lis)] they all work the same , because zip works with the shortest

below is a smart way of avoiding repeated generations
[(x , y) | x <- [ 1..5] , y<-[ x ..7]]

concat flattens a list of lists into a list of elements 

---------------------------------------------------------------------------------------------------------------------------
Map/filter are  implemented recursively as 
daumeMap _ [] = [] ;daumeMap f (x : xs ) = (f x ) : daumeMap f xs
