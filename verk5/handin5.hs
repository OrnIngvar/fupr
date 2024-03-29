-- Authors: Davíð Halldór Lúðvíksson and Örn Ingvar Ásbjörnsson
-- Handin 5

import Control.Monad
import Control.Monad.Writer

{-Exercise 2 (Adding logging to your code)
Use what you learned in Chapter 13 to add logging to code you have written
previously. Pick one of the sorting functions that you have seen: bubble,
stackBubble, queue or quick sort. If you really want to do this for some
other code you have written, you can do that instead, if it creates some fancy
logs.-}

--two patterns are in place so if the input is an empty list an empty list is returned
-- the second pattern takes care of a list with only one value and returns it directly
--with a list with more then one value the following is done
--the first two values of the list are compared and the lower value is prepended to the
-- recursive call of stackBubble with the max of the first two values prepended to the
--tail of the list
stackBubble [] = []
stackBubble (x:[]) = [x]
stackBubble (x:y:xs) = (min x y) : (stackBubble ((max x y):xs))

--stacksort uses guards to first check if the x equals the tail which and if so returns the tail
--the otherwise part of the guard recursively calls itself with the tail where the tail is the
--calling of stackBubble with x

stackSort :: Show a => Ord a => [a] -> Writer [String] [a]
stackSort x
        | x == xs = do
          tell ["Final step, list value: "++ show x]
          return x
        | otherwise = do
          tell ["Calling recursively to stackSort, list value: "++ show xs]
          stackSort xs 
          where xs = stackBubble x
-- Example input: runWriter $ stackSort [3,4,5,2,1]
{-
Exercise 3 (Physics)
Implement a very basic physics engine in the following manner: The system will consist of particles in a 
one dimensional space. Each particle has an identity (an integer) and is either going left or right. 
You should use the following types
       data Speed = Speed v | h
       type Identity = Int
       type Location = Int
       type Particle = (Identity,Location,Speed)
       type PhysicsSystem = [Particle]
For example you might have the following system
1
myPhysicsSystem = [(1,-1,h),(2,3,v),(3,5,h)]
a) Write a function that evolves the physics system by one step
           evolve :: PhysicsSystem -> PhysicsSystem
For example, if you input myPhysicsSystem from above you should get [(1,0,h),(2,2,v),(3,6,h)] as the output. 
When collisions happen you should invert the speeds of the particles involved in the collision.
b) Implement basic logging using the Writer monad, so you output a log of all collisions that happen.
c) (For bonus points) Make the speed a floating point number, and assume there is friction in the system, 
so speed decreases in each time step.
d) (For bonus points) Give each particle a mass and use the law of preser- vation of momentum to calculate 
what happens during collisions (if you want you don’t have to assume friction in the system here).
e) (For lots of bonus points) Display graphics!
-}
data Speed = V | H
type Identity = Int
type Location = Int
type Particle = (Identity,Location,Speed)
type PhysicsSystem = [Particle]

myPhysicsSystem = [(1,-1,H),(2,3,V),(3,5,H)]

--TODO scan over each particle in system and implement +1 step for h and -1 step for v
getSpeed :: Speed -> Int
getSpeed H = 1
getSpeed V = -1

getSpeed' :: Particle -> Speed
getSpeed' (_,_,s) = s 

--setLocation :: Particle -> Speed -> Particle
--getLocation (i,l,s) s = if getSpeed s /= H
--  then part

setLocation' :: Particle -> Int -> Particle
setLocation' (id,l,s) x = (id, l + x, s)

--getSpeed'' :: PhysicsSystem -> Particle
--getSpeed'' xs = map getSpeed' 

--evolve :: PhysicsSystem -> PhysicsSystem
--evolve x:xs = if getSpeed' x /= H 
--              then setLocation x 1
--              else setLocation x -1

--getLocation :: Location -> Int
--getLocation i = i
--partEvolve :: Particle -> [Char]
--partEvolve p = getSpeed' p
			-- | getSpeed /= "v" = "h"
			-- | otherwise 	  = "v"
--evolve :: PhysicsSystem -> PhysicsSystem
--evolve (_ _ s ) = if s /= H 
--                  then 

--evolve' :: Particle -> Particle
--evolve' (i, l, s) = if s = H 
--                  then (i, l+1, s)
--                  else (i, l-1, s)

--	where partLoc = map snd system
--physicsSystem = particleSolver []
--	where functionSolver (i:l:s:xs)  

--			| map snd particleA /= map snd particleB 
--			  &&  = 

{-
Exercise 4 (Cloning Minks)
You involved in illegal experiments with cloning minks. To keep track of your flock of minks you need to 
write functions that given a mink will tell you which mink is it’s mother and which mink is it’s father. 
The minks that are cloned have a single parent (their clone).
a) Write a function minkFather :: Mink -> Maybe Mink that returns the father of the mink, if it exists, 
but Nothing otherwise. Also write the function minkMother.
b) Use the > >= notation to write a function called maternalGrandfather that given a mink will return 
the father of mother of the given mink, if it exists.
c) Write the function traceFamily that given a mink and a path in it’s fam- ily tree, like 
father-mother-mother-father-mother will return the mother of the father of the mother of the mother 
of the father of the given mink, if it exists. 
Hint: Use foldl.
-}

--Todo: Create type Mink
--data List a = Empty | Link a (List a) deriving (Show, Read, Eq)
--type Mink = List

data Parent = Father | Mother deriving (Show, Read, Eq)
type Mink = Parent 

minkFather :: Mink -> Maybe Mink
minkFather mink
    | mink == Father = Just Father
    | otherwise      = Nothing

minkMother :: Mink -> Maybe Mink
minkMother mink
    | mink == Mother = Just Mother
    | otherwise      = Nothing

--minkMother :: Mink -> Maybe Mink

--testFamily = Parent "Father"  
--            (Parent "Mother"  
--                (Parent "Child" Empty Empty)  
--                (Parent "Child" Empty Empty)  
--            )  
--            (Parent "Mother"  
--                (Parent "Child" Empty Empty)  
--                (Parent "Child" Empty Empty)  
--            )

--testMink = Parent "Father"
--            (Parent "Child" Empty Empty)
--            (Parent "Child" Empty Empty)

--testMink1 = Empty

--testMink2 = Father
--minkFather Father >>= minkFather >>= minkMother
          