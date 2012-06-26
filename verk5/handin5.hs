-- Authors: Davíð Halldór Lúðvíksson and Örn Ingvar Ásbjörnsson
-- Handin 5
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
data Speed = Speed v | h
type Identity = Int
type Location = Int
type Particle = (Identity,Location,Speed)
type PhysicsSystem = [Particle]

myPhysicsSystem = [(1,-1,h),(2,3,v),(3,5,h)]

evolve :: PhysicsSystem -> PhysicsSystem
--TODO scan over each particle in system and implement +1 step for h and -1 step for v
--TODO implement a way to compare each particles location to others to detect collision

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
minkFather :: Mink -> Maybe Mink

minkMother :: Mink -> Maybe Mink

--Todo: Create type Mink