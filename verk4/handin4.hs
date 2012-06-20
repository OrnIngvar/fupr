-- Authors: Davíð Halldór Lúðvíksson and Örn Ingvar Ásbjörnsson
-- Handin 4

{-Exercise 1
Implement the function enigma which, when given a string of words codedMessage
(usually long), in plain English, without numbers, tries to find the permutation
of the letters a,b,: : : ,z that was used to encode the message. You can
assume that spaces are mapped to themselves. Note that this will be impossible
using the brute-force method of trying all permutations, since there
are 26! many of them! You must therefore do something better and choose
either a) or b) (or both!) below.

a) Use frequency analysis: In English text some letters appear more often
than others. Many of you probably thought about this during the course
Problem Solving. Implement this as follows in Haskell: Your program
now also takes as input a frequency table which is a list of tuples of the
form [(a,0.2),(e,0.14),: : : ]. Your program then does a frequency
analysis on the encoded text and tries to figure out some of the letters.
It then uses a brute-force method to complete the task.

b) Assume that the encoded text contains common words such as “is”, “and”,
“or”, “hamster” (just kidding). Use this information first, then use a
brute-force method to complete the task.-}

{-Exercise 2
Using techniques and standard library functions you have learned about in
this course so far, solve the following implementation exercises. Do not
forget to explain in your own words how your implementation works, in the
paper you hand in.
Implement queue which scans through a list and either stores elements in a
queue or bypasses them to the output. The queue must be kept in increasing
order. For instance,
queue [3,4,2,5,1] = [2,1,3,4,5]
because 3 is put is put in the queue, then 4, then comes 2, which can’t go
on the queue. We then ask which is smaller, the 2 or the front of the queue,
which is 3. The 2 is smaller which is then outputted. Now 5 is put on
the queue. Finally 1 comes and that is smaller than everything so that is
outputted. Then we clear the queue.
queue [2,3,1,4] = [1,2,3,4]
because first 2 and 3 go on the queue, then comes 1. This can not be put at
the end of the queue. So we compare 1 with the front element in the queue,
2. We bypass 1 to the output since it is smaller. Then 4 is put at the end
the queue and then it is emptied. Then use queue to implement queueSort
which sorts a list by use of repeated application of queue.
queue :: (Ord a) => [a] -> [a]
queueSort :: (Ord a) => [a] -> [a]-}

{-Exercise 3 (Extending the Reverse Polish Notation Calculator)
Extend the function solveRPN from Chapter 10 in the book to accept more
functions in the input string. Implement at least -,/,sum,average,max,min
and then your favorite function.-}
solveRPN :: String -> Float -- BANNAÐ AÐ GERA TAB!!!!!!!!!!!!!!!!!!!!!
solveRPN = head . foldl eitthvadFall [] . words
    where eitthvadFall (x:y:ys) "*" = (x * y):ys
          eitthvadFall (x:y:ys) "+" = (x + y):ys
          eitthvadFall (x:y:ys) "-" = (y - x):ys
          eitthvadFall (x:y:ys) "/" = (y / x):ys          
          --eitthvadFall xs "average" = [sum xs] / genericLength xs
          eitthvadFall xs "max" = [maximum xs]
          eitthvadFall xs "min" = [minimum xs]
          eitthvadFall xs "sum" = [sum xs]
          eitthvadFall ys numString = read numString:ys


{-Exercise 4 (Extending the Heathrow To London Problem)
Extend the function optimalPath (and associated data structures) from Chapter
10 in the book to accept a road system consisting of:

a) Three main roads (A,B,D) connected with crossroads (C1 between A and
B, C2 between B and D). Your implementation should accept the input
type RoadSystem = [Section] where Section contains five integers,
the lengths of A,B,D,C1,C2, respectively.

b) Three-dimensional helicopter routes (A,B,D,E) connected with crossroads
(C1 between A and B, C2 between B and D, C3 between D and
E, C4 between E and A). Your implementation should accept the input
type RoadSystem = [Section] where Section contains five integers,
the lengths of A,B,D,E,C1,C2,C3,C4 respectively.-}
data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [ Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0 ]

data Label = A | B | C deriving (Show)
type Path = [(Label,Int)]

optimalPath :: RoadSystem -> Path
optimalPath roadSystem
              | sum ( map snd bestPathA ) <= sum ( map snd bestPathB ) = reverse bestPathA
              | otherwise                                              = reverse bestPathB
              where (bestPathA,bestPathB) = foldl roadStep ([],[]) roadSystem

roadStep :: (Path,Path) -> Section -> (Path,Path)
roadStep (pathA,pathB) (Section a b c) = (newPathA,newPathB)
    where timeA = sum $ map snd pathA
          timeB = sum $ map snd pathB

          forwardTimeToA = timeA + a
          crossTimeToA   = timeB + b + c

          forwardTimeToB = timeB + b
          crossTimeToB   = timeA + a + c

          newPathA = if forwardTimeToA <= crossTimeToA
                        then (A,a):pathA
                        else (C,c):(B,b):pathB

          newPathB = if forwardTimeToB <= crossTimeToB
                        then (B,b):pathB
                        else (C,c):(A,a):pathA