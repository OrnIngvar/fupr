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

-}
import qualified Data.Map as Map
import Data.Char

freqTable :: [(Char, Float)]
freqTable = [('a',8.2), ('b',1.5), ('c',2.8), ('d',4.3), ('e',12.7), ('f', 2.2), ('g', 2.0), ('h',6.1), ('i',7.0), ('j',0.2), ('k',0.8), ('l',4.0), ('m',2.4),  ('n',6.7), ('o',7.5), ('p',2.0), ('q',0.1), ('r',6.0), ('s',6.3), ('t',9.1), ('u',2.8), ('v',1.0), ('w',2.4), ('x',0.2), ('y',2.0), ('z',0.1)]  

-- TODO: Taka út shift á blili
encode :: Int -> String -> String
encode shift msg = map chr $ map (+shift) $ map ord msg

-- TODO: Ekki taka prósentur á blili
wordFreq msg = Map.toList $ Map.fromListWith (+) [(toLower c, 1) | c <- msg]

toPercentage xs = [(fst x, snd x / total * 100)| x <- xs]
                   where total = totalLetters xs

totalLetters s = sum [snd x | x <- s]

mapToFreqTable xs = [ (fst y, snd y) | x <- xs, y <- freqTable]

-- usage: freqTable $ toPercentage $ wordFreq $ encode 2 "helloworld" 
{-

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
solveRPN :: String -> Float
solveRPN = head . foldl functionSolver [] . words
    where functionSolver (x:y:ys) "*" = (x * y):ys
          functionSolver (x:y:ys) "+" = (x + y):ys
          functionSolver (x:y:ys) "-" = (y - x):ys
          functionSolver (x:y:ys) "/" = (y / x):ys 
          functionSolver (x:xs) "ln" = log x:xs          
          functionSolver xs "average" = [sum xs / genericLength xs]
          functionSolver xs "max" = [maximum xs]
          functionSolver xs "min" = [minimum xs]
          functionSolver xs "sum" = [sum xs]
          functionSolver ys numString = read numString:ys


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
--Exercise 4
--a)
data Section = Section { getA :: Int, getB :: Int, getD :: Int, getC1 :: Int, getC2 :: Int } deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [ Section 50 10 35 30 25, Section 5 90 45 20 35, Section 40 2 15 25 18, Section 10 20 8 0 0 ]

data Label = A | B | D | C1 | C2 deriving (Show)
type Path = [(Label,Int)]

optimalPath :: RoadSystem -> Path
optimalPath roadSystem
              | sum ( map snd bestPathA ) <= sum ( map snd bestPathB ) && sum ( map snd bestPathA ) <= sum ( map snd bestPathD ) = reverse bestPathA
              | sum ( map snd bestPathB ) <= sum ( map snd bestPathA ) && sum ( map snd bestPathB ) <= sum ( map snd bestPathD ) = reverse bestPathB               
              | otherwise                                              = reverse bestPathD             
              where (bestPathA,bestPathB,bestPathD) = foldl roadStep ([],[],[]) roadSystem

roadStep :: (Path,Path,Path) -> Section -> (Path,Path,Path)
roadStep (pathA,pathB,pathD) (Section a b d c1 c2) = (newPathA,newPathB,newPathD)
    where timeA = sum $ map snd pathA
          timeB = sum $ map snd pathB
          timeD = sum $ map snd pathD

          forwardTimeToA = timeA + a
          crossTimeToA   = timeB + b + c1

          forwardTimeToB = timeB + b
          crossTimeToB   = timeA + a + c1
          crossTimeToB'  = timeD + d + c2

          forwardTimeToD = timeD + d
          crossTimeToD	 = timeB + b + c2

          newPathA = if forwardTimeToA <= crossTimeToA
                        then (A,a):pathA
                        else (C1,c1):(B,b):pathB

          newPathB = if forwardTimeToB <= crossTimeToB && forwardTimeToB <= crossTimeToB'
                        then (B,b):pathB
                        else if crossTimeToB <= crossTimeToB'
                        		then (C1,c1):(A,a):pathA
                        		else (C2,c2):(D,d):pathD

          newPathD = if forwardTimeToD <= crossTimeToD
          				then (D,d):pathD
          				else (C2,c2):(B,b):pathB
                      
--b) Will not compile unless a) is commented out !!
data Section = Section { getA :: Int, getB :: Int, getD :: Int, getE :: Int, getC1 :: Int, getC2 :: Int, getC3 :: Int, getC4 :: Int } deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [ Section 15 30 40 35 20 10 15 10, Section 75 85 65 45 30 20 15 25, Section 45 30 50 55 10 20 15 25, Section 35 55 20 40 0 0 0 0]

data Label = A | B | D | E | C1 | C2 | C3 | C4 deriving (Show)
type Path = [(Label,Int)]

optimalPath :: RoadSystem -> Path
optimalPath roadSystem
              | sum ( map snd bestPathA ) <= sum ( map snd bestPathB ) 
              	&& sum ( map snd bestPathA ) <= sum ( map snd bestPathD )
              	&& sum ( map snd bestPathA ) <= sum ( map snd bestPathE ) = reverse bestPathA
              | sum ( map snd bestPathB ) <= sum ( map snd bestPathA ) 
              	&& sum ( map snd bestPathB ) <= sum ( map snd bestPathD )
              	&& sum ( map snd bestPathB) <= sum ( map snd bestPathE ) = reverse bestPathB               
              | sum ( map snd bestPathD ) <= sum ( map snd bestPathA ) 
              	&& sum ( map snd bestPathD ) <= sum ( map snd bestPathB )
              	&& sum ( map snd bestPathD) <= sum ( map snd bestPathE ) = reverse bestPathD
              | otherwise                                              = reverse bestPathE
              where (bestPathA,bestPathB,bestPathD,bestPathE) = foldl roadStep ([],[],[],[]) roadSystem

roadStep :: (Path,Path,Path,Path) -> Section -> (Path,Path,Path,Path)
roadStep (pathA,pathB,pathD,pathE) (Section a b d e c1 c2 c3 c4) = (newPathA,newPathB,newPathD,newPathE)
    where timeA = sum $ map snd pathA
          timeB = sum $ map snd pathB
          timeD = sum $ map snd pathD
          timeE = sum $ map snd pathE

          forwardTimeToA = timeA + a
          crossTimeToA   = timeB + b + c1
          crossTimeToA'  = timeE + e + c4

          forwardTimeToB = timeB + b
          crossTimeToB   = timeA + a + c1
          crossTimeToB'  = timeD + d + c2

          forwardTimeToD = timeD + d
          crossTimeToD	 = timeB + b + c2
          crossTimeToD'	 = timeE + e + c3

          forwardTimeToE = timeE + e
          crossTimeToE	 = timeD + d + c3
          crossTimeToE'	 = timeA + a + c4

          newPathA = if forwardTimeToA <= crossTimeToA && forwardTimeToA <= crossTimeToA'
                        then (A,a):pathA
                        else if crossTimeToA <= crossTimeToA' 
                        		then (C1,c1):(B,b):pathB
                        		else (C4,c4):(E,e):pathE

          newPathB = if forwardTimeToB <= crossTimeToB && forwardTimeToB <= crossTimeToB'
                        then (B,b):pathB
                        else if crossTimeToB <= crossTimeToB'
                        		then (C1,c1):(A,a):pathA
                        		else (C2,c2):(D,d):pathD

          newPathD = if forwardTimeToD <= crossTimeToD && forwardTimeToD <= crossTimeToD'
          				then (D,d):pathD
          				else if crossTimeToD <= crossTimeToD'
          						then (C2,c2):(B,b):pathB
          						else (C3,c3):(E,e):pathE

          newPathE = if forwardTimeToE <= crossTimeToE && forwardTimeToE <= crossTimeToE'
          				then (E,e):pathE
          				else if crossTimeToE <= crossTimeToE'
          						then (C3,c3):(D,d):pathD
          						else (C4,c4):(A,a):pathA