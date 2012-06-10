-- Authors: Davíð Halldór Lúðvíksson and Örn Ingvar Ásbjörnsson
-- Handin 3

import System.IO
import Data.Char

--Exercise 1
--a) Implement the function backforth which, when given a list of elements
--[a, b, ..., y, z],
--for any values of a, b, ..., y, z, returns the infinite list
--[a, b, ..., y, z, y, ..., b, a, b, ..., y, z, y, ...].
--Example:, backforth [1, 2, 3] = [1, 2, 3, 2, 1, 2, 3, 2, ...]
--and backforth [1] = [1,1,1,...].
--backforth :: [a] -> [a]
--Hint: To test backforth, use take.
backForth :: [a] -> [a]
backForth [] = []
backForth xs = xs ++ reverse (init xs) ++ backForth xs


--b) Implement the input-output function main which accepts 1 line of input,
--and returns that line, in upper case.
--main :: IO ()
--For example (first line after main typed using the keyboard, second is
--the result),
-- main
--Flesh-eating Zombies!
--FLESH-EATING ZOMBIES!

main :: IO ()
main = do
  txt <- getLine
  let bigTxt = map toUpper txt
  putStrLn (  bigTxt  )

--1
--Exercise 2 (High-Low)
--Implement the classical High-Low guessing game in Haskell, in a function
--called main. The program randomly generates a number x between 0 and
--100 without displaying it to the player, and asks the player to guess the
--number x. If the player guesses a number less than x, the program informs
--the player that x is “higher”, and the player gets to guess again. If the player
--guesses a number higher than x, the program informs the player that x is
--“lower”, and the player gets to guess again. If the player guesses correctly,
--the program writes “bingo”.
--main :: IO ()
--An illustrated example (the “<-- user input” text is not part of the user
--input, but there to point out that the line it is written in consists of userinputted
--text):
-- > main
--make a guess between 0 and 100.
--50 <-- user input
--lower.
--25 <-- user input
--lower.
--12 <-- user input
--higher.
--18 <-- user input
--higher.
--22 <-- user input
--lower.
--20 <-- user input
--higher.
--21 <-- user input
--bingo.
-- >
--main :: IO ()
--main = do
--    putStrLn( "make a guess between 0 and 100" )
--    input <- getLine
--    putStrLn( "lower" )

--Exercise 3
--Using techniques and standard library functions you have learned about in
--this course so far, solve the following implementation exercises. Do not
--forget to explain in your own words how your implementation works, in the
--paper you hand in.
--Implement stackBubble which scans through a list and pushes each item to
--the right until it reaches a larger element. So, the largest element reaches the
--end of the list, and the smaller elements progress closer to the beginning).
--For instance,
--2
--stackBubble [4,3,2,1] = [1,2,3,4]
--stackBubble [1,2] = [1,2]
--stackBubble [2,1] = [1,2]
--stackBubble [3,2,4,1] = [2,3,1,4]
--stackBubble [2,3,1,4] = [2,1,3,4]
--stackBubble [2,1,3,4] = [1,2,3,4]
--Then use stackBubble to implement stackSort which sorts a list by use of
--repeated application of stackBubble.
--stackBubble :: (Ord a) => [a] -> [a]
--stackSort :: (Ord a) => [a] -> [a]

--IMPLEMENTATION ÚR H2!!! EKKI SKILA ÓBREYTTU

stackBubble (xs:[]) = [xs]
stackBubble (x:y:rest) = (min x y) : (stackBubble ((max x y):rest))

stackSort x | x==rest = rest
	| otherwise = stackSort rest
	where rest = stackBubble x

  --stackSort :: (Ord a) => [a] -> [a]

--Exercise 4 (Presentation!)
--Pick a problem (not too easy!) from the site http://www.haskell.org/
--haskellwiki/99_questions to present to the class during on June 12 or
--June 14.


