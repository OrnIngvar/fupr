-- Authors: Davíð Halldór Lúðvíksson and Örn Ingvar Ásbjörnsson
-- Handin 3

import System.IO
import Data.Char
import System.Random

--Exercise 1
--a)
backForth :: [a] -> [a]
backForth [] = []
backForth (x:xs) = (x:xs) ++ tail (reverse (x:xs)) ++ tail (backForth (x:xs))


--b)
--Commented out so that the file will compile. Otherwise functional
--main :: IO ()
--main = do
--  txt <- getLine
--  let bigTxt = map toUpper txt
--  putStrLn (  bigTxt  )

--Exercise 2 (High-Low)
main :: IO ()
main = do
    gen <- getStdGen
    let (randNumber, newGen) = randomR (1,100) gen :: (Int, StdGen)
    putStr "make a guess between 0 and 100 "
    checkForNumber randNumber

checkForNumber num = do
   guess <- getLine
   if (read guess) < num
     then do putStrLn "higher."
             checkForNumber num
     else if (read guess) > num
            then do putStrLn "lower."
                    checkForNumber num
            else do putStrLn "bingo!"

--Exercise 3
--two patterns are in place so if the input is an empty list an empty list is returned
-- the second pattern takes care of a list with only one value and returns it directly
--with a list with more then one value the following is done
--the first two values of the list are compared and the lower value is prepended to the
-- recursive call of stackBubble with the max of the first two values prepended to the
--tail of the list
Exercise 1
a) Implement the function backforth which, when given a list of elements
[a, b, ..., y, z],
for any values of a, b, ..., y, z, returns the infinite list
[a, b, ..., y, z, y, ..., b, a, b, ..., y, z, y, ...].
Example:, backforth [1, 2, 3] = [1, 2, 3, 2, 1, 2, 3, 2, ...]
and backforth [1] = [1,1,1,...].
backforth :: [a] -> [a]
Hint: To test backforth, use take.
b) Implement the input-output function main which accepts 1 line of input,
and returns that line, in upper case.
main :: IO ()
For example (first line after main typed using the keyboard, second is
the result),
> main
Flesh-eating Zombies!
FLESH-EATING ZOMBIES!
>
1
Exercise 2 (High-Low)
Implement the classical High-Low guessing game in Haskell, in a function
called main. The program randomly generates a number x between 0 and
100 without displaying it to the player, and asks the player to guess the
number x. If the player guesses a number less than x, the program informs
the player that x is “higher”, and the player gets to guess again. If the player
guesses a number higher than x, the program informs the player that x is
“lower”, and the player gets to guess again. If the player guesses correctly,
the program writes “bingo”.
main :: IO ()
An illustrated example (the “<-- user input” text is not part of the user
input, but there to point out that the line it is written in consists of userinputted
text):
> main
make a guess between 0 and 100.
50 <-- user input
lower.
25 <-- user input
lower.
12 <-- user input
higher.
18 <-- user input
higher.
22 <-- user input
lower.
20 <-- user input
higher.
21 <-- user input
bingo.
>
Exercise 3
Using techniques and standard library functions you have learned about in
this course so far, solve the following implementation exercises. Do not
forget to explain in your own words how your implementation works, in the
paper you hand in.
Implement stackBubble which scans through a list and pushes each item to
the right until it reaches a larger element. So, the largest element reaches the
end of the list, and the smaller elements progress closer to the beginning).
For instance,
2
stackBubble [4,3,2,1] = [1,2,3,4]
stackBubble [1,2] = [1,2]
stackBubble [2,1] = [1,2]
stackBubble [3,2,4,1] = [2,3,1,4]
stackBubble [2,3,1,4] = [2,1,3,4]
stackBubble [2,1,3,4] = [1,2,3,4]
Then use stackBubble to implement stackSort which sorts a list by use of
repeated application of stackBubble.
stackBubble :: (Ord a) => [a] -> [a]
stackSort :: (Ord a) => [a] -> [a]
Exercise 4 (Presentation!)
Pick a problem (not too easy!) from the site http://www.haskell.org/
haskellwiki/99_questions to present to the class during on June 12 or
June 14. [] = []
stackBubble (x:[]) = [x]
stackBubble (x:y:xs) = (min x y) : (stackBubble ((max x y):xs))

--stacksort uses guards to first check if the x equals the tail which and if so returns the tail
--the otherwise part of the guard recursively calls itself with the tail where the tail is the
--calling of stackBubble with x
stackSort x
        | x == xs = xs
        | otherwise = stackSort xs
        where xs = stackBubble x

--Exercise 4 (Presentation!)
--Pick a problem (not too easy!) from the site http://www.haskell.org/
--haskellwiki/99_questions to present to the class during on June 12 or
--June 14.

-- 10 Problem 20
-- (*) Remove the K'th element from a list.

-- This function takes index of element in list and removes it from the list and adds the 
-- result to touple
-- If we get out-of bound index it wil return Nothing as first value and the original list
-- as the second value in the touple
-- If the index (x) is one then the head item (y) will be returned as the rest of the list
-- in the touple
-- Otherwise two variables are created in the touple which calls recursive and with one
-- value lower index and the tail but adds the head to the second touple list value.
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ []                 = (Nothing, [])
removeAt x (y:ys) | x == 1    = (Just y, ys)
                  | otherwise = let (front, back) = removeAt (x-1) ys in (front, y:back)

-- let (a, r) = removeAt (k - 1) xs in (a, x:r)
--removeAt n xs = (xs!!n,take n xs ++ drop (n+1) xs)
