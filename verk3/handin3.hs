-- Authors: Davíð Halldór Lúðvíksson and Örn Ingvar Ásbjörnsson
-- Handin 3

import System.IO
import Data.Char
import System.Random

--Exercise 1
--a)
backForth :: [a] -> [a]
backForth [] = []
backForth xs = xs ++ reverse (init xs) ++ backForth xs


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
stackBubble [] = []
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