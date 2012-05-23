-- Authors: Davíð Halldór Lúðvíksson and Örn Ingvar Ásbjörnsson
-- Handin 1

-- Exercise 3
-- a)

myLength xs = sum [1 | _ <- xs]

myLength' [] 	 = 0
myLength' (x:xs) = 1 + myLength' xs

-- b) 

cons [] = []
cons (x:xs) = x ++ cons xs 