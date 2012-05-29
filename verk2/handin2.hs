-- Authors: Davíð Halldór Lúðvíksson and Örn Ingvar Ásbjörnsson
-- Handin 1

-- Exercise 3
-- a) mylength :: (Num t1) => [t] -> t1

myLength xs = sum [1 | _ <- xs]

myLength' [] 	 = 0
myLength' (x:xs) = 1 + myLength' xs

-- b) cons :: a -> [a] -> [a]

cons x xs = [x] ++ xs

-- c) mysq :: (Num a) => [a] -> [a]

mysq [] = []
mysq (x:xs) = (sqrt x) : mysq xs


-- d) myget :: (a, (a1, [a2])) -> a2
myget x = snd (snd x) !! 1 

-- e) isSingleDigit :: Integer -> Bool

isSingleDigit x = if x < 10
	then True
	else False

-- Exercise 4
-- a) Get the first element in the list [1, 2, 3, 4, 5].
-- fst [1,2,3,4,5]

-- Answer: fst only works on tuples, correct implementation would be [1,2,3,4,5] !! 0

-- b) Get the second element from the list [1,2,3,4,5].
-- !! [1,2,3,4,5] 2 

-- Answer: The !! operator sould be used as infix notation, correct implementation would be [1,2,3,4,5] !! 2

-- c) Let idx1sel be the function that selects the element at index 1 from a list,
-- and let x be the element at index 1 in the list [’a’, ’b’, ’c’, ’d’, ’e’].
-- idx1sel = (1!!)
-- x = idx1sel [’a’,’b’,’c’,’d’,’e’]

-- Answer: The function declration doesnet have parmeter decleration, the correct implementation would be idx1sel = xs!!1
-- x = idx1sel [’a’,’b’,’c’,’d’,’e’]

-- d) Let 2incr be the function which increments a numeral by 2.
-- 2incr = (2+)

-- Answer: The function needs input parmeter delecration, correct implementation would be 2incr x = x+2

-- The following erroneous program does not have a correct implementation.
-- Explain why.
-- e) Let totuple be the function which converts a list to a nesting of pairs.
-- For example, totuple [1, 2, 3, 4] = (1, (2, (3, 4))).
-- totuple [] = ()
-- totuple (x:xs) = (x, totuple xs)
