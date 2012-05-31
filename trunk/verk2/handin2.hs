-- Authors: Davíð Halldór Lúðvíksson and Örn Ingvar Ásbjörnsson
-- Handin 2

-- Exercise 1
-- a) Redefine the following version of the disjunction operator using conditional
-- expressions rather than pattern matching:
-- False || False = False
-- _ || _ = True

x `or_operator` y = if x == False && y == False
	then False
	else True

-- b) Redefine the following version of the disjunction operator using conditional
-- expressions rather than pattern matching:
-- False || b = b
-- True || _ = True

x `or_operator'` y = if x == False
	then y
	else if x == True
		then True

-- c) Define the function myTakeWhile which behaves like takeWhile, except
-- that it is defined using conditional expressions only.


-- d) Define the function myElem which behaves just like the function elem.
-- Define it using
-- i) Conditional expressions only (call this version myElem1),
-- ii) Pattern matching and conditionals only (call this version myElem2),
-- iii) Guards and pattern matching (call this version myElem3),
-- iv) foldl or foldr, without using conditionals, pattern matching, guards,
-- or recursion (call this version myElem4)

-- 2a)
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted

-- [3,2,8,6,1,5,4,7]
-- on first run of quicksort smallerSorted becomes :
-- [ 2 ] quicksort [ 8,6,1,5,4,7 ] 
-- second run biggerSorted becomes : 
-- [ 8 ] quicksort [ 6,1,5,4,7 ]
-- third run biggerSorted becomes :
-- [ 8, 6 ] quicksort [ 1, 5, 4, 7 ]
-- fourth run smallerSorted becomes :
-- [ 2, 1 ] quicksort [ 5, 4, 7 ]
-- fifth run biggerSorted becomes :
-- [ 8, 6, 5 ] quicksort [ 4, 7 ]
-- sixth run biggerSorted becomes :
-- [ 8, 6, 5, 4 ] quicksort [ 7 ]
-- eight run biggerSorted becomes :
-- [ 8, 6, 5, 4, 7 ]
--
-- smallerSorted [2, 1] 
-- is then prepended to [3] and appended to biggerSorted
-- [8, 6, 5, 4, 7] to form [2, 1] ++ [3] ++ [8, 6, 5, 4, 7]
-- smallerSorted is then sorted becoming [1,2]
-- biggerSorted is then sorted becoming [4,5,6,7,8]
-- again smallersorted is prepended to [3] and biggerSorted
-- is appended to it forming the final list of :
-- [1,2,3,4,5,6,7,8]

-- 2b)
-- foldr (++) [] [[1,2], [3,4], [5,6]]
-- ++ [1,2]( ++ [3,4] ( ++ [5,6] [] ))
-- [[1,2], [3,4]] ++ [5,6]
-- [[1,2]] ++ [3,4,5,6]
-- [1,2,3,4,5,6]
--
-- foldl  (++)  []  [[1,2], [3,4], [5,6]]
-- ++ ( ++ ( ++ [] [1,2] ) [3,4] ) [5,6] 
-- [1,2] ++ [[3,4],[5,6]]
-- [1,2,3,4] ++ [[5,6]]
-- [1,2,3,4,5,6]

-- 3a)
--myunzip :: [(a, b)] -> ([a], [b])
--myunzip [] = ([],[])
--myunzip ((x,y):xs) = map xs ([x] ++ fst xs, [y] ++ snd xs )
	--let
	--	(a,b) = myunzip xs
	--in
	--	([x] ++ a, [y] ++ b)

-- 3b)
lettersOnly  ::  String -> String
lettersOnly  "" = "Empty"
lettersOnly (x:xs) = filter ( `elem` ['a'..'z']) (x:xs)
