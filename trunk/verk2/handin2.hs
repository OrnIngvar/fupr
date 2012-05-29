-- Authors: Davíð Halldór Lúðvíksson and Örn Ingvar Ásbjörnsson
-- Handin 1

-- Exercise 1
-- a) Redefine the following version of the disjunction operator using conditional
-- expressions rather than pattern matching:
-- False || False = False
-- _ || _ = True

fall x y = if x == False && y == False
	then False
	else True

-- b) Redefine the following version of the disjunction operator using conditional
-- expressions rather than pattern matching:
-- False || b = b
-- True || _ = True

-- c) Define the function myTakeWhile which behaves like takeWhile, except
-- that it is defined using conditional expressions only.

-- d) Define the function myElem which behaves just like the function elem.
-- Define it using
-- i) Conditional expressions only (call this version myElem1),
-- ii) Pattern matching and conditionals only (call this version myElem2),
-- iii) Guards and pattern matching (call this version myElem3),
-- iv) foldl or foldr, without using conditionals, pattern matching, guards,
-- or recursion (call this version myElem4)