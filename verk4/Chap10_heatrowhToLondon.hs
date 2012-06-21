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