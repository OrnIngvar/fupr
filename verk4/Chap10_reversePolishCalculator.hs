solveRPN :: (Num a, Read a) => String -> a -- BANNAÐ AÐ GERA TAB!!!!!!!!!!!!!!!!!!!!!
solveRPN expr = head $ foldl eitthvadFall [] $ words expr
    where eitthvadFall (x:y:ys) "sinnum" = (x*y):ys
          eitthvadFall (x:y:ys) "plus" = (x+y):ys
          eitthvadFall ys numString = read numString:ys