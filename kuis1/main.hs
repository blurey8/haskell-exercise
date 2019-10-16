main = putStrLn "a"

-- 1
mistery xs ys = concat(map(\x -> map(\y -> (x,y)) ys) xs)


concat :: List(List a) -> List a
concat [] = []
concat xs:xss = xs ++ (concat xss)

conc = fold (++) []