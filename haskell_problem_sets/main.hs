-- myLast x:xs = if xs == [] then x else myLast xs

main = putStrLn "a"

-- 1
myLast :: [a] -> a
myLast [] = error "Kosong cuy!"
myLast [x] = x
myLast (x:xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast [] = error "Kosong cuy!"
myButLast [x] = error "Cuman 1 cuy!"
myButLast [x, y] = x
myButLast (x:xs) = myButLast xs

-- 3
elementAt :: [a] -> Int -> a
elementAt [] n = error "kosong"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)

-- 4
myLen :: [a] -> Int
myLen [] = 0
myLen (x:xs) = 1 + myLen xs

-- 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myRev :: [a] -> [a]
myRev = foldl (flip (:)) []

-- -- if the list is empty, the result is the initial value z; else
-- -- apply f to the first element and the result of folding the rest
-- foldr f z []     = z 
-- foldr f z (x:xs) = f x (foldr f z xs) 

-- -- if the list is empty, the result is the initial value; else
-- -- we recurse immediately, making the new initial value the result
-- -- of combining the old initial value with the first element.
-- foldl f z []     = z                  
-- foldl f z (x:xs) = foldl f (f z x) xs

-- 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = (x == (last xs)) && (isPalindrome(init xs))

isPal :: Eq a => [a] -> Bool
isPal xs = xs == (reverse xs)

-- 7
-- ???
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List []) = []
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten(List xs)

-- 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = if (x == head xs) then (compress xs) else ([x] ++ compress xs)

-- comp :: Eq a => [a] -> [a]
-- comp = map head group

-- 9
-- pack :: Eq a => [a] -> [a]
-- pack [] = []
-- pack (x:xs) = if (head x == head xs) then [[x] ++ [head xs]] else [[x] ++ [pack xs]]