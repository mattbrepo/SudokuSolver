module UtilGen where

-- distinct (with foldl)
fl_distinct xs = foldl f [] xs
  where f vs x = if (elem x vs) then vs else (vs ++ [x])

-- drop n tail items of xs
dropTail n xs = if (length xs - n) < 0 then [] else (take (length xs - n) xs) 

-- replicate
myRep x n
  | n <= 1 = [x]
  | otherwise = x : (myRep x (n-1))

-- filter (with guards)
myFilter f [] = []
myFilter f (x:xs)
  | f x = x : myFilter f xs
  | otherwise = myFilter f xs
  
myFilter2 f (x:xs)
  | (x:xs) == [] = []
  | f x = x : myFilter2 f xs
  | otherwise = myFilter2 f xs
  
-- drop  
myDrop :: Int -> [a] -> [a]
myDrop n xs = if n <= 0 || null xs
then xs
else myDrop (n - 1) (tail xs)

-- sum (pattern matching e non)
sumList (x:xs) = x + sumList xs
sumList [] = 0

sumList2 (x:xs) = if null xs
then x
else x + sumList2 xs

sumList3 (x:xs) = x + sumList3 xs
sumList3 _ = 0

-- Hamming weight
hamming :: [Char] -> Int
hamming [] = 0
hamming (x:xs) = (if x == '0' then 0 else 1) + (hamming xs)

-- revert list
revert (x:xs) = (revert xs) ++ [x]
revert [] = []

-- palindromic
palindro xs = xs ++ (revert xs)

--- lastButOne :: [a] -> a
lastButOne xs = last(take 1 (drop (length(xs)-2) xs))

--- lastButOne2 :: [a] -> a
lastButOne2 (x:xs) = if length xs == 1
                     then x
                     else lastButOne2 xs
-- last
myLast :: [a] -> a
myLast (x:xs) = if length xs == 0
                then x
                else myLast xs

-- head
myHead :: [a] -> a
myHead (x:xs) = x

-- sumWith (strict eval test $!)
sumWith v [] = v
sumWith v (x:xs) = (sumWith $! (v+x)) xs

-- integer divisions
myIntDivFloor :: Int -> Int -> Int
myIntDivFloor a b = floor $ (fromIntegral a) / (fromIntegral b)

myIntDivCeiling :: Int -> Int -> Int
myIntDivCeiling a b = ceiling $ (fromIntegral a) / (fromIntegral b)

--- count
myCount f = length . filter f
myCountVal v xs =  length $ filter (\x -> x == v) xs

--- tuple3
-- fst3 :: (a, b, c) -> a
fst3 (x,_,_) = x
snd3 (_,x,_) = x
trd3 (_,_,x) = x

--- elemIndex
myElemIndex v xs = myEI0 0 v xs
  where myEI0 i v [] = -1
        myEI0 i v (x:xs) = if v == x then i else myEI0 (i+1) v xs

--- fattoriale
fact n = product [1..n]

-- map with Index
mapIdx f xs = zipWith f [0..] xs

--- restituisce tutte le sub-list che rispettano testF
subListTest testF xs = filter (\ys -> (length ys) > 0) (subListTest0 testF xs)
  where subListTest0 testF [] = []
        subListTest0 testF (x:xs) = (subListTest1 testF (x:xs) [] []):(subListTest0 testF xs)
        subListTest1 testF [] lastGood current = lastGood
        subListTest1 testF (x:xs) lastGood current
          | testF (current ++ [x]) = subListTest1 testF xs (current ++ [x]) (current ++ [x])
          | otherwise = subListTest1 testF xs lastGood (current ++ [x])

--- restituisce la miglior sub-list che rispetta fIsValid ad ogni passaggio
--- fGB: (aka fGetBest) confronta due subList e restituisce la migliore
--- fIV: (aka fIsValid) restituisce True se la subList è valida
subListBest :: ([a] -> [a] -> [a]) -> ([a] -> Bool) -> [a] -> [a]
subListBest fGB fIV xs = sLB0 xs [] []
  where sLB0 [] cs bs = fGB cs bs --cs: current sublist, bs: best sublist so far
        sLB0 (y:ys) cs bs
          | fIV (cs ++ [y]) = sLB0 ys (cs ++ [y]) (sLB0 ys cs bs)
          | otherwise = sLB0 ys cs bs

remove a [] = []
remove a (x:xs) | x == a = remove a xs | otherwise = (x:remove a xs)
