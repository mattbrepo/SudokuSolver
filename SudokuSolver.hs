-- 
-- foldr sudoku (test)
-- 

import System.Random
import Data.List (nub, elemIndex)
import UtilGen (myIntDivFloor, myIntDivCeiling, fst3, snd3, trd3)

-- -----------------------------------------
--     | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |
-- -----------------------------------------
--   1 |           |           |           |
--   2 |     1     |     2     |     3     |
--   3 |           |           |           |
-- -----------------------------------------
--   4 |           |           |           |
--   5 |     4     |     5     |     6     |
--   6 |           |           |           |
-- -----------------------------------------
--   7 |           |           |           |
--   8 |     7     |     8     |     9     |
--   9 |           |           |           |
-- -----------------------------------------

test1 :: [Int]
test1 = [0,0,5,0,0,0,2,0,0,
         0,0,0,1,0,7,0,0,0,
         2,0,9,0,0,0,1,0,3,
         6,0,0,0,3,0,0,0,4,
         0,8,0,0,5,0,0,2,0,
         0,0,0,9,0,2,0,0,0,
         0,0,0,0,0,0,0,0,0,
         5,0,8,2,0,6,9,0,7,
         0,2,3,0,0,0,5,8,0]

test2 :: [Int]
test2 = [8,0,0,0,0,0,0,0,0,
         0,0,3,6,0,0,0,0,0,
         0,7,0,0,9,0,2,0,0,
         0,5,0,0,0,7,0,0,0,
         0,0,0,0,4,5,7,0,0,
         0,0,0,1,0,0,0,3,0,
         0,0,1,0,0,0,0,6,8,
         0,0,8,5,0,0,0,1,0,
         0,9,0,0,0,0,4,0,0]

-- sudoku dimensions
s_areamax = 3
s_max = s_areamax * s_areamax

-- temp
countval v xs =  length $ filter (==v) xs
countnon0 xs =  length $ filter (/=0) xs

-- -------------------------------
-- ------------------------------- ACCESS
-- -------------------------------

-- get area from row col
s_getAreaNum row col = (myIntDivCeiling col s_areamax) + s_areamax * ((myIntDivCeiling row s_areamax) - 1)

-- get cell value
s_getCell xs row col = xs!!(s_max * (row - 1) + (col - 1))

-- get row values
s_getRow xs row = take s_max (drop (s_max * (row - 1)) xs)

-- get col values
s_getCol xs col = foldr f [] [1..s_max]
  where f row vs = (s_getCell xs row col):vs

-- get area (modular arithmetic)
s_getArea xs area = foldr f [] [0..s_areamax - 1]
  where f n vs = (take s_areamax (drop ((getAreaCol1 area) - 1) (s_getRow xs ((getAreaRow1 area) + n)))) ++ vs
        -- get area first column
        getAreaCol1 area = (mod (area - 1) s_areamax) * s_areamax + 1
        -- get area first row
        getAreaRow1 area = (myIntDivCeiling area s_areamax) * s_areamax - (s_areamax - 1)

-- -------------------------------
-- ------------------------------- CHECK
-- -------------------------------

-- check it all
s_check xs = foldr f True [1..s_max]
  where f n v = if (not v) then False else (check1 (s_getRow xs n)) && (check1 (s_getCol xs n)) && (check1 (s_getArea xs n))
        -- check one sudoko element (row, col, area)
        check1 xs = (all (\x -> x >= 1 && x <= s_max) xs) && (length xs == length (nub xs))

-- check if xs ha x in row, col or area
s_checkHasN row col x xs = (elem x $ s_getRow xs row) || (elem x $ s_getCol xs col) || (elem x $ s_getArea xs $ s_getAreaNum row col)

-- -------------------------------
-- ------------------------------- CREATE
-- -------------------------------

-- get random val 1..smax
s_getRndVal g = randomR (1, s_max) g

-- is rc (row+col) valid?
s_isValidRC rc = (rc > 11 && rc < (s_max * 10 + s_max)) && (mod rc (s_max + 1)) /= 0

-- get random row+col: (rc, g)
s_getRndRC g
  | s_isValidRC $ fst $ getRndRC0 g = getRndRC0 g
  | otherwise = s_getRndVal $ snd $ getRndRC0 g
  where getRndRC0 g = randomR (11, (s_max * 10 + s_max)) g

-- get list of random row+col: [(rc, g)]
s_getRndRCs g 0 ts = ts
s_getRndRCs g n ts = do
  let t = s_getRndRC g
  let rc = fst t
  if (not $ s_isValidRC rc) || (elem rc $ map fst ts) then (s_getRndRCs (snd t) n ts) else (s_getRndRCs (snd t) (n - 1) (t:ts))

-- put a value (x) in the row col of the sudoku grid (xs)
s_putValInGrid row col x xs = do
  let idx = (row - 1) * s_max + col - 1
  take idx xs ++ [x] ++ (drop (idx + 1) xs)

-- fill the sudoku grid (xs) with n (length rcs) random numbers in the specified position (rcs)
s_fillRndGrid g [] xs = xs
s_fillRndGrid g rcs xs = do
                  let r = myGetRow $ head rcs
                  let c = myGetCol $ head rcs
                  let x = fst g
                  let y = (s_getRndVal $ snd g)
                  if s_checkHasN r c x xs then
                    s_fillRndGrid y rcs xs
                    else
                      s_fillRndGrid y (tail rcs) (s_putValInGrid r c x xs)
                  where myGetRow rc = myIntDivFloor rc 10
                        myGetCol rc = rc - (myIntDivFloor rc 10) * 10

-- create sudoku with n numbers
s_create n = do
  let ts = s_getRndRCs (mkStdGen n) n []
  let xs = s_fillRndGrid (s_getRndVal $ snd $ last ts) (map fst ts) (take (s_max * s_max) $ repeat 0)
  -- pro debug: (ts, xs)
  xs

-- -------------------------------
-- ------------------------------- PRINT
-- -------------------------------

-- print it
s_print xs = do
  putStrLn("-----------------------------------------")
  putStrLn("    | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |")
  putStrLn("-----------------------------------------")
  mapM_ (s_printRow xs) [1..s_max]
  where s_printRow xs row = do
                    let rowVals = s_getRow xs row
                    putStrLn("  " ++ (show row) ++ " " ++ (foldr (\v vs -> "  " ++ (if v == 0 then " " else show v) ++ " " ++ vs) "" rowVals))
                    putStrLn("-----------------------------------------")

-- -------------------------------
-- ------------------------------- SOLVE
-- -------------------------------

-- solve it
s_solve xs = s_solve0 (s_getNextT xs) [] xs

s_getNextT xs = case elemIndex 0 xs of
                  Just n  -> (myGetRow n, myGetCol n, 1)
                  Nothing -> (0,0,1)
                where myGetRow idx = (myIntDivFloor idx 9) + 1
                      myGetCol idx = (idx - (myIntDivFloor idx 9) * 9)+1

s_insertv (r,c,x) xs = s_putValInGrid r c x xs
s_checkv (r,c,x) xs = not (s_checkHasN r c x xs)

s_solve0 :: (Int, Int, Int) -> [(Int, Int, Int)] -> [Int] -> [Int]
s_solve0 (0,0,x) ts xs = xs -- finished
s_solve0 (r,c,10) [] xs = take (length xs) $ repeat 0 -- failed
s_solve0 (r,c,10) ts xs = do
  let t = last ts
  let rp = fst3 t
  let cp = snd3 t
  let xp = trd3 t
  s_solve0 (rp, cp, (xp+1)) (init ts) (s_insertv (rp,cp,0) xs) -- searching for a new path
s_solve0 (r,c,x) ts xs
                 | s_checkv (r,c,x) xs = do
                    let ys = s_insertv (r,c,x) xs
                    s_solve0 (s_getNextT ys) (ts ++ [(r,c,x)]) ys
                 | otherwise = s_solve0 (r,c,x+1) ts xs
