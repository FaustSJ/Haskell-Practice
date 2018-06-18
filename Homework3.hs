--sfaust3
--Sarah Faust

--ghci file.hs
module Homework3 where
import Prelude hiding (zipWith)
import Data.List --for sorting

--------------------------------------
--fib and its helper return the n'th value in the fibonacci sequence
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fibHelper n 3 1 1 --for n>2 we require recursive calc
--this recursively counts through fib seq
fibHelper :: Int -> Int -> Int -> Int -> Int
fibHelper n cnt twoFore oneFore = if cnt>=n
                                  then if (n `rem` 2)==0
                                       then oneFore
                                       else oneFore + twoFore
                                  else fibHelper n (cnt+2) (twoFore+oneFore) (oneFore + oneFore + twoFore)

---------------------------------------------------
--reversed and its helper return a reversed version of the given list
reversed :: (Eq a) => [a] -> [a]
reversed [] = [] --if given an empty list, return an empty list
reversed xs = if (length xs)==1
              then xs
              else reversedHelper (tail xs) [(head xs)]
--the recursive helper method
reversedHelper :: [a] -> [a] -> [a]
reversedHelper from to = if (length from)==1
                         then ((head from) : to)
                         else reversedHelper (tail from) ((head from) : to)

------------------------------------------------------
--prime returns a bool statimg whether the given number is prime or not
prime :: Int -> Bool
prime n = if n<=1 --we knock out any easy non-prime numbers...
          then False
          else if n<=3
               then True
               else if (n `rem` 2)==0
                    then False
                    else if (n `rem` 3)==0
                    then False
                    else primeHelper n 5
--run through numbers most likely to be prime and check...
--------if n is divisable by them.
primeHelper :: Int -> Int -> Bool
primeHelper num iter = if (iter*iter)>num
                       then True
                       else if (num `rem` iter)==0
                       then False
                       else if (num `rem` (iter+2))==0
                       then False
                       else primeHelper num (iter+6)

----------------------------------------------------
--nub returns the list xs without any duplicate elements
nub :: (Eq a) => [a] -> [a]
nub [] = [] --if given an empty list, return an empty list
nub xs = if (length xs)==1
         then xs
         else nubHelper (tail xs) [(head xs)]
--the recursive helper function         
nubHelper :: (Eq a) => [a] -> [a] -> [a] --(Eq a) tells ghci equality for a will be tested
nubHelper from to = if (length from)==1 --if we are on the last element...
                    then if ((head from) `elem` to)==False 
                         then reversed ((head from) : to)
                         else reversed to
                    else if ((head from) `elem` to)==False
                         then nubHelper (tail from) ((head from) : to)
                         else nubHelper (tail from) to

-------------------------------------------------
--zip the elements from xs and ys together with the given function
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f xs ys = if (length ys)<(length xs)
                  then if (length ys)==0 --make sure one of the lists isn't empty
                       then []
                       else zipWithHelper f (tail xs) (tail ys) ((f (head xs) (head ys)) : []) ((length ys)-1)
                  else if (length xs)==0 --make sure one of the lists isn't empty
                       then [] 
                       else zipWithHelper f (tail xs) (tail ys) ((f (head xs) (head ys)) : []) ((length xs)-1)
--he recursive helper function
zipWithHelper :: (a -> b -> c) -> [a] -> [b] -> [c] -> Int -> [c]
zipWithHelper _ _ _ to 0 = reverse to
zipWithHelper somf fromx fromy to cnt = zipWithHelper somf (tail fromx) (tail fromy) ((somf (head fromx) (head fromy)) : to) (cnt-1)

--------------------------------------------------
--creates a sequence of numbers from n down to 1 following a set of rules
collatz :: Int -> [Int]
collatz 0 =  [0]
collatz n =  collatzHelper n [n]
--the recursive helper function
collatzHelper :: Int -> [Int] -> [Int]
collatzHelper 1 lis = reverse lis
collatzHelper num lis = if (num `rem` 2)==0
                        --since '/' is only for doubles, we use `div` instead
                        then collatzHelper (num `div` 2) ((num `div` 2) : lis)
                        else collatzHelper ((num*3)+1) (((num*3)+1) : lis)

------------------------------------------------------
--since we can't use global variables, I had to get clever! :<
--this is the main function that finds the mean med and mode of a given list
listReport :: [Int] -> (Double, Double, [Int])
listReport xs = if (length xs)==0
                then (0.0, 0.0, [0])
                else if (length xs)==1
                     then (fromIntegral (head xs), fromIntegral (head xs), [head xs])
                     else ((mean xsor)/(fromIntegral (length xsor)), median xsor, mode xsor)
                     where xsor = (sort xs)
--finds the mean
mean :: [Int] -> Double
mean xs = if (length xs)==0
          then 0.0
          else ((fromIntegral (head xs))+(mean (tail xs)))
--finds the median
median :: [Int] -> Double
median xs =  if ((length xs) `rem` 2)==0
             then ((fromIntegral (getElem (0.0 ::Double) half xs (fromIntegral (length xs))))+(fromIntegral (getElem (0.0 ::Double) (half-1) xs (fromIntegral (length xs)))))/(2.0 ::Double)
             else (fromIntegral (getElem (0.0 ::Double) (half-(0.5 :: Double)) xs (fromIntegral (length xs))))
             where half = ((fromIntegral (length xs))/(2.0 ::Double) ::Double)
--mode now uses one helper instead of two, and no global variables!  
mode :: [Int] -> [Int]
mode xs = if (length xs)==(length (Homework3.nub xs))
          then xs
          else modeHelper xs []
--thank god for haskell's head and tail functions!
modeHelper :: [Int] -> [Int] ->[Int]
modeHelper xsRem modem = if (length xsRem)==1
                         then reversed (Homework3.nub modem)
                         else if ((head xsRem) `elem` (tail xsRem))==True
                              then modeHelper (tail xsRem) ((head xsRem) : modem)
                              else modeHelper (tail xsRem) modem

--getElem grabs and element ising a double index, instead of an int index                              
getElem :: Double -> Double -> [a] -> Double -> a
getElem cur i lis lisSize = if ((cur>i)||(cur==lisSize))
                            then lis !! 0
                            else if cur==i
                                 then head lis
                                 else getElem (cur+(1.0 ::Double)) i (tail lis) lisSize
                                 
-----------------------------------------------------
checkSudoku :: [ [Int] ] -> Bool
--return rowCheck ^ colCheck ^ boxCheck
checkSudoku grid = (checkRows 9 grid) && (checkCols 8 grid) && (checkBoxes 9 grid)
--checking rows
checkRows :: Int -> [ [Int] ] -> Bool
checkRows 0 _ = True
checkRows cnt sudo = if (length cRow)<9
                     then False
                     else checkRows (cnt-1) (tail sudo)
                     where cRow = Homework3.nub (head sudo)
--checking columns
checkCols :: Int -> [ [Int] ] -> Bool
checkCols cnti sudo =  if cnti<0
                       then True
                       else if (length (Homework3.nub curLis))<9
                            then False
                            else checkCols (cnti-1) sudo
                            where curLis = colsHelper cnti 0 [] sudo
--since it's a nested loop, the helper needs a helper
colsHelper :: Int -> Int -> [Int] -> [ [Int] ] -> [Int]
colsHelper _ 9 lis _ = lis
colsHelper cnti cntj lis sudo = colsHelper cnti (cntj+1) (((sudo !! cntj) !! cnti) : lis ) sudo
--Checks the sudoku 3x3 boxes
checkBoxes :: Int -> [ [Int] ] -> Bool
checkBoxes 1 sudo = if (length (Homework3.nub curBox1))<(length curBox1)
                    then False
                    else True
                    where curBox1 = theMainLoop 0 3 0 3 [] sudo
checkBoxes 2 sudo = if (length (Homework3.nub curBox2))<(length curBox2)
                    then False
                    else checkBoxes 1 sudo
                    where curBox2 = theMainLoop 0 3 3 6 [] sudo
checkBoxes 3 sudo = if (length (Homework3.nub curBox3))<(length curBox3)
                    then False
                    else checkBoxes 2 sudo
                    where curBox3 = theMainLoop 0 3 6 9 [] sudo
checkBoxes 4 sudo = if (length (Homework3.nub curBox4))<(length curBox4)
                    then False
                    else checkBoxes 3 sudo
                    where curBox4 = theMainLoop 3 6 0 3 [] sudo
checkBoxes 5 sudo = if (length (Homework3.nub curBox5))<(length curBox5)
                    then False
                    else checkBoxes 4 sudo
                    where curBox5 = theMainLoop 3 6 3 6 [] sudo
checkBoxes 6 sudo = if (length (Homework3.nub curBox6))<(length curBox6)
                    then False
                    else checkBoxes 5 sudo
                    where curBox6 = theMainLoop 3 6 6 9 [] sudo
checkBoxes 7 sudo = if (length (Homework3.nub curBox7))<(length curBox7)
                    then False
                    else checkBoxes 6 sudo
                    where curBox7 = theMainLoop 6 9 0 3 [] sudo
checkBoxes 8 sudo = if (length (Homework3.nub curBox8))<(length curBox8)
                    then False
                    else checkBoxes 7 sudo
                    where curBox8 = theMainLoop 6 9 3 6 [] sudo
checkBoxes 9 sudo = if (length (Homework3.nub curBox9))<(length curBox9)
                    then False
                    else checkBoxes 8 sudo
                    where curBox9 = theMainLoop 6 9 6 9 [] sudo
--this builds up an array of the nums in a 3x3 sudoku box for checkBoxes to check
theMainLoop :: Int -> Int -> Int -> Int -> [Int] -> [ [Int] ] -> [Int]
theMainLoop jfrom jto kfrom kto curBox sudo = if jfrom>=jto
                                              then curBox
                                              else if kfrom>=kto
                                              then theMainLoop (jfrom+1) jto (kfrom-3) kto curBox sudo
                                              else theMainLoop jfrom jto (kfrom+1) kto (((sudo !! kfrom) !! jfrom) : curBox) sudo
