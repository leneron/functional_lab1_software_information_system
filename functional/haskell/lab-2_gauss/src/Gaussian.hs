{-# Language ViewPatterns #-}

module Main (
    main
) where

import Control.Monad (liftM)
import Data.Maybe
import Data.List
import GHC.Conc.Sync
import System.FilePath.Posix

--HELPERS

--Searching the row index with maximum element in the column
max_row :: Ord a => Int -> [[a]] -> Maybe Int
max_row n matrix =
    elemIndex (maximum column) column
    where
        column = fmap (!! n) matrix

--Swapping two elements in the list
swap_elements :: Int -> Int -> [a] -> [a]
swap_elements i j xs
  | i > j = swap_elements j i xs
  | i == j = xs
  | i < j = let
    (part_1, rest_part_1) = splitAt i xs
    (part_2, rest_part_2) = splitAt (j - i - 1) (tail rest_part_1)
    part_3 = tail rest_part_2
  in part_1 ++ [xs !! j] ++ part_2 ++ [xs !! i] ++ part_3

multiply a b = map (\(x,y) -> x * y) $ zip a b

--Getting last n elements of the list
last_n :: Int -> [a] -> [a]
last_n n xs = drop (length xs - n) xs

--GAUSSIAN ELIMINATION

--Parallel 
forward_elimination_iter :: (Num a, Ord a, Fractional a) => [a] -> [[a]] -> Int -> [[a]]
forward_elimination_iter [] _ _ = []
forward_elimination_iter _ [] _ = []
forward_elimination_iter first_line (curr_line:next_lines) column_no = 
  par a (par b (a:b))
  where 
    a = fmap (count_line c) $ zip first_line curr_line
    b = forward_elimination_iter first_line next_lines column_no
    c = coeff first_line curr_line column_no
    coeff x y k = (y !! k) / (x !! k)
    count_line = \c (x,y) -> y - x * c
        
forward_elimination :: (Ord a, Fractional a) => [[a]]-> Int -> [[a]]
forward_elimination [] _ = []
forward_elimination lines column_no =
  (a:b)
  where
    a = first_line
    b = forward_elimination (tail divided) (column_no + 1)
    swapped_lines = swap_elements 0 (fromJust (max_row column_no lines)) lines
    first_line = head swapped_lines
    divided = (forward_elimination_iter first_line swapped_lines column_no) 

backward_substitution :: (Ord a, Num a, Fractional a) => [[a]] -> Int -> [a] -> [a]
backward_substitution _ (-1) result = result
backward_substitution [] _ result = result
backward_substitution all_lines column_no result = 
  backward_substitution (init all_lines) (column_no - 1) (a:result)
  where 
    a = (b - sum (multiply last_x result)) / (x !! column_no)
    b = last x
    last_x = last_n (length result) (init x)
    x = last all_lines
    
    
gauss :: (Ord a, Fractional a) => [[a]] -> [a]
gauss matrix =
    backward_substitution (forward_elimination matrix 0) ((length matrix) - 1) []

--PARSING HELPERS

read_ints :: (Read a) => String -> [a]
read_ints = map read . words

parse_matrix :: (Read a) => [String] -> [[a]]
parse_matrix all_lines = parse_matrix' all_lines []
    where parse_matrix' [] result = reverse result
          parse_matrix' (x:xs) result = parse_matrix' xs $ (read_ints x) : result

read_file :: (Read a, Fractional a) => FilePath -> IO ([[a]])
read_file file_name = do
    all_lines <- (liftM lines . readFile) file_name
    return $ parse_matrix all_lines 

--MAIN

main = do
    putStrLn "Please, specify the testcase filename:"
    file_name <- getLine
    putStrLn "Reading the file..."
    matrix <- (read_file file_name) 
    putStrLn $ show matrix
    putStrLn "Computing the result..."
    let 
      size = length matrix
      result =  if ((size > 0) && (size == (length (matrix !! 0) - 1))) 
                then (show (gauss matrix))
                else "Couldn't solve such linear equation" 
    putStrLn result 