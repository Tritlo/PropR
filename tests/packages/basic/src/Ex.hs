module Ex where

foldInt :: (Int -> Int -> Int) -> Int -> [Int] -> Int
foldInt _ z [] = 0
foldInt f z (x : xs) = (foldInt f z xs) `f` x
