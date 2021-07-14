module BrokenModule where

prop_isSum :: [Int] -> Bool
prop_isSum xs = broken xs == sum xs


broken :: [Int] -> Int
broken = foldl (-) 0

add :: Int -> Int -> Int
add = (+)

main :: IO ()
main = print "Unrelated main function"
