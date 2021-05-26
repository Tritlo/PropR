module MagicConstant where

prop_isTheAnswer :: Bool
prop_isTheAnswer = theAnswer == 42

theAnswer :: Int
theAnswer = 17

main :: IO ()
main = print "Unrelated main function"
