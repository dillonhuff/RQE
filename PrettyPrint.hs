module PrettyPrint(sumList) where

sumList :: Show a => String -> [a] -> String
sumList s [] = ""
sumList s [x] = show x
sumList s (x:y:[]) = show x ++ " " ++ s ++ " " ++ show y
sumList s (x:y:rest) = show x ++ " " ++ s ++ " " ++ show y ++ " " ++ s ++ " " ++ (sumList s rest)

