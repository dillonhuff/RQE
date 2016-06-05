module PrettyPrint(sumList) where

sumList :: Show a => [a] -> String
sumList [] = ""
sumList [x] = show x
sumList (x:y:[]) = show x ++ " + " ++  show y
sumList (x:y:rest) = show x ++ " + " ++  show y ++ " + " ++ (sumList rest)

