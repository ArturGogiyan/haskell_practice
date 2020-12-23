module Lab1 where


--If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
--Find the sum of all the multiples of 3 or 5 below 1000.

isMultiple x y = isZero $ mod x y where
    isZero = (==) 0

isMultipleOf_3_Or_5 x = isMultiple x 3 || isMultiple x 5


findMultiplesByReqursion = helper 999 where
    helper 0 = 0
    helper x | isMultipleOf_3_Or_5 x = x + helper (x - 1)
             | otherwise             = helper (x - 1)

findMultiplesByFold = foldr (\x sum -> if isMultipleOf_3_Or_5 x then x + sum else sum) 0 [1..999]

findMultiplesByListComprehension = sum [j | j <- [1..999] , isMultipleOf_3_Or_5 j]

findMultiplesByMap = sum $ map (\x -> if isMultipleOf_3_Or_5 x then x else 0) [1..999]

findMultiplesByFilter = sum $ filter isMultipleOf_3_Or_5 [1..999]

