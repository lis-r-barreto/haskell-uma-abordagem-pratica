--Q1
sumWithLimits :: Int -> Int -> Int
sumWithLimits number1 number2
             | number1 == number2 = number1
             | number1 < number2 = number1 + sumWithLimits (number1 + 1) number2
             | otherwise = error "ERROR! The first integer must be greater than the second integer."

sumWithoutLimits :: Int -> Int -> Int
sumWithoutLimits number1 number2
             | number1 == number2 = number1
             | number1 < number2 = sumWithLimits (number1 + 1) (number2 - 1)
             | otherwise = error "ERROR! The first integer must be greater than the second integer."
-- Q2
multi :: Int -> Int -> Int
multi m n
      | n < m = 0
      | otherwise = 1 + multi m (n-m)

multiples :: Int -> Int -> Int -> Int
multiples number1 number2 number3
          | number1 > number2 = multiples number2 number1 number3
          | otherwise = (multi number3 number2) - (multi number3 (number1 - 1))
-- Q3
productUsingSum :: Int -> Int -> Int
productUsingSum n1 n2
           | n1 > 0 || n2 > 0 = n2 + productUsingSum n2 (n1-1)
           | n1 < 0 && n2 < 0 = negate (productUsingSum n1 (negate n2))
           | otherwise = 0

{- GHCi tests 
- Q1
sumWithLimits 0 1
sumWithLimits 1 2

sumWithoutLimits 0 1
sumWithoutLimits 1 2

- Q2
multiples 1 10 1
multiples 0 50 5
multiples 10 9 1

- Q3
productUsingSum 0 0
productUsingSum 1 0
productUsingSum 0 1
productUsingSum 1 1
productUsingSum 10 10
productUsingSum (-10) 10
productUsingSum 10 (-10)
productUsingSum (-10) (-10)
-}