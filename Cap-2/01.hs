-- Q1
threeEqual :: Int -> Int -> Int -> Int
threeEqual number1 number2 number3
           | allNumbersEqual = 3
           | number1IsDifferent  = 2
           | number2IsDifferent  = 2
           | number3IsDifferent  = 2
           | otherwise = 0
           where allNumbersEqual = (number1 == number2) && (number1 == number3)
                 number1IsDifferent = (number1 /= number2) && (number1 /= number3) && (number2 == number3)
                 number2IsDifferent = (number2 /= number1) && (number2 /= number3) && (number1 == number3)
                 number3IsDifferent = (number3 /= number1) && (number3 /= number2) && (number1 == number2)
-- Q2
aboveAverage :: Float -> Float -> Float -> Int
aboveAverage a b c
    | aLarger && not  bLarger && not cLarger  = 1
    | bLarger && not aLarger && not cLarger  = 1
    | cLarger && not aLarger && not  bLarger  = 1
    | cLarger && aLarger && not  bLarger  = 2
    | cLarger && not aLarger &&  bLarger  = 2
    | not cLarger && aLarger &&  bLarger  = 2
    | otherwise                       = 3
    where aLarger = a > (a + b + c)/3
          bLarger = b > (a + b + c)/3
          cLarger = c > (a + b + c)/3
-- Q3
power_2 :: Int -> Int
power_2 number = number * number
-- Q4
power_4 :: Int -> Int
power_4 number = power_2 number * power_2 number
-- Q5
exclusiveOr ::  Bool -> Bool -> Bool
exclusiveOr number1 number2 = (number1 || number2) && not (number1 && number2)
-- Q6
numberNDroots :: Float -> Float -> Float -> Integer   -- ND; Non-Degenerate.
numberNDroots a b c

 | b^2 > 4.0 * a * c  = 2
 | b^2 == 4.0 * a * c  = 1
 | otherwise          = 0   -- | b^2 < 4.0 * a * c  = 0

numberRoots :: Float -> Float -> Float -> Integer
numberRoots a b c

 | a /= 0.0                = numberNDroots a b c
 | b /= 0.0                = 1
 | b == 0.0  &&  c == 0.0  = 3
 | otherwise               = 0    -- | b == 0.0  &&  c /= 0.0  = 0

smallerRoot, largerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c
            | 0 == numberRoots a b c ||  3 == numberRoots a b c  = 0.0
            | 1 == numberRoots a b c ||  2 == numberRoots a b c  = 
              (-(b + sqrt(b^2 - 4.0 * a * c)) / 2 * a) *
              (if (a < 0) then (-1) else 1)
 
largerRoot a b c
          | 0 == numberRoots a b c ||  3 == numberRoots a b c  = 0.0
          | 1 == numberRoots a b c ||  2 == numberRoots a b c  =
                 (((-b) + sqrt (b^2 - 4.0 * a * c)) / 2 * a) * 
                 (if (a < 0) then (-1) else 1)
{- GHCi tests 
- Q1
equals 1 1 1
equals 1 2 3 
equals 1 1 2
equals 1 2 2
equals 1 2 1 

- Q2
aboveAverage 1 2 3
aboveAverage 1 0 0
aboveAverage 1 0 2
aboveAverage 0 1 0

- Q3
power_2 2
power_2 0
power_2 (-2)

- Q4
power_4 2
power_4 0
power_4 (-2)

- Q5
exclusiveOr True False
exclusiveOr False True
exclusiveOr False False
exclusiveOr True True

- Q6
smallerRoot 1 12 (-13)
largerRoot 1 12 (-13)
smallerRoot 2 (-16) (-18)
largerRoot 2 (-16) (-18)
-}