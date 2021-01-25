--Q1
factorialAux :: Int -> Int -> Int
factorialAux 0 r = r
factorialAux n r  = factorialAux (n - 1) (r * n)

factorialTail :: Int -> Int
factorialTail n = factorialAux n 1

binomialCoefficient :: Int -> Int -> Int
binomialCoefficient n k
       | k == 0 = 1
       | k == n = 1
       | n > k = (factorialTail) n `div` (factorialTail k * factorialTail (n - k))
       | otherwise = error "ERROR! n must be greater than k"
--Q2
gcd' :: Int -> Int -> Int
gcd' m n
    | n == 0 = m
    | n > 0 = gcd' n (mod m n)
    | otherwise = 0
--Q3
lcmAux :: Int -> Int -> Int
lcmAux m n 
    | m == 0 = 0
    | n == 0 = 0
    | m == n = m
    | otherwise = div (m * n) (gcd' m n)

lcm' :: Int -> Int -> Int -> Int
lcm' m n o = lcmAux (lcmAux m n) o
--Q4
intSquareRoot :: Int -> Int
intSquareRoot n = aux n
                  where 
                      aux x
                        | (x * x) > n = aux (x - 1)
                        | otherwise = x

intSquareRoot' :: Int -> Int
intSquareRoot' n
               | n >= 0 = intSquareRoot n
               | otherwise = error "ERROR! n must be positive!"
-- Q5
ackermannFunction :: Int ->  Int -> Int
ackermannFunction 0 n = n + 1
ackermannFunction m 0 = ackermannFunction (m - 1) 1
ackermannFunction m n = ackermannFunction (m - 1) (ackermannFunction m (n - 1))

{-- GHCi tests 
--Q1
binomialCoefficient 0 0
binomialCoefficient 4 2
binomialCoefficient 6 2
binomialCoefficient 0 2

--Q2
gcd' 12 9
gcd' 1 9
gcd' 12 9

--Q3
lcm' 10 9 3
lcm' 1 9 3
lcm' 9 9 3
lcm' 19 9 3

--Q4
intSquareRoot' 0
intSquareRoot' 1
intSquareRoot' 5
intSquareRoot' (-5)
intSquareRoot' 9

--Q5
ackermannFunction 0 0
ackermannFunction 0 1
ackermannFunction 1 0
ackermannFunction 1 1
ackermannFunction 1 2
ackermannFunction 2 4
--}