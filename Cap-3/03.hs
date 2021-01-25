--Q1 a)
factorial :: Int -> Int
factorial n
         | n == 0 = 1
         | otherwise = n * factorial(n - 1)

factorial' :: Int -> Int
factorial' n
          | n < 0 =  error "ERROR! The integer must be greater than zero."
          | otherwise = factorial n
          
exp' :: Int -> Int -> Float
exp' _ 0 = 1.0
exp' x n = p / f + exp' x (n-1)
    where
         f = fromIntegral (factorial' n)
         p = fromIntegral (x^n)
--Q1 b)
absoluteError :: Int -> Float
absoluteError n = (exp 2) - (exp' 2 n) -- absoluteError = exactValue - approximation

howManyTermsAreNeeded :: Int -> Float
howManyTermsAreNeeded n
                       | absoluteError n <= 0.001 = absoluteError n
                       | otherwise =  error "Error! Try again. Enter another integer."
--Q2
auxMod n d
       | n < d = n
       | otherwise = auxMod (n-d) d

mod' n d
       | d == 0 = error "Error! Division by zero."
       | otherwise = auxMod n d
--Q3 a)
sequence' n
     | n == 1 = sqrt(6)
     | n == 2 = sqrt(6 + sqrt(6))
     | n > 2 = sqrt(6 + sequence' (n-1)) -- recursive sum
     | otherwise = error "Error! The integer must be greater than zero."

--Q3 b) GHCi test: sequence' 10

{-GHCi tests
--Q1 a)
exp' 0 10
exp' 1 10
exp' 2 10

--Q1 b)
howManyTermsAreNeeded 8
howManyTermsAreNeeded 9
howManyTermsAreNeeded 10

--Q2
mod' 1 0
mod' 0 1
mod' 10 2
mod' 10 3
mod' 11 4

--Q3
sequence' 0
sequence' 1
sequence' 3
sequence' 10
-}