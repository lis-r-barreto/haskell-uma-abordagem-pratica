--Q1
maximum' :: (Ord a) => [a] -> (a, Int)
maximum' l = 
  let pmaximum' :: (Ord a) => [a] -> Int -> (a, Int)
      pmaximum' [] _  = error "Erro! A lista está vazia."           
      pmaximum' [x] xi = (x, xi)                     
      pmaximum' (x:xs) xi                            
        | x > t     = (x, xi)                     
        | otherwise = (t, ti)                     
        where (t, ti) = pmaximum' xs (xi + 1)        
  in pmaximum' l 0                                   
--Q2
dic_10 :: [(Int, [Char])]
dic_10 = [(0,"zero"), (1,"um"), (2,"dois"), (3,"tres"), (4,"quatro"), 
          (5,"cinco"), (6,"seis"), (7,"sete") , (8,"oito"), (9,"nove")]

list_10 :: [Int]
list_10 = (fst (unzip dic_10))

list_ten :: [String]
list_ten = (snd (unzip dic_10))

isInAList :: Int -> [Int] -> Int 
isInAList n list_10
   | elem n list_10 == True = 1
   | otherwise = 0

translate n list_10
  | isInAList n list_10 == 1 = list_ten!!n
  | otherwise  = error "ERRO! O elemento nao esta no dicionario."   
--Q3
dada_lista = [("joao",21), ("alex",32), ("aloisio",12)]

list_names :: [String]
list_names = (fst (unzip dada_lista))

list_ages :: [Int]
list_ages = (snd (unzip dada_lista))

min' :: [Int] -> Int
min' []       = 0
min' [x]      = x
min' (x:xs)   = min x (min' xs)

max' :: [Int] -> Int
max' []       = 0
max' [x]      = x
max' (x:xs)   = max x (max' xs)

youngest_oldest :: p -> (String, String)
youngest_oldest dada_lista
  | (min' list_ages == list_ages!!0) && (max' list_ages == list_ages!!1) 
     = (list_names!!0, list_names!!1)
  | (min' list_ages == list_ages!!1) && (max' list_ages == list_ages!!0) 
     = (list_names!!1, list_names!!0)
  | (min' list_ages == list_ages!!1) && (max' list_ages == list_ages!!2) 
     = (list_names!!1, list_names!!2)
  | otherwise = (list_names!!2, list_names!!1)
--Q4
deletePosition :: [Int] -> Int -> [Int]
deletePosition list n
    | [1| x <- list, (position n) == x] == [] || n > ((length list) - 1) || n < 0 
      = error "Erro! Essa posicao nao existe."
    | otherwise    =  [x| x <- list, (position n) /= x]
    where position n = list!!n

delPosition :: [Int] -> Int -> [Int]
delPosition list n
    | n > ((length list) - 1) || n < 0 = error "Erro! Essa posicao nao existe."
    | otherwise = deletePosition list n
--Q5
minimumElement :: Int -> Int -> Int
minimumElement a b = if a <= b then a else b

smallest ::[Int] -> Int
smallest [x] = x
smallest (x:xs) = minimumElement x (smallest xs)

removeFromTheList :: Int -> [Int] -> [Int]
removeFromTheList _ [] = []
removeFromTheList y (x:xs)
  | y == x = xs
  | otherwise = x : removeFromTheList y xs

selectionSort :: [Int] -> [Int]
selectionSort [] = []
selectionSort zs = m : selectionSort (removeFromTheList m zs)
           where m = smallest zs

isOdd x = (mod x 2) /= 0

filter' :: (a -> Bool) -> [a] -> [a]
filter' p (x:xs) | p x       = x : filter' p xs
                 | otherwise = filter' p xs
filter' _ []                 = []

list_odd_numbers (x:xs)= filter' isOdd (selectionSort (x:xs))
--Q6
addPosition :: [Int] -> Int -> Int -> [Int]
addPosition list position n
    | [1| x <- list, n == x] == [] = list ++ [n]
    | otherwise = error "ERRO!"
    where position n = list!!n

insertAt :: Int -> Int -> [Int] -> [Int]
insertAt newElement 0 as = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as

insertPosition :: [Int] -> Int -> Int -> [Int]
insertPosition list position n
    | position > ((length list) - 1) = addPosition list position n
    | otherwise = insertAt n position list
--Q7
nthElement n xs = head (drop n xs)
--Q8
replicate' a 0 = []
replicate' a b = a : replicate' a (b - 1)

replicate_number 0 = []
replicate_number n = replicate' n n ++ replicate_number (n-1)
--Q9
palindrome :: [Char] -> Bool 
palindrome [] = True 
palindrome [x] = True  
palindrome xs = head xs == last xs && palindrome (init (tail xs))

{-- GHCi tests
--Q1
maximum' [9,2,3,14,29] -> (29,4)
maximum' [9,9,35,9,29] -> (35,2)
maximum' [] -> Exception: Erro! A lista está vazia.

--Q2
translate 0 list_10 -> "zero"
translate 9 list_10 -> "nove"
translate 10 list_10 -> Exception: ERRO! O elemento nao esta no dicionario.

--Q3
youngest_oldest dada_lista -> ("aloisio","alex")

--Q4
delPosition [1, 2, 3, 4, 5] 4 -> [1, 2, 3, 4]
delPosition [1..10] 9 -> [1,2,3,4,5,6,7,8,9]
delPosition [1, 2, 3, 4, 5] 40 -> Exception: Erro! Essa posicao nao existe.
delPosition [] 4 -> Exception: Erro! Essa posicao nao existe.

--Q5
list_odd_numbers [67,5,2,99,0,1,3] -> [1,3,5,67,99]
list_odd_numbers [67,5,2,99,0,0,0,0] -> [5,67,99]
list_odd_numbers [1,1,1,1,2,3,3,3,3] -> [1,1,1,1,3,3,3,3]

--Q6
insertPosition [1,2,3,4] 0 5 -> [5,1,2,3,4]
insertPosition [1,2,3,4] 4 5 -> [1,2,3,4,5]
insertPosition [1,2,3,4] 5 5 -> [1,2,3,4,5]
insertPosition [1,2,3,4] 10 5 -> [1,2,3,4,5]

--Q7
nthElement 0 [1,2,3,4,5,6] -> 1
nthElement 3 [1,2,3,4,5,6] -> 4
nthElement 5 [1,2,3,4,5,6] -> 6

--Q8
replicate_number 7 -> [7,7,7,7,7,7,7,6,6,6,6,6,6,5,5,5,5,5,4,4,4,4,3,3,3,2,2,1]
replicate_number 2 -> [2,2,1]
replicate_number 0 -> []

--Q9
palindrome "arara" -> True
palindrome "palavra" -> False
--}