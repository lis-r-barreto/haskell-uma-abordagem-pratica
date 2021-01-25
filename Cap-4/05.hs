--Q1
import Data.Char

difference :: Int
difference = ord('a') - ord('A') --32

upperToLower :: Char -> Char
upperToLower ch = chr((ord ch) + difference)

lowerToUpper :: Char -> Char
lowerToUpper ch = chr((ord ch) - difference)

convert :: Char -> (Char, Char, Int)
convert x 
     | x >= 'a' && x <= 'z' = (x, lowerToUpper x, ord(x))
     | x >= 'A' && x <= 'Z' = (x, upperToLower x, ord(x))
     | otherwise = error "ERRO! Digite um caractere válido."
--Q2
type Meu_tipo = (String, Float, Char)

pessoa rg
  | rg == 1 = ("Joao Silva", 12, 'm')
  | rg == 2 = ("Jonas Souza", 51, 'm')
  | rg == 3 = ("Joice Silva", 21, 'f')
  | rg == 4 = ("Janete Souza", 55, 'f')
  | rg == 5 = ("Jocileide Strauss", 21, 'f')
  | otherwise = ("Nao ha ninguem mais", 9999, 'x')

idade :: Meu_tipo -> Float
idade (x, y, z) = y

menor :: Meu_tipo -> Meu_tipo -> Meu_tipo
menor x y
  | idade x <= idade y = x
  | otherwise = y

menor_idade :: Float -> Meu_tipo
menor_idade x
  | x == 1 = pessoa 1
  | otherwise =  menor (pessoa x) (menor_idade(x-1))
--Q2 a)
nome :: Meu_tipo -> String
nome (x, y, z) = x
menor_idade_nome x = nome (menor_idade x)
--Q2 b)
soma_idades n
  | n == 1 = idade(pessoa 1)
  | otherwise = idade (pessoa n) + idade (pessoa (n - 1))

media_idades n = soma_idades n / n
--Q2 c)
sexo (x, y, z) = z
quantosM x
  | x == 1 = 1
  | x > 1 && sexo (pessoa x) == 'm' = 1 + quantosM (x - 1)
  | otherwise = quantosM (x - 1)
--Q2 d)
maior :: Meu_tipo -> Meu_tipo -> Meu_tipo
maior x y
  | idade x >= idade y  && idade x < 9999 = x
  | otherwise = y

maior_idade :: Float -> Meu_tipo
maior_idade x
  | x == 1 = pessoa 1
  | otherwise =  maior (pessoa x) (maior_idade(x-1))

rg_maior_idade x
  | maior_idade x == pessoa 1 = 1
  | maior_idade x == pessoa 2 = 2
  | maior_idade x == pessoa 3 = 3
  | maior_idade x == pessoa 4 = 4
  | maior_idade x == pessoa 5 = 5
  | otherwise = error "Nao consta no registro"--}
--Q4
minimumElement :: Int -> Int -> Int
minimumElement a b = if a <= b then a else b

smallest ::[Int] -> Int
smallest [x] = x
smallest (x:xs) = minimumElement x (smallest xs)

removeFromTheList :: Int -> [Int] -> [Int]
removeFromTheList _ [] = []
removeFromTheList y (x:xs)
  | y == x = xs
  |otherwise = x : removeFromTheList y xs

selectionSort :: [Int] -> [Int]
selectionSort [] = []
selectionSort zs = m : selectionSort (removeFromTheList m zs)
           where m = smallest zs

integersToList :: a -> a -> a -> a -> [a]
integersToList a b c d = [a, b, c, d]

listToTuple :: [d] -> (d, d, d, d)
listToTuple [a,b,c,d] = (a,b,c,d)

orderIntegers :: Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
orderIntegers a b c d= listToTuple(selectionSort((integersToList a b c d)))

{- GHCi tests 
--Q1
convert 'A'
convert 'a'
convert 'Z'
convert 'z'
convert '!'

--Q2 a)
menor_idade_nome 5

--Q2 b)
media_idades 5

--Q2 c)
quantosM 5

--Q2 d)
rg_maior_idade 5

--Q3
Idêntica à Q1

--Q4
orderIntegers 4 3 2 1
orderIntegers 3 4 1 2
--}