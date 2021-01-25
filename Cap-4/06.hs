--Q1 
type Date  = (Float, Float, Float)

day, month, year :: Date -> Float
day (day, month, year) = day
month (day, month, year) = month
year (day, month, year) = year

howManyDays :: Date -> Date -> Int
howManyDays (day1, month1, year1) (day2, month2, year2)
  | ((day1 <= day2) && (month1 <= month2) && (year1 <= year2))
    = truncate ((date2InMilis - date1InMilis) * milisInDays)
  | otherwise = error "Erro! A data 1 deve ser menor que a data 2."
    where
      date1InMilis = (day1 * 86400000 + month1 * 2629800000 + year1 * 31557600000)
      date2InMilis = (day2 * 86400000 + month2 * 2629800000 + year2 * 31557600000)
      milisInDays = 0.000000011574074074074074
--Q2
type Coefficients = (Float, Float, Float)

a, b, c :: Coefficients -> Float
a (a, b, c) = a
b (a, b, c) = b
c (a, b, c) = c

delta :: Coefficients -> Float
delta (a, b, c) = sqrt(b^2 - 4.0 * a * c)

bhaskara :: Coefficients -> (Float, Float)
bhaskara (a, b, c)
  | delta (a, b, c) >= 0 = ((-b - delta (a, b, c)) / (2 * a), (-b + delta (a, b, c)) / (2 * a))
  | otherwise = error "Erro! Raízes complexas."
--Q3
type Sides = (Int, Int, Int)

x, y, z :: Sides -> Int
x (x, y, z) = x
y (x, y, z) = y
z (x, y, z) = z

isATriangle :: Sides -> Int
isATriangle (x, y, z)
  | ((x + y) > z) && ((x + z) > y) && ((y + z) > x) = 1
  | otherwise = 0

typeOfTriangle :: Sides -> String
typeOfTriangle (x, y, z)
          | (x == y && x == z && y == z) = "Equilatero"
          | (x /= y && x /= z && y /= z) = "Escaleno"
          | otherwise = "Isosceles"

perimeterOfTriangle :: Sides -> Int
perimeterOfTriangle (x, y, z) = x + y + z

triangle :: Sides -> (String, Int)
triangle (x, y, z)
  | isATriangle (x, y, z) == 1 = (typeOfTriangle (x, y, z), perimeterOfTriangle (x, y, z))
  | otherwise = error "Erro! Não é um triângulo."
--Q4
type Meu_Tipo = (Int, String, String, Char) 

base :: Int -> Meu_Tipo
base x
         |x==0 = (1793, "Pedro Paulo", "MESTRE", 'M')
         |x==1 = (1797, "Joana Silva Alencar", "MESTRE", 'F')
         |x==2 = (1534, "Joao De Medeiros", "DOUTOR", 'M')
         |x==3 = (1267, "Claudio Cédar de Sa", "DOUTOR", 'M')
         |x==4 = (1737, "Paula de Medeiros", "MESTRE", 'F')
         |x==5 = (1888, "Rita de Matos", "MESTRE", 'F')
         |x==6 = (1888, "Ana Cristina Oliveira", "DOUTOR", 'F')
         |x==7 = (1267, "Adriano Goncalves Dias", "DOUTOR", 'M')
         |x==8 = (1737, "Marta Queiroz", "MESTRE", 'F')
         |x==9 = (1698, "Tereza Cristina Andrade", "MESTRE", 'F')
         |x==10 = (9999999, "Fim do registro! Não há mais nomes", " ", '0')
-- Q4.a)
formacao (w, x, y, z) = y
quantosDoutores x
  | x == 0 = 0
  | x > 1 && formacao (base x) == "DOUTOR" = 1 + quantosDoutores (x - 1)
  | otherwise = quantosDoutores (x - 1)
-- Q4.b)
sexo (w, x, y, z) = z
quantasMulheres x
  | x == 1 = 1
  | x > 1 && sexo (base x) == 'F' = 1 + quantasMulheres (x - 1)
  | otherwise = quantasMulheres (x - 1)
-- Q4.c)
quantosMestresHomens x
  | x == 1 = 1
  | x > 1 && formacao (base x) == "MESTRE" && sexo (base x) == 'M' = 1  + quantosMestresHomens (x - 1)
  | otherwise = quantosMestresHomens (x - 1)
-- Q4.d)
matricula :: Meu_Tipo -> Int
matricula (w, x, y, z) = w

menor :: Meu_Tipo -> Meu_Tipo -> Meu_Tipo
menor x y
  | matricula x <= matricula y = x
  | otherwise = y

menor_matricula :: Int -> Meu_Tipo
menor_matricula x
  | x == 1 = base 1
  | otherwise =  menor (base x) (menor_matricula(x-1))
--Q2 a)
nome :: Meu_Tipo -> String
nome (w, x, y, z) = x
menor_matricula_nome x = nome (menor_matricula x)

{-- GHCi tests
--Q1
howManyDays (1, 10, 2019) (1, 10, 2020) -> 365
howManyDays (1, 8, 2018) (1, 10, 2019)-> 426
howManyDays (1, 8, 2000) (1, 10, 2019) -> 7000
howManyDays (1, 10, 2020) (1, 10, 2019)-> Exception: Erro! A data 1 deve ser menor que a data 2.

--Q2
bhaskara (1, 8, (-9)) -> (-9.0,1.0)
bhaskara (1, 12, (-13)) -> (-13.0,1.0)
bhaskara (4, (-4), 1) -> (0.5,0.5)
bhaskara (1, (-4), 5) -> Exception: Erro! Raízes complexas.

--Q3
triangle (7, 7, 11) -> ("Isosceles",25)
triangle (1, 1, 1) -> ("Equilatero",3)
triangle (208, 203, 145) -> ("Escaleno",556)
triangle (208, 203, 0) -> Exception: Erro! Não é um triângulo.

--Q4 a)
quantosDoutores 10 -> 4

--Q4 b)
quantasMulheres 10 -> 6

--Q4 c)
quantosMestresHomens 10 -> 1

--Q4 d)
menor_matricula_nome 10 -> "Adriano Goncalves Dias"
--}