--4.1) Faça uma função que retorne a média de um [Double], usando foldl .

med :: [Double] -> Double
med lista = total / fromIntegral tamanho
    where
        total = foldl (\acc x -> acc + x) 0 lista
        tamanho = length lista


--4.2) Faça uma função que receba uma [String] e retorne todos os elementos palíndromos. Ver exercício 3.7

recebeString :: [String] -- Assinatura correta
recebeString = ["ovo", "arroz", "subinoonibus"]


retornaElementos :: ([String]) -> [String]
retornaElementos = filter (\x -> x == reverse x)

{- 4.3) Implemente uma função que filtre os números pares e
outra que filtre os ímpares de uma lista recebida via parâmetro -}


numPares :: [Int] -> [Int]
numPares = filter (\x -> x `mod` 2 == 0)

numImpares :: [Int] -> [Int]
numImpares = filter (\x -> x `mod` 2 /= 0)


{- 4.4) Filtre os números primos de uma lista recebida por
parâmetro. -}

verifPrimo :: Int -> Bool
verifPrimo n
    |n <= 1 = False
    |otherwise = not $ any (\x -> n `mod` x == 0) [2..n-1]
    -- A lista é aplicada na função anonima any (\x -> n `mod` x == 0)
    -- o not vai negar caso seja verdadeiro


{- 
4.5) Implemente uma função que receba uma lista de inteiros e
retorne o dobro de todos, eliminando os múltiplos de 4. -}


eliminaMult :: [Int] -> [Int]
eliminaMult lista = [ x*2 | x <- lista, (x*2) `mod` 4 /= 0]




{- 4.6) Faça uma função func que receba uma função f de tipo
 (String -> String) , e uma String s que retorna o reverso
de s concatenado com aplicação da função f em s . -}



func :: (String -> String) -> String -> String
func f s = f (reverse s)

f :: String -> String
f str = "Aleatorio" ++ str


{- 4.7) Crie um tipo Dia contendo os dias da semana. Faça uma
função que receba uma lista de Dias e filtre as Terças . -}

data Dia = Seg | Ter | Quar | Qui| Sex | Sab | Dom deriving (Show, Eq)
-- Adicionando derivations para permitir a exibição e comparação

recebDia :: [Dia] -> [Dia]
recebDia = filter (\x -> x == Ter)



{- 4.8) Implemente o tipo Dinheiro que contenha os campos valor e correncia ( Real ou Dolar ), e uma função que converta todos os "dinheiros" de uma lista para dólar (e outra para real). Com isso, implemente funções para: 

Filtrar todos os Dolares de uma lista de Dinheiro . 
Somar todos os Dolares de uma lista. 
Contar a quantidade de Dolares de uma lista. -}

data Correncia = Real | Dolar deriving  (Show, Eq)
data Dinheiro = Dinheiro { valor :: Double, correncia :: Correncia } deriving Show

paraDolar :: [Dinheiro] -> [Dinheiro]
paraDolar = map (\(Dinheiro v c) -> if c == Real then Dinheiro (v / 5.0) Dolar else Dinheiro v Dolar)

paraReal :: [Dinheiro] -> [Dinheiro]
paraReal = map (\(Dinheiro v c) -> if c == Dolar then Dinheiro (v * 5.0) Real else Dinheiro v Real)

filtrarDolares :: [Dinheiro] -> [Dinheiro]
filtrarDolares = filter (\(Dinheiro _ c) -> c == Dolar)

somaDolares :: [Dinheiro] -> Double
somaDolares = sum . map valor . filtrarDolares

contarDolares :: [Dinheiro] -> Int
contarDolares = length . filtrarDolares



{- 4.9) Usando a função foldl , crie lambdas para:

Contar números negativos de uma lista de Int .
Contar letras 'P' de uma String .
Para contar Sabados em uma lista de um[DiaSemana] .
Para, a partir de uma lista de [DiaSemana] , retornar
a soma dos dias. Exemplo: [Segunda, Segunda,
Quarta] deve retornar 5 . Use uma função auxiliar
para converter DiaSemana para Int . -}

listaNegativos :: [Int] -> Int
listaNegativos = foldl (\acc x -> if x < 0 then acc + 1 else acc) 0

contarP :: String -> Int
contarP = foldl (\acc x -> if x == 'p' then acc + 1 else acc) 0

contarSab :: [Dia] -> Int
contarSab = foldl (\acc x -> if x == Sab then acc + 1 else acc) 0

funcAuxiliarDia :: Dia-> Int
funcAuxiliarDia Seg = 1
funcAuxiliarDia Ter = 2
funcAuxiliarDia Quar = 3
funcAuxiliarDia Qui = 4
funcAuxiliarDia Sex = 5
funcAuxiliarDia Sab = 6
funcAuxiliarDia Dom = 7

somarDias :: [Dia] ->Int
somarDias = foldl (\acc dia -> acc + funcAuxiliarDia dia) 0


