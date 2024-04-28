import Data.Monoid
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

{- EXERCICIOS DO CAPITULO 5 -}


{- 5.1) Crie o tipo TipoProduto que possui os values constructors Escritorio , Informatica , Livro , Filme e Total . O tipo Produto possui um value constructor - de mesmo nome - e os campos valor ( Double ), tp ( TipoProduto ) e um value constructor Nada , que representa a ausência de um Produto . Deseja-se calcular o valor total de uma compra, de modo a não ter nenhuma conversão para inteiro e de forma combinável. Crie uma instância de monoide para Produto , de modo que o retorno sempre tenha Total no campo tp e a soma dos dois produtos em valor . Explique como seria o exercício sem o uso de monoides. Qual(is) seria(m) a(s) diferença(s)? -}

data TipoProduto = Escritorio | Informatica | Livro | Filme | Total deriving (Show, Eq)

data Produto = Produto { valor :: Double, tp :: TipoProduto } | Nada deriving (Show, Eq)


instance Monoid Produto where
    mempty = Nada
    mappend Nada p = p
    mappend p Nada = p
    mappend (Produto v1 _) (Produto v2 _) = Produto (v1 + v2) Total

{- Sem usar monoides, precisaríamos escrever uma função para calcular o valor total da compra, bem como lidar com os casos em que um dos produtos é "Nada". Teríamos que verificar manualmente se um dos produtos é Nada e retornar o outro produto como resultado. Além disso, precisaríamos calcular a soma dos valores dos produtos e criar um novo produto com o tipo Total.


A principal diferença é que, ao usar monoides, podemos aproveitar as propriedades dos monoides, como a identidade (mempty) e a operação de combinação (mappend), para lidar com a combinação de produtos de forma mais concisa e elegante. Isso nos permite evitar a lógica manual de lidar com diferentes casos de Nada e a soma dos valores dos produtos, tornando o código mais simples e legível

 -}

{- 5.2) Crie uma função totalGeral que recebe uma lista de produtos e retorna o preço total deles usando o monoide anterior -}

totalGeral :: [Produto] -> Double
totalGeral = valor . mconcat

{- 5.3) A função min no Haskell retorna o menor entre dois números, por exemplo, min 4 5 = 4 .
     Crie um tipo Min com um campo inteiro, que seja instância de Ord , Eq e Show (deriving). 
     Crie uma instância de Monoid para Min ( maxBound representa o maior inteiro existente no Haskell). 
     Quanto vale a expressão Min (-32) <> Min (-34) <> Min (-33) ?
     Explique sua escolha para o mempty . -}

data Min = Min Int deriving (Eq, Show)

instance Ord Min where
    compare (Min x) (Min y) = compare x y

instance Monoid Min where
    mempty = Min maxBound
    mappend (Min x) (Min y) = Min (min x y)

{- 
Quanto vale a expressão Min (-32) <> Min (-34) <> Min (-33) ?
(-34)

Explique sua escolha para o mempty
Essa escolha para mempty garante que, ao combinar com qualquer outro valor Min, o resultado sempre será esse outro valor, já que é o menor entre eles.
 -}


{- 5.4) Crie uma função minAll que recebe um [Min] e retorna um Min contendo o menor valor. -}

minAll :: [Min] -> Min
minAll [] = error "Lista vazia"
minAll xs = Min (minimum [x | Min x <- xs])

{- 
5.5) Crie o tipo Paridade com os values constructors Par e Impar . Crie o typeclass ParImpar que contém a função decide :: a -> Paridade e possui as instâncias: 
    Para Int : noção de Par/Impar de Int . 
    Para [a] : uma lista de elementos qualquer é Par se o número de elementos o for. 
    Bool : False como Par , True como Impar .

 -}

data Paridade = Par | Impar deriving (Show)

class ParImpar a where
    decide :: a -> Paridade

instance ParImpar Int where
    decide n
        | even n    = Par
        | otherwise = Impar

instance ParImpar [a] where
    decide xs
        | even (length xs) = Par
        | otherwise        = Impar

instance ParImpar Bool where
    decide False = Par
    decide True  = Impar


{- 
5.7) Usando a estrutura de árvore, monte uma função mapa , que jogue uma função passada por parâmetro para todos os elementos de uma árvore. Deixe explícito o tipo desta função.

 -}

data Arvore a = No a (Arvore a) (Arvore a) | Folha deriving (Show)

mapa :: (a -> b) -> Arvore a -> Arvore b
mapa f Folha = Folha
mapa f (No x esq dir) = No (f x) (mapa f esq) (mapa f dir)

{- 
5.8) Usando o exercício anterior, some 5 a cada elemento de uma árvore de inteiros.
 -}

arvoreSoma :: Arvore Int
arvoreSoma = No 1 (No 2 Folha Folha) (No 3 Folha Folha)

arvoreResultado :: Arvore Int
arvoreResultado = soma5 arvoreSoma



 {- 5.11) Implemente os percursos pós-ordem e pré-ordem. Via comentário, faça os "testes de mesa" para os dois percursos da árvore Raiz 15 (Raiz 11 (Folha 6) (Raiz 12 (Folha 10) Nula)) (Raiz 20 Nula (Raiz 22 (Folha 21) Nula)) -}


data Arvore a = No a (Arvore a) (Arvore a) | Folha | Nula deriving (Show)


preOrdem :: Arvore a -> [a]
preOrdem Folha = []
preOrdem (No x esq dir) = [x] ++ preOrdem esq ++ preOrdem dir

posOrdem :: Arvore a -> [a]
posOrdem Folha = []
posOrdem (No x esq dir) = posOrdem esq ++ posOrdem dir ++ [x]

{- 

Árvore:
           15
         /    \
        11    20
       /  \     \
      6   12    22
          /      \
         10       21

Percurso pré-ordem: 15 -> 11 -> 6 -> 12 -> 10 -> 20 -> 22 -> 21
Percurso pós-ordem: 6 -> 10 -> 12 -> 11 -> 21 -> 22 -> 20 -> 15


 -}



