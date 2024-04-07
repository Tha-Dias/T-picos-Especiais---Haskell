{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import System.Win32 (xBUTTON1, COORD (yPos))
soma 1 = 1
soma n = soma (n-1) + n


{- Forma prefixa de chamar função -}
maiorq1 :: Int -> Int -> Bool
maiorq1 x y = x > y

{- Forma infixa de chamar função-}
maiorq :: Int -> Int -> Bool
maiorq x y = (>) x y


{- -------------------------------------------------------------------- -}

{- Operação com lista -}

{- Função Head
Head -> Extrai o primeiro elemenro 
    head [1,2,3,4] = 1

last -> Extrai o ultimo elemento
    last [1,2,3,4] = 4

tail -> Exibe a lista sem o primeiro elemento 
    tail [1,2,3,4,5,6] = 2,3,4,5,6

reverse -> Reverte a letra das palavras
    reverse "Thais" = "siahT"

head com reverse
    (head . reverse) "Thais" = 's'
!! -> recebe uma lsita e um numero, que indica a posição da lista.
    (!!) [1,2,3,4,5,6,7] 3 = 4

: -> Essa função recebe um numero e uma lista, e adiciona esse numero an frente da lista
    (:) 2 [4,6,8,10] = [2,4,6,8,10]

length -> recebe uma lista e retorna a quatidade de elementos contida nela
    length [1,2,3] = 3
-}


{- Compreensão de listas -}


{- Tabuada do 2 -}
quadrados :: [Int]
quadrados = [x*2|x <- [1..10]]

{- Cria uma lista com elementos pares de 1 a 25 -}

pares:: [Int]
pares = [x | x <- [1..25], even x]

{- Lista de strings que possui mais 5 caracteres -}

palavrasLongas :: [String]
palavrasLongas = ["Sacol", "Caracol", "Eusoulinda", "MAreEla", "Desistodeviver"]
resultado = [x | x <- palavrasLongas, length x /= 5]

tuplas1 :: Char -> Int -> (Int, String)
tuplas1 x y = (y +10, [x])



-- Exemplo
data DiaSemana = Segunda | Terca | Quarta | Quinta | Sexta| Sabado | Domingo

agenda :: DiaSemana -> String
agenda Domingo = "Descanso"
agenda Sabado = "Festa com a familia"


data Pessoa = Fisica String Int | Juridica String

{-  -}
idade :: Pessoa -> Int
idade (Fisica _ y) = y
