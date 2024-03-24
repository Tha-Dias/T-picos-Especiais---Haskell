{- Atividades - Tópico Especiais -}

somar :: Int -> Int -> Int
somar x y = y + x

{- 2.1) Gera as listas: -}

{- a)[1,11,121,1331,14641,161051,1771561]  -}

listaA :: [Int] 
listaA = iterate (*11) 1

limitando7 :: [Int]
limitando7 = take 7 listaA

{- b) [1,2,3,5,6,7,9,10,11,13,14,15,17,18,19,21,22,23, 25,26,27,29,30,31,33,34,35,37,38,39] -}

listaB :: [Int]
listaB = [1..39]

{-c)["AaBB", "AbBB", "AcBB", "AdBB", "AeBB", "AfBB", "AgBB"] -}

listaC :: [String]
listaC = ["AA" ++ [letra] ++ "BB" | letra <- ['a'..'g']]


{-d)[5,8,11,17,20,26,29,32,38,41] -}

listaD :: [Int]
listaD = [5, 8..41]

{-e)[1.0,0.5,0.25,0.125,0.0625,0.03125] -}

listaE :: [Double]
listaE = take 6 (iterate (/2) 1.0)

{-f)[1,10,19,28,37,46,55,64] -}

listaF :: [Int]
listaF = [1,10..64]

{-g)[2,4,8,10,12,16,18,22,24,28,30] -}

listaG :: [Int]
listaG = [x | x <- [2, 4..30], x `notElem` [20]] {- `notElem`retira i=o valor dentro dos colchetes da lista -}

{-h)['@','A','C','xxD','E','G','J','L']-} 

listaH :: [String]
listaH = ["@", "A", "C", "xxD", "E", "G", "J", "L"]

