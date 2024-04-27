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


