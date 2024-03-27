
{- 3.2) Faça o tipo Temperatura que pode ter valores Celsius , Farenheit ou Kelvin . Implemente as funções: 
converterCelsius : recebe um valor double e uma temperatura, e faz a conversão para Celsius. 
converterKelvin : recebe um valor double e uma e faz a conversão para Kelvin.
converterFarenheit : recebe um valor double e uma temperatura, e faz a conversão para Farenheit.  -}

data Temperatura = Celsius Double| Fahrenheit Double| Kelvin Double deriving (Show)

converterCelsius :: Double -> Temperatura -> Double
converterCelsius valor (Celsius temperatura) = temperatura
converterCelsius valor (Fahrenheit temperatura) = (temperatura - 32) * 5 / 9
converterCelsius valor (Kelvin temperatura) = temperatura - 273.15

converterKelvin :: Double -> Temperatura -> Double
converterKelvin valor (Celsius temperatura) = temperatura + 273.15
converterKelvin valor (Fahrenheit temperatura) = (temperatura + 459.67) * 5 / 9
converterKelvin valor (Kelvin temperatura) = temperatura

converterFahrenheit :: Double -> Temperatura -> Double
converterFahrenheit valor (Celsius temperatura) = temperatura * 9 / 5 + 32
converterFahrenheit valor (Fahrenheit temperatura) = temperatura
converterFahrenheit valor (Kelvin temperatura) = temperatura * 9 / 5 - 459.67

{- ------------------------------------------------------------------------------------ -}
{- 3.3) Implemente uma função que simule o vencedor de uma partida de pedra,
papel e tesoura usando tipos criados. Casos de empate devem ser considerados em seu tipo.  -}


data Escolha = Pedra | Papel | Tesoura deriving (Show, Eq)

data Resultado = Vitoria | Derrota | Empate deriving (Show, Eq)

vencedorPedraPapelTesoura :: Escolha -> Escolha -> Resultado
vencedorPedraPapelTesoura jogador1 jogador2| jogador1 == jogador2 = Empate| jogador1 == Pedra && jogador2 == Tesoura ||jogador1 == Papel && jogador2 == Pedra ||jogador1 == Tesoura && jogador2 == Papel = Vitoria
    | otherwise = Derrota


{- -------------------------------------------------------------------------------- -}

{- 3.4) Faça uma função que retorne uma string, com todas as vogais maiúsculas e minúsculas 
eliminadas de uma string passada por parâmetro usando list compreenshion -}

{- --------------------------------------------------------------------------------- -}
{- 3.7) Faça uma função que receba uma String e retorne True se esta for um palíndromo; caso contrário, False .  -}

recebeString :: String -> Bool
recebeString palavra1 = palavra1 == reverse palavra1

{- ----------------------------------------------------------------------------------- -}
{- 3.7 -}
{- 3.8 -}
{- 3.10) Faça uma função chamada revNum , que receba uma String s e um Int n . 
Esta deverá retornar as n primeiras letras em ordem reversa e o restante em sua ordem normal. Exemplo: revNum 4 "FATEC" = "ETAFC"  -}

revNum :: String -> Int -> String
revNum s n = reverse (take n s) ++ drop n s