{- 2.1)	 Crie	 uma	 função	 que	 verifique	 se	 o	 tamanho	 de	 uma
String	é	par	ou	não.	Use		Bool		como	retorno. -}

tamanhoPar :: String -> Bool
tamanhoPar str = even (length str)


{- 2.3)	 Escreva	 uma	 função	 que	 receba	 um	 vetor	 de	 Strings	 e
retorne	uma	lista	com	todos	os	elementos	em	ordem	reversa. -}

listaReversa:: [String] -> [String]
listaReversa = reverse

{- 2.4)	 Escreva	 uma	 função	 que	 receba	 um	 vetor	 de	 Strings	 e
retorne	 uma	lista	 com	 o	 tamanho	 de	 cada	 String.	As	palavras	 de
tamanho	par	devem	ser	excluídas	da	resposta. -}

removerPar:: [String] -> [Int]
removerPar = map length . filter (odd . length)

{- 2.7)	 Faça	 uma	 função	 que	 receba	 um	 inteiro	 e	 retorne	 uma
tupla,	contendo:	o	dobro	deste	número	na	primeira	coordenada,	o
triplo	na	segunda,	o	quádruplo	na	terceira	e	o	quíntuplo	na	quarta. -}


tuplaN:: Int -> (Int, Int, Int, Int)
tuplaN x= (x*2, x*3, x*4, x*5) 

