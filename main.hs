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
