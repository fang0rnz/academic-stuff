-- trabalho Haskell da dupla: João Pedro Brito Fernandes e Lucas Sartori Moraes - Eng Comp


--os arquivos de teste, sao, respectivamente:
--palavras, cpalavras
-- palavras1, cpalavras1
--palavras2, cpalavras2
--listas de custos para teste: lcustos e lcustos1


cpalavras = ['F','L','O','R','E','S','T','A','R','S',
			'A','T','S','E','R','O','L','F','U','V',
			'C','A','P','E','L','A','P','A','S','C',
			'D','T','C','A','D','W','Y','E','R','T',
			'F','D','C','C','S','A','L','U','S','E',
			'Q','L','K','C','A','U','E','A','I','O',
			'A','A','S','A','R','P','R','D','K','A',
			'N','E','N','E','F','T','E','B','A','U',
			'J','K','D','D','S','H','W','L','F','L',
			'I','O','T','U','G','J','J','V','A','A']

cpalavras2 = ['F','L','O','R','E','S','T','A','R','S',
			'K','J','G','D','F','T','J','J','U','V',
			'C','A','P','E','L','A','A','I','S','C',
			'D','T','C','A','D','W','Y','E','R','T',
			'F','D','C','C','S','A','L','U','S','E',
			'Q','L','K','C','A','U','E','A','I','O',
			'A','A','S','A','R','P','R','D','K','A',
			'N','E','N','E','F','T','E','B','A','U',
			'J','K','D','D','S','H','W','L','F','L',
			'I','O','T','U','G','J','J','V','A','A']

cpalavras1= ['S', 'A', 'B', 'O', 'N', 'E', 'T', 'E', 'J', 'P',
			'A', 'M', 'H', 'G', 'J', 'L', 'P', 'J', 'A', 'I',
			'L', 'I', 'M', 'A', 'O', 'L', 'V', 'W', 'C', 'C',
			'S', 'A', 'B', 'O', 'N', 'E', 'T', 'E', 'A', 'L',
			'I', 'Q', 'F', 'C', 'T', 'M', 'U', 'H', 'T', 'E',
			'C', 'W', 'Z', 'S', 'E', 'A', 'J', 'A', 'C', 'S',
			'H', 'G', 'D', 'N', 'X', 'C', 'Q', 'M', 'M', 'L',
			'A', 'R', 'U', 'I', 'N', 'U', 'S', 'A', 'C', 'O',
			'K', 'F', 'P', 'O', 'W', 'J', 'A', 'C', 'A', 'D',
			'M', 'A', 'N', 'T', 'E', 'I', 'G', 'A', 'J', 'D']

d::Int
d = 10



lcustos = [('A',1),('B',10),('C',18),('D',38),('E',70),('F',10),('G',43),('H',15),('I',54),('J',10),
			('K',44),('L',81),('M',19),('N',39),('O',54),('P',91),('Q',72),('R',30),('S',84),('T',75),('U',22),
			('V',47),('X',66),('Y',59),('Z',32)]

lcustos1 = [('A',1),('B',2),('C',3),('D',4),('E',5),('F',6),('G',7),('H',8),('I',9),('J',10),
			('K',11),('L',12),('M',13),('N',14),('O',15),('P',16),('Q',17),('R',18),('S',19),('T',20),('U',21),
			('V',22),('X',23),('Y',24),('Z',25)]


palavras = ["AULA", "ALEGRIA", "CAPELA", "FLORESTA", "LAPIS"]		

palavras1 = ["SABONETE", "SALSICHA", "MANTEIGA", "LIMAO", "PICLES", "JACA", "CAJA"]

palavras2 = ["AULA", "ALEGRIA", "CAPELA", "FLORESTA", "LAPIS", "CAU"]




lcustos1::[(Char, Int)]

lcustos::[(Char, Int)] 			-- Define a lista lcustos como uma lista de tuplas onde o 1º elemento da tupla é um "Char" e o 2º é um "Int" pra não dar erro de tipo nas funções abaixo

------------------------------ Funções genéricas para todas as questões ------------------------------ 
matrizlinha xs x = if null xs			-- separa o caça-palavras em listas de tamanho n (que seria a dimensão do caça palavras) na forma de linhas
					then []				-- Recebe de entrada a matriz do caça-palavras e o tamanho de cada linha do mesmo
					else [take x xs] ++ matrizlinha (drop x xs) x
					
matrizcoluna xs x = if (length xs ) == (x^2-x)				-- separa o caça-palavras em listas de tamanho n (que seria a dimensão do caça palavras) na forma de colunas
					then []								-- Recebe de entrada a matriz do caça-palavras e o tamanho de cada coluna do mesmo
					else [[ xs!!y | y<-[x*0,x*1..(x*(x-1))]]] ++ matrizcoluna (tail xs) x

funcao12 xs ys = if null ys -- Verifica se uma string pertence à uma lista de strings. E me retorna uma lista concatenada da string pela quantidade de vezes que ela foi encontrada.
					then []	-- Ex.: string: "AULA" encontrada 3 vezes. Output: "AULAAULAAULA"
					else if (sublista xs (head ys))
							then [(xs)] ++ (funcao12 xs (tail ys)) --antes tava apenas xs, e nao [xs]
							else funcao12 xs (tail ys)             --entao concatenava tudo

funcao123 zs ys = if null zs || null ys -- Utiliza a funcao12 recursivamente em uma lista de strings.
					then []				
					else funcao12 (head zs) ys ++ funcao123 (tail zs) ys

palavrasexist xs ys n = funcao123 xs w ++ funcao123 xs v ++ funcao123 xs (revertestrin w) ++ funcao123 xs (revertestrin v) 
					where				-- A função palavrasexist recebe de entrada a lista de palavras, o caça-palavras e o tamanho de cada linha/coluna
					w = matrizlinha ys n	-- e analisa quais palavras são encontradas no caça-palavras (esquerda>direita, cima>baixo e vice-versa)
					v = matrizcoluna ys n
-- A função palavrasexist recebe de entrada a lista de palavras, o caça-palavras e o tamanho de cada linha/coluna 
-- e analisa quais palavras são encontradas no caça-palavras (esquerda>direita, cima>baixo e vice-versa)

prefixo xs ys = xs == take n ys				-- Verifica se os elementos de uma lista é o prefixo de uma outra lista. (ex.: "FLOR" "FLOR~ESTA"
					where 					-- Entrada xs ys são listas.
					n = length xs
					
sublista xs ys = if null ys 				-- Verifica se os elementos de uma lista - de modo ordenado - pertence à outra lista. (ex.: "gato" "O gato esta aqui")
					then False				-- Admite como entrada em: xs e ys como listas 
						else prefixo xs ys || sublista xs (tail ys)
						
revertestrin xs = [reverse x | x<-xs] 		-- devolve a lista de strings na ordem inversa de cada uma. (ex.: [AULA, BACANA] -> [ALUA, ANACAB]

------------------------------ 1ª QUESTÃO ------------------------------
ocorre x xs = length [ y | y <- xs, x == y] -- x é a 'letra' a ser analisada e xs é a "palavra" onde a letra aparece

custopal1 xs ys = if null xs				-- xs é a lista de custos (lcustos)  ys é a "string" da palavra
					then 0
					else (ocorre (fst (head xs)) ys)*(snd (head xs)) + custopal1 (tail xs) ys
					
funcaocusto xs ys = [custopal1 xs y | y <- ys]		-- xs é a lista de custos (lcustos) e ys é a lista de palavras (palavra).
											--Retorna a lista de custos.
											
analise1 xs ys = zip ys x  					-- ys é a lista de palavras e xs é a lista de custos
				where
				x = funcaocusto xs ys 			--x é a lista de custos de cada palavra. 
											-- analise1 retorna uma lista de tuplas onde o 1º elemento de cada tupla é a PALAVRA e o segundo, seu CUSTO.
											
palavramenorc xs ys  = [ x | x <-analise1 xs ys, (snd x) == minimum p] -- A função palavramenorc recebe xs: lista de custos; ys: lista de palavras
				where						-- analisa as tuplas de (palavra, custo) e retorna com a lista de menor custo.
				p = funcaocusto xs ys			--  p é a lista de custos (ex.: [1, 10, 105, 1231])

noctupla x xs = length [x | z<-xs, x==z] --essa funcao tem como saida a quantidade de vezes que uma tupla (x,y) aparece em uma lista de tuplas

ocorrencianaorepetidatupla xs = if null xs --essa funcao tem como entrada uma lista de tuplas qualquer de strings e como saida uma lista que elimina a repeticao de tuplas
							then []
							else if  (noctupla (head xs) xs == 1)
								then [(head xs)] ++  ocorrencianaorepetidatupla (tail xs)
							else
								ocorrencianaorepetidatupla (tail xs)
				
ocorrenciamc xs ys n zs = if null (palavramenorc zs (palavrasexist xs ys n))
			  then error "Nenhuma palavra encontrada na matriz!"
			  else ocorrencianaorepetidatupla(palavramenorc zs (palavrasexist xs ys n)) -- Colocamos o head na funcao para, caso ocorra a palavra de menor custo mais de uma vez, só teremos de saída, uma tupla.


------------------------------ 2ªQUESTÃO ------------------------------
numocorrencia x zs ys n = length [ y | y <- xs, x==y]	-- a função recebe em x a palavra a ser analisada (entre aspas) e a lista de palavras que estão sendo procuradas no caça-palavras e a dimensão
					where 							-- e retorna quantas vezes essa palavra aparece no caça-palavras
					xs = palavrasexist ys zs n
					
tuplaocorr x xs ys n =if z == 0 then   --retorna o número de ocorrência de cada palavra na matriz, se o número de ocorrências for igual a 0, retorna lista vazia
					 []
					else [(x,z)] --[(x, z)]
					where 
					z = numocorrencia x ys xs n
					
listatuplaocorr xs ys n = concat [tuplaocorr x xs ys n| x<-xs] -- A função recebe de entrada a lista de palavra em xs, o caça-palavras e a dimensão do caça-palavras e retorna uma lista de duplas (PALAVRA, nº de ocorrencias)

nocorrencias xs ys n = (tuplaordemalfabetica)  -- concat é usado para concatenar as listas vazias criadas pelas palavras que ocorrem 0 vezes com as que não ocorrem, evitando assim uma lista com listas vazias em seu meio.
						where tuplaordemalfabetica =  (quicksort (listatuplaocorr xs ys n))
						
quicksort :: Ord a => [a] -> [a]		-- Quicksort. [?] --
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater) --usado para deixar a lista de palavras encontradas em ordem alfabética
				where
				lesser  = filter (< p) xs
				greater = filter (>= p) xs

				
------------------------------ 3ª QUESTÃO ------------------------------
noc x xs = length [x | z<-xs, sublista x z] -- essa funcao tem como saida a quantidade de vezes que uma string x aparece em uma lista de strings xs

ocorrencianaorepetida xs = if null xs -- essa funcao tem como entrada uma lista qualquer de strings e como saida uma lista que elimina a repeticao de strings
							then []		-- ex.: ocorrencianaorepetida ["AULA", "AULA", "FLORESTA"] >> ["AULA", "FLORESTA"]
							else if  (noc (head xs) xs == 1)
								then [(head xs)] ++  ocorrencianaorepetida (tail xs)
							else
								ocorrencianaorepetida (tail xs)

tuplacusto xs zs n ys = zip x a   	-- Inputs: xs: lista de palavras; zs: caça-palavras; n: dimensão; ys: lista de custos
					where  			-- cria uma lista de tuplas com a palavra e seu respectivo custo
					a = ocorrencianaorepetida (palavrasexist xs zs n)
					x = funcaocusto ys (ocorrencianaorepetida (palavrasexist xs zs n))

palavrasmaiscaras1 xs zs n ys = reverse (quicksort custpal)     -- inputs: xs: lista de palavras; zs: caça-palavras; n: dimensão; ys: lista de custos
								where 	--o quicksort ordena em ordem crescente, queremos do mais caro pro menos caro, então utilizo a função reverse
								custpal = tuplacusto xs zs n ys

invertTup list = [(x,y) | (y,x)<- list] -- Inverte todas as tuplas de uma lista de tuplas

palavrasmaiscaras xs zs n ys = invertTup (palavrasmaiscaras1 xs zs n ys) -- A função final da 3ª questão. Retorna uma lista de tuplas (PALAVRA, custo) ordenada da palavra mais cara pra menos cara.


------------------------------ 4ª QUESTÃO ------------------------------
matrizposicoes xs ys n = if null (ordeminput) -- xs: uma palavra; ys: o caça-palavras; n: dimensão
							then [] 		  -- A função matrizposicoes retorna as linhas em que a palavra a ser analisada está escrita. 
						else if sublista xs (head(ordeminput)) || sublista xs (reverse (head(ordeminput))) -- Tanto em ordem normal quanto inversa.
							then [head(ordeminput)] ++ (matrizposicoes xs (drop 10 ys) n) 
						else (matrizposicoes xs (drop 10 ys) n) 
						where ordeminput = matrizlinha ys n 

listalinhas xs ys n = concat ([matrizposicoes x ys n | x <- xs]) -- inputs: xs: lista de palavras; ys: caça-palavras; n: dimensão
-- listalinhas utiliza matrizposicoes com uma lista de palavras e não com uma só palavra.
						
matrizposicoesc xs ys n = if null (ordeminput) -- xs: uma palavra; ys: o caça-palavras; n: dimensão
								then []		   -- A função matrizposicoes retorna as linhas em que a palavra a ser analisada está escrita. 
						   else if sublista xs (head(ordeminput)) || sublista xs (reverse(head(ordeminput))) -- Tanto em ordem normal quanto inversa.
								then [head(ordeminput)] ++ (matrizposicoesc xs (tail ys) n)
							else (matrizposicoesc xs (tail ys) n)
							where ordeminput = matrizcoluna ys n

listacol xs ys n = concat ([matrizposicoesc x ys n | x<- xs]) -- inputs: xs: lista de palavras; ys: caça-palavras; n: dimensão
-- listacol faz o mesmo que listalinhas, porém utiliza matrizposicoesc. Ou seja, é baseada nas COLUNAS.

verificarac x xs = if  null xs	 -- inputs: x: a palavra ; xs: a string maior que PODE OU NÃO conter a palavra (x)
						then  0:[]	 -- Verifica a posição da 1ª letra de uma palavra em uma outra string SE a palavra existir nessa string.	
					else if not (sublista x xs)  --Retorna uma lista de UM's para cada letra que vem ANTES da 1ª letra da palavra a ser analisada seguido de um ZERO quando a palavra aparecer
							then [] 			-- ex.: verificarac "AULA" "OUTRAAULA" >> [1, 1, 1, 1, 1, 0]	
					else if prefixo x xs 		-- essa função será utilizada na próxima função.
							then 0:[]
					else 1:[] ++ verificarac x (tail xs)

verificaracrev x xs = if  null xs	-- inputs: x: a palavra; xs: a string maior que pode OU NÃO conter a palavra (x)
					then  0:[]		-- Essa função faz o mesmo que - verificarac - só que com as strings ao contrário ("FLORESTAUV" = "VUASEROLF")	
					else if not (sublista x (reverse xs))
							then [] 	
					else if (prefixo x (reverse xs))
						then 0:[]
					else 1:[] ++ verificarac x (tail (reverse xs))

orientacaopalavra x zs xs ys n = if sublista x (reverse zs) && elem zs (listacol xs ys n) -- x: "PALAVRA", zs: "A string sendo examinada", xs: lista de palavras, ys: caça-palavras, n: dimensão
									then "Vertical-base-topo"								--Verifica a orientação de uma palavra na lista de palavras
								 else if sublista x zs && elem zs (listacol xs ys n)
									then "Vertical-topo-base"
								 else if sublista x (reverse zs) && elem zs (listalinhas xs ys n)
									then "Horizontal-direita-esquerda"
								 else  "Horizontal-esquerda-direita"


verificaracint x zs xs ys n = if verificarac x zs == []	&& verificaracrev x zs == [] -- inputs: x: a palavra ; zs: a string maior que PODE OU NÃO conter a palavra (x)
							then []					 		-- Soma a lista que a função - verificarac x zs - retorna.
						else if sublista  x (reverse zs)
							then [((length (zs) - 1) - sum (verificaracrev x zs), orientacao)] --base topo, direita esquerda
						else [(sum (verificarac x zs), orientacao)]  --topo base, esquerda direita 	-- ex.: verificaracint "AULA" "OUTRAAULA" >> [5]  --Obs.: a 1ª letra da string tem como contagem 0.
						where 
							orientacao = orientacaopalavra x zs xs ys n



verificaracrec x xs ys n = if null stringdavez || null x	--inputs: x: palavra, xs: lista de palavras, ys: caça-palavras, n: dimensão.
							then [] 						-- Utiliza a função verificaracint recursivamente analisando uma palavra em uma lista de palavras.
						else 
							(verificaracint (x) (head stringdavez) xs ys n) ++ (verificaracrec x (tail stringdavez)  ys n)	  
						where 														  -- ex.: verificaracrec "FLORESTA" palavra cpalavras 10 >
							stringdavez = (listalinhas xs ys n) ++ (listacol xs ys n) -- >> [(0, "Horizontal-esquerda-direita"), (7, "Horizontal-direita-esquerda")]
														
verrr x xs ys n = (x, y)		-- inputs: x: palavra; xs: lista de strings
			where y = verificaracrec x xs ys n -- Retorna uma tupla com (PALAVRA, [posição em cada string que a palavra é --contrada])

verificador xs ys n = if null xs || null ys -- inputs: xs = lista de palavras, ys = caça-palavras e n = dimensão
						then [] 			-- Essa função adapta a função verrr para, no lugar de UMA palavra, analisar TODAS as palavras de uma lista de palavras.
					else [verrr (head xs) xs ys n] ++ verificador (tail xs) ys n

dadospalavra xs ys n = [ x | x <- (verificador xs ys n), snd x /= []]	--A função verificador também analisa palavras que não são encontradas. Terminamos a função adaptando ela para retirar essas palavras.
-- As palavras que não são encontradas devolvem uma tupla com o elemento "[]". Logo, retiramos todas as tuplas que o 2º elemento é esse. Assim terminamos a 4ª questão.


------------------------------ 5ª QUESTÃO ------------------------------
direcao xs ys n = if null xs || null ys 		-- A função recebe a lista de palavras, o caça-palavras e a dimensão do caça-palavras
					then error "0"		-- analisa se alguma das lista é nula, quando não for, verifica quantas palavras estão nas linhas e quantas estão nas colunas (OLHAR >WHERE<)
					else if (length (linhas)) > (length (colunas))	-- Se o número de palavas nas linhas for maior que o de palavras da vertical
						then ("Horizontal", length (linhas))	-- ele dá uma saída com "Horizontal" (linhas são horizontais!!) e o número de palavras que tem nas linhas
					else if (length (linhas) ) < (length (colunas))	-- Caso o contrário ocorra, 
						then ("Vertical", length (colunas))		-- retorna "Vertical" (afinal colunas são......) e o número de palavras nas colunas.
					else ("Ambas as direcoes", length (linhas)) -- Se o número de ocorrencias for igual pra ambos, retorna que são as duas direções e o número de palavras.
					--where
					--linhas = funcao123 xs (matrizlinha ys n) 					-- retorna a lista de palavras que se encontram nas linhas do caça-palavras
					--colunas = funcao123 xs (matrizcoluna ys n) 					-- retorna a lista de palavras que se encontram nas colunas do caça-palavras
					--linhasrev = funcao123 xs (revertestrin (matrizlinha ys n))  -- retorna a lista de palavras que se encontram nas linhas INVERTIDAS do caça-palavras
					--colunasrev = funcao123 xs (revertestrin (matrizcoluna ys n))-- retorna a lista de palavras que se encontram nas colunas INVERTIDAS do caça-palavras
					where
					linhas = listalinhas xs ys n 					
					colunas = listacol xs ys n 					

					
------------------------------ 6ª QUESTÃO ------------------------------ 
funcao12o xs ys = if null ys -- Verifica se uma string pertence à uma lista de strings. E me retorna uma lista concatenada da string pela quantidade de vezes que ela foi encontrada.
					then []	-- Ex.: string: "AULA" encontrada 3 vezes. Output: "AULAAULAAULA"
					else if (sublista xs (head ys))
							then [(xs)] ++ [", "] ++(funcao12o xs (tail ys)) --antes tava apenas xs, e nao [xs]
							else funcao12o xs (tail ys)             --entao concatenava tudo

funcao123o zs ys = if null zs || null ys -- Utiliza a funcao12 recursivamente em uma lista de strings.
					then []				
					else funcao12o (head zs) ys ++ funcao123o (tail zs) ys

palavrasexisto xs ys n = funcao123o xs w ++ funcao123o xs v ++ funcao123o xs (revertestrin w) ++ funcao123o xs (revertestrin v) 
						where
							w = matrizlinha ys n
							v = matrizcoluna ys n

matrizlinha1 xs x = if null xs
					then []
					else [take x xs] ++ ["\n"] ++ matrizlinha1 (drop x xs) x

adespaco1 xs = if null xs
						then []
			  	else (head xs) ++ " " ++ adespaco1 (tail(xs))
adespaco xs = init (adespaco1 xs) 

saida = do
		putStr("Bem vindo ao modulo de saida do caca palavra! Atencao: insira seu caca palavras, sua dimensao e sua lista de palavras, respectivamente, nas variaveis cpalavras1, d e palavras1, no inicio do script trab-EC-joaopedro-lucassartori.hs!\nDigite enter para continuar.")
		whatever <-getLine
		putStr ("Palavras a serem examinadas: " ++ adespaco1 palavras1 ++ "\n")
		putStr ("Matriz a ser examinada de dimensao " ++ show d ++ ":" ++ "\n\n" ++ concat (matrizlinha1 cpalavras1 d) ++ "\n--Aguarde uns instantes!--\n")
		writeFile "saida.txt" ("Conjunto de palavras: " ++ adespaco1 palavras1 ++ "\n\n" ++ "Matriz n = " ++ show d ++ "\n\n" ++ concat(matrizlinha1 cpalavras1 d) ++ "\n\nQuesito 1 (ocorrenciamc): " ++ show (ocorrenciamc palavras1 cpalavras1 d lcustos) ++ "\n\nQuesito 2 (nocorrencias): " ++ show (nocorrencias palavras1 cpalavras1 d) ++ "\n\nQuesito 3 (palavrasmaiscaras): " ++show (palavrasmaiscaras palavras1 cpalavras1 d lcustos) ++ "\n\nQuesito 4 (dadospalavra): " ++ show (dadospalavra palavras1 cpalavras1 d) ++ "\n\nQuesito 5 (direcao): " ++ show (direcao palavras1 cpalavras1 d) ++ "\n\n" ++ porquito1)
		--writeFile "teste.txt" ((concat (init(palavrasexisto (palavras1) cpalavras1 d))) ++ "\n" ++ (concat (matrizlinha1 cpalavras1 d)))
		putStr ("--Arquivo de saida gerado com sucesso! Se encontra na mesma pasta desse script com nome saida.txt.--") 


--concat (init(palavrasexisto palavras1 cpalavras1 10))
			
-- FIM!
-- >When I wrote this, only God and I understood what I was doing
-- >Now, God only knows
porquito1 = "O PORCO DA SALVAÇÃO CHEGOU\n                          _\n  _._ _..._ .-',     _.._(`))\n '-. `     '  (-._.-'    ',)\n    )         )            '.\n   ( _    _    |             )\n  |  a    a    )              |\n  (   .-.                     ;  \n   '-('' ).-'       ,'       ;\n      '-;           |      .'\n         )           (    )\n         | 7  .__  _.-(   )\n         | |  |  ``(  (`  )\n        (,_|  |   (,_(   )\n           (,_(      '`-'\n"

porquito = putStr "O PORCO DA SALVAÇÃO CHEGOU\n                          _\n  _._ _..._ .-',     _.._(`))\n '-. `     '  (-._.-'    ',)\n    )         )            '.\n   ( _    _    |             )\n  |  a    a    )              |\n  (   .-.                     ;  \n   '-('' ).-'       ,'       ;\n      '-;           |      .'\n         )           (    )\n         | 7  .__  _.-(   )\n         | |  |  ``(  (`  )\n        (,_|  |   (,_(   )\n           (,_(      '`-'\n"
