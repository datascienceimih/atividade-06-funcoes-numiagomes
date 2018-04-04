PROJETO INTEGRADOR - Exercício 06
================
Prof. Neylson Crepalde
2018, Abril, 9 - 13

Olá. Este é o sexto exercício de nosso **Projeto Integrador**, o último relacionado a treinamento R. Neste exercício iremos praticar a programação de funções e sua mobilização de forma automatizada com a família de funções `apply`. Pau na máquina!

1.  Escreva uma função que calcule a média, a mediana, a variância e o desvio padrão de um vetor numérico. A função deve retornar os resultados numa matriz. Teste a sua função com uma variável do banco de dados `iris`.

2.  Reescreva sua função de modo que ela seja capaz de processar vetores com dados completos e vetores com *missing values*. Deve haver uma opção para o usuário marcar se ele quer a remoção de missings ou não. Teste a sua função com uma variável numérica do banco de dados `enade`. Mostre os resultados COM REMOÇÃO de *missings* e SEM REMOÇÃO.

3.  Reescreva sua função de modo que ela aceite apenas vetores do typo `integer` ou `numeric`. Se o usuário tentar passar um vetor de outro tipo, a função deve exibir um ERRO e uma mensagem com a orientação: "Object is not integer or numeric". *Dica*: use o comando `stop("mensagem")`. Teste a função com uma variável numérica, uma variável integer e um objeto de outro tipo qualquer.

4.  Agora, use a função `sapply` para executar sua função para todas as variáveis numéricas do banco de dados `iris`. Execute-a também para as variáveis *idade*, *nota geral*, *nota da formação geral* e *nota do componente específico* do banco de dados `enade`.

5.  Sabemos que as variáveis do questionário do estudante do ENADE relacionadas à IES possuem duas categorias que não são úteis para nossa análise, 7 e 8 (Não sei, não se aplica). Essas categorias precisam ser substituídas por `NA's`. Faça a substituição usando uma função programada por você e a função sapply.

Desafio
-------

6.  Reescreva a sua função de estatísticas descritivas. Não utilize as funções prontas do `R` mas implemente a média, a variância e o desvio padrão (mediana não) a partir de suas fórmulas.

Divirta-se!
===========
