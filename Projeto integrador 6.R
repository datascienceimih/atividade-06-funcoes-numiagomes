##########################################################
######                Prog. O.O.                   #######
#####             Ciência de Dados                 #######
##########################################################


###Funções
rm(list=ls(all=T))
if (!require(readr)) install.packages('readr');library(readr)
if (!require(dplyr)) install.packages('dplyr');library(dplyr)


# 1 - Escreva uma função que calcule a média, a mediana, a variância e o desvio padrão de um vetor
# numérico. A função deve retornar os resultados numa matriz. Teste a sua função com uma variável
# do banco de dados iris.

descritivas <- function(x){
  media = mean(x)
  mediana = median(x)
  variancia = var(x)
  d.padrao = sd (x)
  
  matri <- matrix (c( media, mediana, variancia, d.padrao),nrow = 1)
  colnames(matri) <- c("Média","Mediana", "Variância","Desvio Padrão")
  
  return(matri)
}

#Teste
descritivas(iris$Sepal.Length)

# 2 - Reescreva sua função de modo que ela seja capaz de processar vetores com dados completos e vetores com 
# missing values. Deve haver uma opção para o usuário marcar se ele quer a remoção de missings ou não.
# Teste a sua função com uma variável numérica do banco de dados enade. Mostre os resultados COM REMOÇÃO 
# de missings e SEM REMOÇÃO.

enade14 <- read_csv2("https://raw.githubusercontent.com/neylsoncrepalde/introducao_ao_r/master/dados/enade_2014_amostra.csv")

View(enade14)

descritivas2 <- function(x,remove.na = F){

  if(remove.na ==T){
    x = x[!is.na(x)]
  }
  media = mean(x)
  mediana = median(x)
  variancia = var(x)
  d.padrao = sd (x)
  
  matri <- matrix (c( media, mediana, variancia, d.padrao),nrow = 1)
  colnames(matri) <- c("Média","Mediana", "Variância","Desvio Padrão")
  
  return(matri)
  }
###########Teste 
descritivas2(enade14$nu_idade, remove.na = T)
descritivas2(enade14$nu_idade)

# 3 - Reescreva sua função de modo que ela aceite apenas vetores do typo integer ou numeric. Se o usuário 
# tentar passar um vetor de outro tipo, a função deve exibir um ERRO e uma mensagem com a orientação: 
#   "Object is not integer or numeric". Dica: use o comando stop("mensagem"). Teste a função com uma 
# variável numérica, uma variável integer e um objeto de outro tipo qualquer.

descritivas3 <- function(x,remove.na = F){
  if(class(x)=="integer"| 
     class(x)=="numeric"){
    
    if(remove.na ==T){
      x = x[!is.na(x)]
    }
    media = mean(x)
    mediana = median(x)
    variancia = var(x)
    d.padrao = sd (x)
    
    matri <- matrix (c( media, mediana, variancia, d.padrao),nrow = 1)
    colnames(matri) <- c("Média","Mediana", "Variância","Desvio Padrão")
    
    return(matri)
  }
  else{
    stop('Object is not an integer or a numeric!')
  }
}

###########Teste 
descritivas3(enade14$nu_idade, remove.na = T)
descritivas3(enade14$nu_idade)

descritivas3(enade14$vt_gab_ofg_orig, remove.na = T)

descritivas3(enade14$nt_ger, remove.na = T)
descritivas3(enade14$nt_ger)

# 4 - Agora, use a função sapply para executar sua função para todas as variáveis numéricas do banco de 
# dados iris. Execute-a também para as variáveis idade, nota geral, nota da formação geral e nota do
# componente específico do banco de dados enade.
 
sapply(iris, class)
select1 <- iris %>% select(Sepal.Length,
                           Sepal.Width, 
                           Petal.Length, 
                           Petal.Width)

sapply(select1, descritivas3, remove.na = T)


select2 <- enade14 %>% select(nu_idade,
                                nt_fg, nt_ce,
                                nt_ger)

sapply(select2, descritivas3,remove.na = T)


# 5 - Sabemos que as variáveis do questionário do estudante do ENADE relacionadas à IES possuem duas 
# categorias que não são úteis para nossa análise, 7 e 8 (Não sei, não se aplica). Essas categorias
# precisam ser substituídas por NA's. Faça a substituição usando uma função programada por você e a 
# função sapply.

substituir <- function(x){
  if (is.numeric(x) == T){
    x[x == 7 | x == 8] <- NA
  }
  return(x)
}

#Teste
substituir(enade14$qe_i27)


# 6 - Reescreva a sua função de estatísticas descritivas. Não utilize as funções prontas do R mas implemente
# a média, a variância e o desvio padrão (mediana não) a partir de suas fórmulas.

#### Média
minha_media <- function(x, na.rm = F){
  if(na.rm ==T){
    x = x[!is.na(x)]
  }
  med = sum(x) / length(x)
  return(med)
}

#Teste
minha_media(iris$Sepal.Length)

#### Mediana
#Não consegui... Me ajuda?

#### Variância 
minha_variancia <- function(x, na.rm = F){
  med = sum(x) / length(x)
  desv_qua = (x - med)^2
  vari = sum(desv_qua)/(length(x) - 1) 
  return(vari)
}

#Teste 
minha_variancia(iris$Sepal.Length)

#### Desvio Padrão
meu_desvio <- function(x, na.rm = F){
  med = sum(x) / length(x)
  desv_qua = (x - med)^2
  vari = sum(desv_qua)/(length(x) - 1)  
  dp = vari^(1/2)
  return(dp)
}

#Teste
meu_desvio(iris$Sepal.Length)
