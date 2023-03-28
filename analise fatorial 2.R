# TŽcnica de an‡lise fatorial para os retornos semanais de     #
# cinco ações do mercado financeiro obtidas na bolsa de Nova Yorque.                  #
#######################################################################################   

stock<-read.csv2("http://www.professores.uff.br/joel/dados/StockPrice.csv")
 summary(stock)
#######################################################################################
# Vetor com as 5 ações                                                                #
#######################################################################################

 X<-stock[,-1]
 head(X)
####################################################################################### 
# Matriz de Correlações
#######################################################################################
 R<-cor(X)
  R
#######################################################################################
#### Vamos aplicar o mŽtodo das componentes principais pelo comando princomp.
### Primeiramente aplicaremos à matriz de covariâncias  S.
#######################################################################################

pc.S<-princomp(X)
 pc.S


####  O resumo dos resultados é apresentado pelo comando summary

summary(pc.S)



####Abaixo vamos apresentar o scree-plot, em formato do gráfico de barras

plot(pc.S)



###    Além do scree-plot, vamos apresentar o biplot.

 biplot(pc.S) 
###          Para encontrar as matrizes L e ?(psi) vamos obter os autovalores e autovetores da matriz de
####   Covariâncias.
S=cov(X)
 lambda<-eigen(S)$values
 evec<-eigen(S)$vectors

 lambda
 
evec

###Vamos obter a matriz de cargas fatoriais L para o modelo "completo”, ou seja, com o
####número de fatores igual ao número de variáveis.

L=sqrt(lambda)*t(evec)
 L<-t(L)
 LLT<-L%*%t(L)
 LLT



round(S-LLT,2) 

# Proporção da variabilidade explicada por cada componente

lambda/sum(lambda)

#Modelo fatorial com m = 2

 Lm2<-L[,1:2]
 Lm2

 LLTm2<-Lm2%*%t(Lm2)
 LLTm2


Psi<-diag(diag(S-LLTm2))
Psi

#Matriz Residual
 S-(LLTm2+Psi)

##Obtendo os escores fatorias pelo método dos mínimos quadrados ponderados.
###Primeiramente os pesos

 W<-solve(t(Lm2)%*%solve(Psi)%*%Lm2)%*%(t(Lm2)%*%solve(Psi))
 W
 
####Agora os escores fatoriais


Fs<-W%*%t(X)
 Fs<-t(Fs)
 Fs
#############################################################################
#############################################################################
#############################################################################

data(iris)
head(iris, 3)


# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]
 
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,
                 center = TRUE,
                 scale. = TRUE) 
print(ir.pca)

plot(ir.pca, type = "l")


summary(ir.pca)
 predict(ir.pca, 
        newdata=tail(log.ir, 2))

install.packages("devtools")
library(devtools)

install.packages("psych")
require(psych)


irisX <- iris[2:nrow(iris), 1:4]      # Iris data
ncomp <- 2

pca_iris_rotated <- psych::principal(irisX, rotate="varimax", nfactors=ncomp, scores=TRUE)
print(pca_iris_rotated$scores[1:5,])  # Scores returned by principal()

pca_iris        <- prcomp(irisX, center=T, scale=T)
rawLoadings     <- pca_iris$rotation[,1:ncomp] %*% diag(pca_iris$sdev, ncomp, ncomp)
rotatedLoadings <- varimax(rawLoadings)$loadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
scores          <- scale(irisX) %*% invLoadings
print(scores[1:5,])                   # Scores computed via rotated loadings

scores <- scale(pca_iris$x[,1:2]) %*% varimax(rawLoadings)$rotmat
print(scores[1:5,])   


# Principal Axis Factor Analysis
library(psych)
fit <- factor.pa(mydata, nfactors=3, rotation="varimax")
fit # print results


###################################################################################
###################################################################################
###################################################################################



### Os dados acima encontram-se no arquivo sedimento.txt (sedimento), assim a entrada dos dados no R pode ocorrer da seguinte maneira:
 
dados<-read.table("sedimento.txt",header=T)
dados
 
### Para construir a PCA será utilizado o pacote vegan
 
library(vegan)
 
###  Em geral, dois tipos de PCA são feitas: a PCA de covariância e a PCA de correlação. Primeiramente, será demonstrado a PCA de covariância.
 
resultado<-prcomp(dados) 

# prcomp é a função para construir a PCA
 
#### Para mostrar os desvios dos componentes principais e os loadings
 
resultado



################## Para mostrar a porcentagem de variância capturada por cada eixo
   
summary(resultado)


### Para salvar os scores da PCA (eixos)
 
resultado$x
 
#### Para salvar os loadings
 
resultado$loadings
 
##### Para plotar 
 
biplot(resultado)

screeplot(resultado) 


##### Agora, PCA de correlação. A PCA de correlação é construida usando os dados que foram padronizados para ter média 0 e desvio padrão 1. 
###A PCA de correlação é adequada quando para quando as variáveis foram medidas em unidades diferentes ou quando a variância de cada variável é muito diferente umas das outras. Para ver os dados de variáveis químicas do arquivo sedimento.
 
round(apply(sedimento,2,var),4) 


# veja que a variância de cada variável é muito diferente. Então, temos que usar uma PCA de correlação para que a variável com maior variância não "domine" a análise.

 
 
## A PCA de correlação pode ser realizada de duas maneiras: i) padronizando as variáveis ou ii) mudando o argumento scale da função prcomp para scale=TRUE. 
 
## Para padronizar os dados
 
sedimentopadronizado<-scale(sedimento)
 
### Agora basta refazer a PCA usando os dados padronizados 
 
prcomp(sedimentopadronizado)
 
## Ou apenas mudar o argumento scale=T
 
prcomp(sedimento, scale=T)
 

###################################################################################
###################################################################################
###################################################################################

###

wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
          sep=",")
head(wine)
install.packages("car")
require("car")
head(wine[2:6])

plot(wine[2:6])
scatterplotMatrix(wine[2:6])

plot(wine$V4, wine$V5)

text(wine$V4, wine$V5, wine$V1, cex=0.7, pos=4, col="red")

##########################################################################
## Função para traçar perfis 
########################################################################
makeProfilePlot <- function(mylist,names)
  {
     require(RColorBrewer)
     # find out how many variables we want to include
     numvariables <- length(mylist)
     # choose 'numvariables' random colours
     colours <- brewer.pal(numvariables,"Set1")
     # find out the minimum and maximum values of the variables:
     mymin <- 1e+20
     mymax <- 1e-20
     for (i in 1:numvariables)
     {
        vectori <- mylist[[i]]
        mini <- min(vectori)
        maxi <- max(vectori)
        if (mini < mymin) { mymin <- mini }
        if (maxi > mymax) { mymax <- maxi }
     }
     # plot the variables
     for (i in 1:numvariables)
     {
        vectori <- mylist[[i]]
        namei <- names[i]
        colouri <- colours[i]
        if (i == 1) { plot(vectori,col=colouri,type="l",ylim=c(mymin,mymax)) }
        else         { points(vectori, col=colouri,type="l")                                     }
        lastxval <- length(vectori)
        lastyval <- vectori[length(vectori)]
        text((lastxval-10),(lastyval),namei,col="black",cex=0.6)
     }
  }

#################################################################################################

install.packages("RColorBrewer")
library(RColorBrewer)
 names <- c("V2","V3","V4","V5","V6")
 mylist <- list(wine$V2,wine$V3,wine$V4,wine$V5,wine$V6)
 makeProfilePlot(mylist,names)


###############################################################################
sapply(wine[2:14],mean)

sapply(wine[2:14],sd)


 cor.test(wine$V2, wine$V3)

 mosthighlycorrelated <- function(mydataframe,numtoreport)
  {
     # find the correlations
     cormatrix <- cor(mydataframe)
     # set the correlations on the diagonal or lower triangle to zero,
     # so they will not be reported as the highest ones:
     diag(cormatrix) <- 0
     cormatrix[lower.tri(cormatrix)] <- 0
     # flatten the matrix into a dataframe for easy sorting
     fm <- as.data.frame(as.table(cormatrix))
     # assign human-friendly names
     names(fm) <- c("First.Variable", "Second.Variable","Correlation")
     # sort and print the top n correlations
     head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
  }

 mosthighlycorrelated(wine[2:14], 10)


############################ Variaveis padronizadas


 standardisedconcentrations <- as.data.frame(scale(wine[2:14]))

 standardisedconcentrations <- as.data.frame(scale(wine[2:14]))
 wine.pca <- prcomp(standardisedconcentrations)                 

 summary(wine.pca)
 wine.pca$sdev
 sum((wine.pca$sdev)^2)
 screeplot(wine.pca, type="lines")

