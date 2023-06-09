
################################################################################
###      Os dados                                                           ####         
################################################################################
## Cuse data � dados sobre o uso de contraceptivos, mostrando a distribui��o de #
## 16 mulheres  atualmente casadas e f�rteis entrevistados na Pesquisa de    #  
## Fertilidade Fiji, de acordo com a idade, a escolaridade, e o desejo de ter  #
## mais filhos e atual uso de contraceptivos.                                  #
################################################################################       

cuse =read.table("http://data.princeton.edu/wws509/datasets/cuse.dat", header=TRUE)

cuse

attach(cuse)

################################################################################       
# Vamos primeiro tentar um modelo aditivo simples, onde o uso de               #   
#  anticoncepcionais depende das vari�veis  age, education e wantsMore:        #  
################################################################################       


lrfit=glm( cbind(using, notUsing) ~ age + education + wantsMore , family =binomial)

lrfit
################################################################################       
## Recordamos que o R ordena os n�veis de um fator na ordem alfab�tica. Porque #
## <25 vem antes de 25|- 29, 30|-�39, e 40|-�49, que tem sido escolhido como a ###
## c�lula de refer�ncia para age.                                             #
################################################################################       

noMore =wantsMore == "no"

hiEduc=education == "high"

glm( cbind(using,notUsing) ~ age + hiEduc + noMore, family=binomial)
################################################################################       

# A Deviance residual de 29.92 com 10 g.l � altamente significativa
################################################################################       

 1-pchisq(29.92,10)
###################################################################################      
# por isso precisamos de um modelo melhor. Verificaremos um modelo que introduz ### 
# uma intera��o  entre idade e desejo de ter mais filhos:                         #
###################################################################################
lrfit =glm( cbind(using,notUsing) ~ age * noMore + hiEduc , family=binomial)

lrfit
################################################################################       
################################################################################       
#													 #
#Para analise de res�duos use plot (lrfit). Voc� obt�m os mesmos resultados    #
# como no modelo linear, mas adaptado para um modelo linear generalizado; por  #
#exemplo, os res�duos plotados s�o: res�duos deviance (a raiz quadrada da      #
#contribui��o de uma observa��o ao desvio, com o mesmo sinal que do res�duo    #
#bruto). As fun��es que podem ser usados para extrair os resultados do ajuste  #
#incluem:                                                                      #  
##� residuals ou resid, para os �res�duos deviance�                            #
##� fitted ou fitted.values, para os valores estimados (no caso da regress�o   #
## log�stica )                                                                 #                          #
## � predict, para os preditores lineares (logits estimadas)                   #
## � coef ou coefficients, para os coeficientes                                #    
## � deviance,para a deviance.                                                 #  
################################################################################       
#												       #				
#Algumas dessas fun��es t�m argumentos opcionais; por exemplo, voc� pode       # 
#extrair cinco diferentes tipos de res�duos: "deviance", "pearson", "response" #
#(response � fitted value), "working" ( working vari�vel dependente no algoritmo#
# IRLS � preditor linear), e "partial" (uma matriz de res�duos de X formado por #
# omitir cada termo no modelo). Voc� pode  especificar o que res�duo deseja    #
# usando o argumento type, por examplo "residuals(lrfit,type="pearson")."      #  
#se voc� quiser modificar um modelo que voc� pode considerar usar a fun��o     #
# especial update. Por exemplo, para modificar a intera��o                     #
#age:noMore:  
################################################################################       

lrfit0=update(lrfit, ~ . -age:noMore)
lrfit0
################################################################################       
#													 # 	
#O primeiro argumento � o resultado de um ajuste, e o segundo uma f�rmula de   #
# atualiza��o. O ~ separa a resposta dos preditores e ponto refere�se ao lado   #
# direito da f�rmula original, ent�o aqui simplesmente removemos age:noMore.   #
#  Alternativamente, pode�-se dar uma nova f�rmula como o segundo argumento.    #  
# A fun��o de atualiza��o pode ser usado para ajustar o mesmo modelo para      # 
# diferentes conjuntos de dados, usando os dados de argumento para especificar #
# um novo quadro de dados. Um outro  argumento �til � subset, para ajustar o   #
# modelo a uma subamostra diferente. Esta fun��o trabalha com modelos lineares,#
# bem como modelos lineares generalizados.                                     #  
# Para encaixar uma sequ�ncia de modelos a fun��o anova � �til . Dado uma s�rie#
# de modelos encaixados, anova ir� calculara a mudan�a de deviance entre eles  #
################################################################################       
     

anova(lrfit0,lrfit)

################################################################################       
### Ao aumentar a intera��o tem reduzido o desvio por 17.288 � custa de 3 g.l. #  
## Se o argumento para anova � um �nico modelo, a fun��o ir� mostrar a mudan�a #
## de desvio obtida  pela adi��o de cada um dos termos na ordem listada na     #
## f�rmula modelo, tal como aconteceu para os modelos lineares.                #    
#A fun��o anova permite que voc� especifique um teste opcional. As escolhas    #
#  habituais ser�o "F" para os modelos lineares e "Chisq" para modelos lineares#
# generalizados. Adicionando test= "Chisq", acrescenta p�valores ao lado dos    #
#desvios. No nosso caso:                                                       #
################################################################################       
     

anova(lrfit,test="Chisq")
################################################################################       
# Podemos ver que todos os termos foram altamente significativos quando foram  #    
# introduzidas no modelo.
################################################################################       

################################################################################       

################################################################################       
#########                           Sele��o do modelo                         ##    
#################################################################################       
# Uma ferramenta muito poderosa em R � uma fun��o para a regress�o gradual que  #
#tem tr�s caracter�sticas marcantes:                                            #  
# 1. Ele funciona com modelos lineares generalizados, por isso vai fazer de     #
# regress�o log�stica, ou passo a passo regress�o de Poisson,                   #  
# 2. entender sobre modelos hier�rquicos, por isso s� vai considerar a adi��o   #
# de intera��es somente  ap�s incluir os efeitos principais correspondentes nos #
# modelos e                                                                     #
# 3. Entende termos que envolvem mais de um grau de liberdade, por isso ele vai #
# manter vari�veis em conjunto fict�cios que representam os efeitos de um fator.#
# A id�ia b�sica do processo � come�ar a partir de um determinado modelo (que   #
# poderia muito bem ser o modelo nulo) e tomar uma s�rie de medidas por qualquer#
# exclus�o de um termo j� no modelo ou a adi��o de um termo de uma lista de     #
# candidatos para a inclus�o, chamado a fun��o scope j� definida, � claro, por  #
#  uma f�rmula modelo.                                                          #    
# Sele��o de termos para exclus�o ou inclus�o tem por base o crit�rio de        #  
# informa��o de Akaike (AIC).                                                   #  
# O R define AIC como:                                                          #
#                �2 maximized log�likelihood + 2 number of parameters            #      
# O procedimento termina quando o crit�rio de AIC n�o pode ser melhorado.       # 
# NO R todo esse trabalho � feito chamando um par de fun��es, add1 e drop1, que #
# considera adicionar ou tirar um termo de um modelo. Essas fun��es podem ser   #
# muito �teis na sele��o de modelos.							  #	
# Consideres drop1. Para o modelo logistico,                                    #
#################################################################################

drop1(lrfit, test="Chisq")

#################################################################################
# Obviamente, n�o podemos deixar sair qualquer um destes termos. Note� que o R   #
# considerou deixar sair o principal efeito da educa��o e da idade por rejeitar #   
# termo de intera��o, mas n�o examinou os principais efeitos da idade ou n�o    #
#quero mais filhos (no more) , porque n�o iria sair estes efeitos principais,   #
# mantendo a intera��o.                                                         #
################################################################################# 

#################################################################################
##A fun��o  add1 requer uma margem de manobra para definir os termos adicionais #
#a serem considerados. No nosso exemplo, vamos considerar todas as poss�veis    #
# intera��es de dois fatores:
#################################################################################
add1(lrfit, ~.^2,test="Chisq")

#################################################################################
# Vemos que nenhuma das intera��es de dois fatores ausentes � significativa     #
# por si s� ao n�vel  de cinco por cento de signific�ncia. (No entanto, eles    #
#  s�o conjuntamente significativas.) Note� que o modelo com a idade pela        #
#  intera��o com  educa��o tem um AIC mais baixo do que o nosso modelo de       #
# partida.                                                                      #
# A fun��o step faz uma pesquisa autom�tica.                                    #                      
#################################################################################
search = step(lrfit, ~.^2)


search$anova

