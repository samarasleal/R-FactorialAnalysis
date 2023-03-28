
################################################################################
###      Os dados                                                           ####         
################################################################################
## Cuse data ­ dados sobre o uso de contraceptivos, mostrando a distribuição de #
## 16 mulheres  atualmente casadas e férteis entrevistados na Pesquisa de    #  
## Fertilidade Fiji, de acordo com a idade, a escolaridade, e o desejo de ter  #
## mais filhos e atual uso de contraceptivos.                                  #
################################################################################       

cuse =read.table("http://data.princeton.edu/wws509/datasets/cuse.dat", header=TRUE)

cuse

attach(cuse)

################################################################################       
# Vamos primeiro tentar um modelo aditivo simples, onde o uso de               #   
#  anticoncepcionais depende das variáveis  age, education e wantsMore:        #  
################################################################################       


lrfit=glm( cbind(using, notUsing) ~ age + education + wantsMore , family =binomial)

lrfit
################################################################################       
## Recordamos que o R ordena os níveis de um fator na ordem alfabética. Porque #
## <25 vem antes de 25|- 29, 30|-­39, e 40|-­49, que tem sido escolhido como a ###
## célula de referência para age.                                             #
################################################################################       

noMore =wantsMore == "no"

hiEduc=education == "high"

glm( cbind(using,notUsing) ~ age + hiEduc + noMore, family=binomial)
################################################################################       

# A Deviance residual de 29.92 com 10 g.l é altamente significativa
################################################################################       

 1-pchisq(29.92,10)
###################################################################################      
# por isso precisamos de um modelo melhor. Verificaremos um modelo que introduz ### 
# uma interação  entre idade e desejo de ter mais filhos:                         #
###################################################################################
lrfit =glm( cbind(using,notUsing) ~ age * noMore + hiEduc , family=binomial)

lrfit
################################################################################       
################################################################################       
#													 #
#Para analise de resíduos use plot (lrfit). Você obtém os mesmos resultados    #
# como no modelo linear, mas adaptado para um modelo linear generalizado; por  #
#exemplo, os resíduos plotados são: resíduos deviance (a raiz quadrada da      #
#contribuição de uma observação ao desvio, com o mesmo sinal que do resíduo    #
#bruto). As funções que podem ser usados para extrair os resultados do ajuste  #
#incluem:                                                                      #  
##• residuals ou resid, para os “resíduos deviance”                            #
##• fitted ou fitted.values, para os valores estimados (no caso da regressão   #
## logística )                                                                 #                          #
## • predict, para os preditores lineares (logits estimadas)                   #
## • coef ou coefficients, para os coeficientes                                #    
## • deviance,para a deviance.                                                 #  
################################################################################       
#												       #				
#Algumas dessas funções têm argumentos opcionais; por exemplo, você pode       # 
#extrair cinco diferentes tipos de resíduos: "deviance", "pearson", "response" #
#(response ­ fitted value), "working" ( working variável dependente no algoritmo#
# IRLS ­ preditor linear), e "partial" (uma matriz de resíduos de X formado por #
# omitir cada termo no modelo). Você pode  especificar o que resíduo deseja    #
# usando o argumento type, por examplo "residuals(lrfit,type="pearson")."      #  
#se você quiser modificar um modelo que você pode considerar usar a função     #
# especial update. Por exemplo, para modificar a interação                     #
#age:noMore:  
################################################################################       

lrfit0=update(lrfit, ~ . -age:noMore)
lrfit0
################################################################################       
#													 # 	
#O primeiro argumento é o resultado de um ajuste, e o segundo uma fórmula de   #
# atualização. O ~ separa a resposta dos preditores e ponto refere­se ao lado   #
# direito da fórmula original, então aqui simplesmente removemos age:noMore.   #
#  Alternativamente, pode­-se dar uma nova fórmula como o segundo argumento.    #  
# A função de atualização pode ser usado para ajustar o mesmo modelo para      # 
# diferentes conjuntos de dados, usando os dados de argumento para especificar #
# um novo quadro de dados. Um outro  argumento útil é subset, para ajustar o   #
# modelo a uma subamostra diferente. Esta função trabalha com modelos lineares,#
# bem como modelos lineares generalizados.                                     #  
# Para encaixar uma sequência de modelos a função anova é útil . Dado uma série#
# de modelos encaixados, anova irá calculara a mudança de deviance entre eles  #
################################################################################       
     

anova(lrfit0,lrfit)

################################################################################       
### Ao aumentar a interação tem reduzido o desvio por 17.288 à custa de 3 g.l. #  
## Se o argumento para anova é um único modelo, a função irá mostrar a mudança #
## de desvio obtida  pela adição de cada um dos termos na ordem listada na     #
## fórmula modelo, tal como aconteceu para os modelos lineares.                #    
#A função anova permite que você especifique um teste opcional. As escolhas    #
#  habituais serão "F" para os modelos lineares e "Chisq" para modelos lineares#
# generalizados. Adicionando test= "Chisq", acrescenta p­valores ao lado dos    #
#desvios. No nosso caso:                                                       #
################################################################################       
     

anova(lrfit,test="Chisq")
################################################################################       
# Podemos ver que todos os termos foram altamente significativos quando foram  #    
# introduzidas no modelo.
################################################################################       

################################################################################       

################################################################################       
#########                           Seleção do modelo                         ##    
#################################################################################       
# Uma ferramenta muito poderosa em R é uma função para a regressão gradual que  #
#tem três características marcantes:                                            #  
# 1. Ele funciona com modelos lineares generalizados, por isso vai fazer de     #
# regressão logística, ou passo a passo regressão de Poisson,                   #  
# 2. entender sobre modelos hierárquicos, por isso só vai considerar a adição   #
# de interações somente  após incluir os efeitos principais correspondentes nos #
# modelos e                                                                     #
# 3. Entende termos que envolvem mais de um grau de liberdade, por isso ele vai #
# manter variáveis em conjunto fictícios que representam os efeitos de um fator.#
# A idéia básica do processo é começar a partir de um determinado modelo (que   #
# poderia muito bem ser o modelo nulo) e tomar uma série de medidas por qualquer#
# exclusão de um termo já no modelo ou a adição de um termo de uma lista de     #
# candidatos para a inclusão, chamado a função scope já definida, é claro, por  #
#  uma fórmula modelo.                                                          #    
# Seleção de termos para exclusão ou inclusão tem por base o critério de        #  
# informação de Akaike (AIC).                                                   #  
# O R define AIC como:                                                          #
#                –2 maximized log­likelihood + 2 number of parameters            #      
# O procedimento termina quando o critério de AIC não pode ser melhorado.       # 
# NO R todo esse trabalho é feito chamando um par de funções, add1 e drop1, que #
# considera adicionar ou tirar um termo de um modelo. Essas funções podem ser   #
# muito úteis na seleção de modelos.							  #	
# Consideres drop1. Para o modelo logistico,                                    #
#################################################################################

drop1(lrfit, test="Chisq")

#################################################################################
# Obviamente, não podemos deixar sair qualquer um destes termos. Note­ que o R   #
# considerou deixar sair o principal efeito da educação e da idade por rejeitar #   
# termo de interação, mas não examinou os principais efeitos da idade ou não    #
#quero mais filhos (no more) , porque não iria sair estes efeitos principais,   #
# mantendo a interação.                                                         #
################################################################################# 

#################################################################################
##A função  add1 requer uma margem de manobra para definir os termos adicionais #
#a serem considerados. No nosso exemplo, vamos considerar todas as possíveis    #
# interações de dois fatores:
#################################################################################
add1(lrfit, ~.^2,test="Chisq")

#################################################################################
# Vemos que nenhuma das interações de dois fatores ausentes é significativa     #
# por si só ao nível  de cinco por cento de significância. (No entanto, eles    #
#  são conjuntamente significativas.) Note­ que o modelo com a idade pela        #
#  interação com  educação tem um AIC mais baixo do que o nosso modelo de       #
# partida.                                                                      #
# A função step faz uma pesquisa automática.                                    #                      
#################################################################################
search = step(lrfit, ~.^2)


search$anova

