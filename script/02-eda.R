# Carregar pacotes necessários --------------------------------------------

library(sjPlot)
library(easyORtables)
library(xfun)
library(plotor)      
library(datasets)   
library(dplyr)       
library(tidyr)       
library(forcats)     
library(stats)      
library(broom)       
library(ggplot2)     
library(gtsummary)
library(tidyverse)
library(ResourceSelection)
library(BaylorEdPsych)
library(DescTools)

# Mudar formato do número a ser usado
options(scipen = 100) # evitar usar número científico como padrão

# Importando base de dados ------------------------------------------------

fuvest <- readxl::read_xlsx("dados/fuvest.xlsx")

# Selecionando variáveis de interesse, conforme dicionário
# da FUVEST.
fuvest <- fuvest |> 
  dplyr::select(
    c(ano,V4,V6,V7,V8,V9,V11,V12,V15,V16,V17,V18,V25,V26)
  )

fuvest <- fuvest |> 
  dplyr::rename(
    matricula = V4,
    sexo = V6,
    raca = V7,
    renda = V8,
    n_moradores = V9,
    esc1 = V11,
    esc2 = V12,
    ensino_fund = V15,
    ensino_med = V16,
    tipo_EM = V17,
    cursinho = V18, 
    modalidade_inscricao = V25,
    modalidade_matricula = V26
  )

# Criação do modelo de Regressão ------------------------------------------
# Gerar o modelo a partir de variáveis de interesse

mod <- glm(formula = matricula ~ renda + esc1 + ensino_fund +
             ensino_med + tipo_EM + cursinho,
           family=binomial(link=probit), data=fuvest)

# Imprimir o modelo
base::summary(mod)

tab_model(mod)

## ---------------------- Criação de um Modelo Nulo para comparação ------------

# OBS: Em modelos de regressão se compara o modelo proposto com o modelo nulo.
# modelo nulo: apenas VD e o intercepto
mod_null <- glm(formula = matricula ~ 1, family=binomial(link=logit),
                     data=fuvest) 

## ---------------------- Tabela 1 - Descritivo da amostra no modelo -----------

base_rep <- mod$model
table(base_rep$matricula)
round(prop.table(table(base_rep$matricula)),2)

### 17% convocados para matrícula, 83% não convocados para matrícula. 


## ---------------------- Tabela 2 - Teste de Hosmer e Lemeshow ----------------
hoslem.test(mod$y, fitted(mod), g=10)

## ---------------------- Tabela 3 - Teste Omnibus dos coeficientes  -----------

# OBS: O SPSS entrega essa tabela como default. No R precisamos calculá-lo. 
#     Primeiro, precisamos dos graus de liberdade

mod$df.null-mod$df.residual # Gl do modelo nulo, menos Gl do nosso 
# modelo completo.


#     Segundo, deviance do modelo nulo, menos o desvio do modelo completo
#     Essa diferença se distribui em ChiSquare

with(mod, null.deviance - deviance)

#     Terceiro, teste de probabilidade (p-valor) de que o modelo completo 
#   explica mais a variável dependente do que o modelo nulo.

with(mod, pchisq(null.deviance - deviance, df.null - df.residual, 
                      lower.tail = FALSE))

# Chi Square: 168.4118
# P-valor 0.0000000000000000000000000000000009768134

## ---------------------- Tabela 4 coeficientes --------------------------------

# Vamos coletar os coeficientes em um dataframe para facilitar a manipulação

coeficientes <- data.frame(cbind(round(
  summary(mod)$coefficients,3), # Coeficientes Beta, erro padrão, z, p-valor
  exp(summary(mod)$coefficients[,1]), # exponencial de Beta
  (exp(summary(mod)$coefficients[,1])-1)*100)) # magnitude de Beta em probabilidade percentual 

# Renomeando o nome de cada coluna coeficiente 
names(coeficientes) <- c('B', 'Erro Padrão', 'Z', 'Sig.', 'exp(B)', '(exp(B)-1) x 100')

# salvando essa tabela
write.table(coeficientes, 'coeficientes.csv', sep = ';', dec = ',')
write.table(coeficientes, 'coeficientes_probit.csv', sep = ';', dec = ',')

## ---------------------- Tabela 5 - Medidas de ajustes ------------------------

# -2LL
round(-2*logLik(mod),2) #pode ser obtida por fit5_rep$fit5_rep$deviance

# -2LL do modelo nulo
round(-2*logLik(mod_null),2)

# Pseudo R² 
# OBS: Reportaremos apenas Cox e Snell e Nagelkerke seguindo o artigo original
round(PseudoR2(mod),3)

##McFadden 
###0.046 

# BIC
round(BIC(mod),3)
round(BIC(mod_null),3) # BIC do modelo nulo

##  --------------------- Tabela 8. Tabela de classificação --------------------

# calculando os valores preditos do modelo
pred <- predict(mod, type = 'response')

# OBS: Calcula o percentual das margens (total), indicando que probabilidades 
#     preditas maiores que 0.5 serão consideradas 1 (ou seja, acerto)

addmargins(prop.table(table(mod$y, pred > 0.5)))






