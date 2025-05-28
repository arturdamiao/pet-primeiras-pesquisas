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
library(DescTools)
library(caret)
library("predict3d")
library(jtools)
library(huxtable)

# Mudar formato do número a ser usado
options(scipen = 100) # evitar usar número científico como padrão

# Importando base de dados ------------------------------------------------

fuvest <- readxl::read_xlsx("dados/fuvest_tratado.xlsx")
fuvest <- fuvest |> 
  tidyr::drop_na()


# Construindo o modelo ------------------------------------------

modelo1 <- lm(formula = matricula ~ renda + raca + esc1 + esc2 + ensino_fund +
                ensino_med + tipo_EM + cursinho,
              data=fuvest)
modelo2 <- lm(formula = matricula ~ renda + raca + esc1 + esc2 + ensino_fund +
                ensino_med + tipo_EM,
              data=fuvest)
modelo3 <- lm(formula = matricula ~ renda + raca + esc1 + esc2 + ensino_fund +
                ensino_med,
              data=fuvest)
modelo4 <- lm(formula = matricula ~ renda + raca + esc1 + esc2 + ensino_fund,
              data=fuvest)
modelo5 <- lm(formula = matricula ~ renda + raca + esc1 + esc2,
              data=fuvest)
modelo6 <- lm(formula = matricula ~ renda + raca,
              data=fuvest)
modelo7 <- lm(formula = matricula ~ renda,
              data=fuvest)

# Imprimir o modelo
base::summary(modelo0)

jtools::export_summs(modelo1)
jtools::plot_summs(modelo2, modelo3, modelo4, robust = "HC3")

modelo2 |> 
  tbl_regression()

plot(modelo1)

performance::check_model(modelo1)


# Checagem de pressupostos --------------------------------------

modelo0 <- lm(formula = matricula ~ renda + escolaridade + tipo_EM,
              data=fuvest)

modelo0 |> tbl_regression() |> 
  bold_labels()


## Resíduos 
res <- rstandard(modelo00);res

## Teste de normalidade (Shapiro-Wilki)
shapiro.test(res)
# Dados não são normais

## Independência

# Dados são independentes, pela forma que foram coletados. 

## Teste de variância (Breush-Pagan)
car::ncvTest(modelo0)
# Presença de heterocedasticidade, ou seja, os resíduos não tem variância constante ao longo dos valores ajustados. 

modelo00 <- glm(formula = matricula ~ renda + escolaridade + tipo_EM,
              data=fuvest)

summary(modelo00)


# Comparação modelos ---------------------------------------------

## AIC

AIC(modelo1, modelo2, modelo3, modelo4, modelo5, modelo6, modelo7)

## ANOVA - Para modelos aninhados ou hierárquicos

anova(modelo1, modelo2, modelo3, modelo4, modelo5, modelo6, modelo7)

### O melhor modelo é o com o menor RSS - Residual Sum of Squares. O modelo 1. 










# Interpretando o modelo ----------------------------------------

# De modo geral, temos quatro grandes métricas para avaliar o nosso modelo de regressão. A linearidade, que diz respeito sobre a relação entre X e Y ser linear; a homocedasticidade, onde a variação dos resíduos deve ser a mesma para qualquer valor de X; a independência, onde as nossas observações são independentes umas das outras; e, por fim, a normalidade, onde os residuais do modelo estão distribuídos normalmente.

# Ao interpretar os coeficientes do modelo, é possível constatar que, a medida que faixa de renda avança, há um aumento no percentual de chance de probabilidade na matrícula, em relação à categoria de referência (faixa de renda 1, inferior a 1 salário mínimo). A diferença começa a ser estasticamente signifativa a partir da faixa de renda 7 (de 10 a 14,9 SM), onde uma pessoa com essa faixa de renda tem 10 pontos percentuais de probabilidade de ingresso em relação à categoria de referência; uma pessoa na faixa de renda 8 (15 a 19,9 SM) tem 18 pontos percentuais de probabilidade de ingresso em relação a uma pessoa na faixa de renda 1 e uma pessoa com faixa de renda 9 (igual ou superior a 20 SM) tem 20 pontos percentuais de probabilidade em relação a uma pessoa na faixa de renda 1. 

# Além disso, o modelo reporta que pais com somente o ensino médio completo está associada a uma redução na chance de matrícula. 

# Por fim, o tipo de ensino médio também é estatisticamente significativo para a probabilidade de matrícula. Pessoas com Ensino Médio Técnico (ETEC, IF, etc) apresentam maior chance de ingresso em relação a pessoas com Ensino Médio Comum (diálogo com Takata, Dokter e Damião (2025)).






