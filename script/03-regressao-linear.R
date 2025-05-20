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

modelo1 <- lm(formula = matricula ~ renda + raca + esc1 + esc2 + ensino_fund +
                ensino_med + tipo_EM + cursinho, data=fuvest)
modelo2 <- lm(formula = matricula ~ renda, data=fuvest)
modelo3 <- lm(formula = matricula ~ esc1 + esc2, data=fuvest)
modelo4 <- lm(formula = matricula ~ tipo_EM, data=fuvest)
modelo5 <- lm(formula = matricula ~ cursinho, data=fuvest)

# Imprimir o modelo
base::summary(modelo1)

jtools::export_summs(modelo1)
jtools::plot_summs(modelo2, modelo3, modelo4, robust = "HC3")

modelo2 |> 
  tbl_regression()
