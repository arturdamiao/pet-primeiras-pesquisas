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
library(caret)
library(modelbased)
library(easystats)
# Mudar formato do número a ser usado
options(scipen = 100) # evitar usar número científico como padrão

# Importando base de dados ------------------------------------------------

fuvest <- readxl::read_xlsx("dados/fuvest.xlsx")

# Selecionando variáveis de interesse, conforme dicionário
# da FUVEST.
fuvest <- fuvest |> 
  dplyr::select(
    c(ano,V4,V6,V7,V8,V9,V11,V12,V15,V16,V17,V18)
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
    cursinho = V18
  )

fuvest <- fuvest |>
  dplyr::mutate(renda = as.factor(renda),
                esc1 = as.factor(esc1),
                ensino_fund = as.factor(ensino_fund),
                ensino_med = as.factor(ensino_med),
                tipo_EM = as.factor(tipo_EM),
                raca = factor(raca, labels = c("branco", "preto", "pardo", "amarelo", "indígena"))) # checar se está certo

# Criação do modelo de Regressão ------------------------------------------
# Gerar o modelo a partir de variáveis de interesse

# mod <- glm(formula = matricula ~ renda + esc1 + ensino_fund +
#              ensino_med + tipo_EM + cursinho,
#            family=binomial(link=logit), data=fuvest)

mode_lm <- lm(formula = matricula ~ renda + esc1 + ensino_fund +
                 ensino_med + tipo_EM + cursinho + raca, data=fuvest)
summary(mode_lm)


modelbased::estimate_means(mode_lm, by = "renda")

means_renda <- estimate_means(mode_lm, by = "renda")

plot(means_renda)

means_raca <- estimate_means(mode_lm, by = "raca")

plot(means_raca)

means_renda_raca <- estimate_means(mode_lm, by = c("renda", "raca"))

plot(means_renda_raca)

means_renda_em <- estimate_means(mode_lm, by = c("renda", "tipo_EM"))

plot(means_renda_em)

mode_lm <- lm(matricula ~  raca, data=fuvest)

expec <- estimate_expectation(mode_lm)
pred <- estimate_prediction(mode_lm)
head(pred)



pred |>
  ggplot(aes(y = Predicted, x = raca)) + geom_col() +
  geom_errorbar() +
  theme_minimal()

pred |>
  ggplot(aes(y = Predicted, x = renda)) + geom_col() +
  theme_minimal()


# Imprimir o modelo
base::summary(mod)

invlogit(coef(mod)[1] + coef(mod)[2]*9 + coef(mod)[3]*6 + coef(mod)[4]*2 + coef(mod)[5]*2 +
           coef(mod)[6]*1 + coef(mod)[7]*0)

invlogit(coef(mod)[1] + coef(mod)[2]*9 + coef(mod)[3]*6 + coef(mod)[4]*2 + coef(mod)[5]*2 +
           coef(mod)[6]*1 + coef(mod)[7]*1)
tab_model(mod)

coef(mode_lm)[1] + coef(mode_lm)[2]*2 + coef(mode_lm)[3]*1 + coef(mode_lm)[4]*1 + coef(mode_lm)[5]*1 +
  coef(mode_lm)[6]*1 + coef(mode_lm)[7]*1

mod_0 <- glm(formula = matricula ~ cursinho,
           family=binomial(link=logit), data=fuvest)

summary(mod_0)

### Dúvida: modelo link logit ou probit? 

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
round(-2*logLik(mod),2)

# -2LL do modelo nulo
round(-2*logLik(mod_null),2)

# Pseudo R² de McFadden 
round(PseudoR2(mod, which = "McFadden"),3)

# Pseudo R² de Cox & Snell
round(PseudoR2(mod, which = "CoxSnell"), 3)

# Pseudo R² de Nagelkerke
round(PseudoR2(mod, which = "Nagelkerke"), 3)

# BIC
round(BIC(mod),3)
round(BIC(mod_null),3) # BIC do modelo nulo

# Gerar tabela com resultados
tabela_ajustes <- data.frame(
  Medida = c("-2 Log-Likelihood (-2LL)", "Pseudo R² de McFadden", "Pseudo R² de Cox & Snell", 
             "Pseudo R² de Nagelkerke", "BIC"),
  `Valor Modelo Logit` = c(round(-2*logLik(mod), 2),
                           round(PseudoR2(mod, which = "McFadden"), 3),
                           round(PseudoR2(mod, which = "CoxSnell"), 3),
                           round(PseudoR2(mod, which = "Nagelkerke"), 3),
                           round(BIC(mod), 3)),
  `Valor Modelo Nulo` = c(round(-2*logLik(mod_null), 2),
                          NA, NA, NA, round(BIC(mod_null), 3))
)

# Exibir tabela
print(tabela_ajustes) |> 
  flextable::flextable()


##  --------------------- Tabela 6. Tabela de classificação --------------------

# calculando os valores preditos do modelo
pred <- predict(mod, type = 'response')

# OBS: Calcula o percentual das margens (total), indicando que probabilidades 
#     preditas maiores que 0.5 serão consideradas 1 (ou seja, acerto)

addmargins(prop.table(table(mod$y, pred > 0.3)))

# Matriz de confusão com corte de 0.3
table(mod$y, pred > 0.3)



















