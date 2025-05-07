# carregar pacotes
if(require(BaylorEdPsych)==F)install.packages('BaylorEdPsych');require(BaylorEdPsych)
if(require(ResourceSelection)==F)install.packages('ResourceSelection');require(ResourceSelection)
library(sjPlot)

fuvest <- readxl::read_xlsx("dados/fuvest.xlsx")
View(fuvest)


fuvest <- fuvest |> 
  dplyr::select(
    c(ano,V4,V6,V7,V8,V9,V11,V12,V15,V16,V17,V18,V25,V26)
  )

# Gerar o modelo a partir de variáveis de interesse

mod <- glm(formula = V4 ~ V7 + V8 + V9 + V11 +
           V12 + V15 + V16 + V17 + V18,
           family=binomial(link=probit), data=fuvest)

# Imprimir o modelo
base::summary(mod)

tab_model(mod)

## ---------------------- Criação de um Modelo Nulo para comparação ------------

# OBS: Em modelos de regressão se compara o modelo proposto com o modelo nulo.
# modelo nulo: apenas VD e o intercepto
fit5_rep_null <- glm(formula = reeleito ~ 1, family=binomial(link=logit),
                     data=dat) 


## ---------------------- Tabela 4 - Teste de Hosmer e Lemeshow ----------------
hoslem.test(fit5_rep$y, fitted(fit5_rep), g=10)

## ---------------------- Tabela 5 - Teste Omnibus dos coeficientes  -----------

# OBS: O SPSS entrega essa tabela como default. No R precisamos calculá-lo. 
#     Primeiro, precisamos dos graus de liberdade

fit5_rep$df.null-fit5_rep$df.residual # Gl do modelo nulo, menos Gl do nosso 
# modelo completo.

#     Segundo, deviance do modelo nulo, menos o desvio do modelo completo
#     Essa diferença se distribui em ChiSquare

with(fit5_rep, null.deviance - deviance)
