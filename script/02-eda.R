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

