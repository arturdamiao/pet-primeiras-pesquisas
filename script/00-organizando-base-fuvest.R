# Carregar pacotes necessários --------------------------------------------

library(tidyverse)

# Importando base de dados ------------------------------------------------

fuvest <- readxl::read_xlsx("dados/fuvest.xlsx")

# Selecionando variáveis de interesse, conforme dicionário da FUVEST.
fuvest <- fuvest |> 
  dplyr::select(
    c(ano,V4,V6,V7,V8,V9,V11,V12,V15,V16,V17,V18)
  )

# Renomenado variáveis, para facilitar a compreensão
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

# Organizando a base de dados -----------------------------------

# Ao todo, são 11 variáveis. Essas são as variáveis presentes na literatura especializada.

# Modificando os tipos de variáveis
fuvest_df <- fuvest |> 
  dplyr::mutate(
    escolaridade = pmax(as.numeric(esc1),
                        as.numeric(esc2), na.rm = TRUE
    ),
    escolaridade = factor(escolaridade,
                          levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
    ),
    ppi = case_when(
      raca %in% 2:4 ~ 2,
      TRUE ~ 1
    ),
    ppi = factor(ppi,
                 levels = c(1, 2),
                 labels = c("Não PPI", "PPI")
    ),
    raca = factor(raca,
                  levels = c(1, 2, 3, 4, 5),
                  labels = c("Branca", "Preta", "Parda", "Amarela", "Indígena")
    ),
    renda = factor(renda,
                   levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
    ),
    esc1 = factor(esc1,
                  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
    ),
    esc2 = factor(esc2, 
                  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
    ),
    ensino_fund = case_when(
      ensino_fund %in% 1 ~ 1,
      ensino_fund %in% 2:4 ~ 2,
      ensino_fund %in% 5 ~ 3, 
      TRUE ~ 4
    ),
    ensino_fund = factor(ensino_fund,
                         levels = c(1, 2, 3, 4),
                         labels = c("Público", "Particular", "Exterior", "Outro")
    ),
    ensino_med = case_when(
      ensino_med %in% 1 ~ 1,
      ensino_med %in% 2:4 ~ 2,
      ensino_med %in% 5 ~ 3, 
      TRUE ~ 4
    ),
    ensino_med = factor(ensino_med,
                        levels = c(1, 2, 3, 4),
                        labels = c("Público", "Particular", "Exterior", "Outro")
    ),
    tipo_EM = factor(tipo_EM,
                     levels = c(1, 2, 3, 4, 5)
    ),
    cursinho= case_when(
      cursinho %in% 1 ~ 0,
      TRUE ~ 1
    ),
    cursinho = factor(cursinho,
                      levels = c(0, 1),
                      labels = c("Não", "Sim")
    )
  )

# Salvando base de dados ----------------------------------------


writexl::write_xlsx(fuvest_df, path = "dados/fuvest_tratado.xlsx")
