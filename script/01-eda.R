# Instalando pacotes --------------

install.packages(c("curl", "googledrive","tidyverse",
                   "haven", "googlesheets4", "flextable","tidyverse"))

# Carregando as bibliotecas ---------------
library("curl")
library("googledrive")
library("tidyverse")
library("haven") # para ler arquivos .sav
library("googlesheets4")
library("flextable")

# Tratando a base de dados ------------------------------------------------

# Lendo a base de dados direto do GDrive
url <- "https://docs.google.com/spreadsheets/d/1lbLgsoh1dFUEB3nr42g9BdmNbCPvA1LNew8dTocNT-I/edit?usp=sharing"

# Salvando num objeto
df_raw <- googlesheets4::read_sheet(url)

df_raw <- janitor::clean_names(df_raw)

# Selecionando variáveis de interesse
df <- df_raw |> 
  dplyr::select(
    c(ano_ingresso,p1_sexo,p3_cor,
      p9_rfm, p8a_cquants,p4a_escm,p4b_escp
      )
  )

# Renomeando as variáveis de interesse
df <- df |> 
  dplyr::rename(
    ano = ano_ingresso,
    sexo = p1_sexo,
    cor = p3_cor,
    rfm = p9_rfm,
    n_moradores = p8a_cquants,
    escm = p4a_escm,
    escp = p4b_escp
  )

# Modificando os tipos de variáveis
df <- df |> 
  dplyr::mutate(
    sexo = case_when(
      sexo == 1 ~ "Masculino",
      TRUE ~ "Feminino"
    ),
    cor = factor(cor, levels = c(1,2,3,4,5),
                 labels = c("Branca","Preta", "Parda", "Amarela", "Indígena")
    ),
    rfm = case_when(
      rfm == 1 ~ 1518,
      rfm == 2 ~ 3036,
      rfm == 3 ~ 7590,
      rfm == 4 ~ 15180,
      rfm == 5 ~ 30360,
      rfm == 6 ~ 45540,
      rfm %in% c(7, 8) ~ NA_real_
    ),
    escp = case_when(
      escp %in% 1:2 ~ 1,
      escp %in% 3:5 ~ 2,
      escp %in% 6:8 ~ 3,
      escp == 9 ~ NA_real_
    ),
    escp = factor(
      escp,
      levels = c(1, 2, 3),
      labels = c("Baixa", "Média", "Alta"),
      ordered = TRUE
    ),
    escm = case_when(
      escm %in% 1:2 ~ 1,
      escm %in% 3:5 ~ 2,
      escm %in% 6:8 ~ 3,
      escm == 9 ~ NA_real_
    ),
    escm = factor(
      escm,
      levels = c(1, 2, 3),
      labels = c("Baixa", "Média", "Alta"),
      ordered = TRUE
    )
  )


# Selecionando o limite superior da faixa indicada pelo indivíduo e dividir 
# pela qtd. de pessoas no domicílio. proxy da renda familiar per capta
# Senkevics e Mello (2019 )

df <- df |> 
  dplyr::mutate(
    renda_pc = dplyr::if_else(n_moradores > 0, rfm / n_moradores, NA_real_)
  )
  

# Salvando os arquivos como Excel na pasta dados
writexl::write_xlsx(df, path = "dados/df.xlsx")



