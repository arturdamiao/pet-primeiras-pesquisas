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
      p9_rfm, p8a_cquants,p4a_escm,p4b_escp,)
  )


# Selecionando o limite superior da faixa indicada pelo indivíduo e dividir 
# pela qtd. de pessoas no domicílio. proxy da renda familiar per capta
# Senkevics e Mello (2019 )


  
