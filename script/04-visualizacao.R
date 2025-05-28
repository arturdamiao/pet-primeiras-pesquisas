library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(flextable)

# Importando base de dados e organizando o ambiente -----------------------------------
flextable::set_flextable_defaults(
  font.family = "Serif",
  big.mark = ".",
  decimal.mark = ","
#  font.size = 12,
)

fuvest <- readxl::read_xlsx("dados/fuvest_tratado.xlsx")
fuvest <- fuvest |> 
  tidyr::drop_na()

# Tabelas -------------------------------------------------------

t1 <- fuvest |> 
  count(matricula) |> 
  mutate(percentual = round (100 * n / sum(n), 1)) |> 
  flextable::flextable() |> 
  flextable::set_header_labels(
    matricula = "Matrícula",
    n = "N°", percentual = "%") |> 
  flextable::set_caption("Porcentagem de convocados para matrícula\n (n = 3914)")

t2 <- fuvest |> 
  count(matricula, sexo) |> 
  mutate(
    sexo = recode(sexo, 
                  `1` = "Masculino", 
                  `2` = "Feminino"),
    percentual = round(100 * n / sum(n), 1)
  ) |> 
  flextable::flextable() |> 
  flextable::set_header_labels(
    matricula = "Matrícula",
    sexo = "Sexo",
    n = "N°", 
    percentual = "%") |> 
  flextable::set_caption("Porcentagem de convocados para matrícula por sexo\n (n = 3914)")

# Visualização --------------------------------------------------

# Renda e Tipo de Ensino Médio
p1 <- ggplot(data = fuvest, aes(x = as.numeric(renda), group = as.numeric(tipo_EM),
                                fill=tipo_EM)) + 
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum() +
  theme_classic() +
  xlab("Faixa de renda (em SM)") +
  ylab("Densidade") + 
  labs(fill = "Tipo de\n Ensino Médio")

# Taxa de Matrícula e Renda
p2 <- fuvest %>%
  group_by(renda) %>%
  summarise(taxa_matricula = mean(matricula)) %>%
  ggplot(aes(x = renda, y = taxa_matricula)) +
  geom_col(fill = "steelblue") +
  labs(title = "Taxa observada de matrícula por faixa de renda",
       x = "Faixa de Renda (em SM)",
       y = "Proporção de matriculados") +
  theme_classic()

# Salvando gráficos e tabelas

ggsave("images/grafico_renda_em.png",
       plot = p1, dpi = 300, width = 8, height = 6)
ggsave("images/grafico_matricula_renda.png",
       plot = p2, dpi = 300, width = 8, height = 6)

save_as_image(t1, path = "images/tabela_matricula.png")
save_as_image(t2, path = "images/tabela_matricula_genero.png")
