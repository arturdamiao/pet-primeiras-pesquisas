library(tidyverse)

# Importando base de dados ------------------------------------------------

fuvest <- readxl::read_xlsx("dados/fuvest_tratado.xlsx")
fuvest <- fuvest |> 
  tidyr::drop_na()


# Visualização --------------------------------------------------

fuvest %>%
  group_by(renda) %>%
  summarise(taxa_matricula = mean(matricula)) %>%
  ggplot(aes(x = renda, y = taxa_matricula)) +
  geom_col(fill = "steelblue") +
  labs(title = "Taxa observada de matrícula por faixa de renda",
       x = "Faixa de Renda (em SM)",
       y = "Proporção de matriculados") +
  theme_classic()
<<<<<<< HEAD

=======
>>>>>>> f8ec306e9ae16d7a0aa90a1b1bb0c7b36eba085b
