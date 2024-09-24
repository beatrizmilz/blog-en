library(tidyverse)
library(googlesheets4)
library(knitr)



# Criando um vetor com o link da tabela no Google Sheets
link_google_sheets <- "https://docs.google.com/spreadsheets/d/1D8rh2frRvQ_YSLDrljpiqDCcTJdCjoyMb3rfoEwjPjE/edit?usp=sharing"

# Tabela de leituras
tabela_leituras <- read_sheet(link_google_sheets, sheet = "leituras")

# Tabela de autoria
tabela_autoria <-  read_sheet(link_google_sheets, sheet = "autoria")


leituras_filtradas <- tabela_leituras |>
  filter(ano_leitura == 2024,
         # mes_leitura_fim %in% c("Julho", "Agosto"),
         status_leitura == "completo") |>
  mutate(decada_publicacao_original = (ano_publicacao_original%/%10)*10
           )


dados <- leituras_filtradas |>
  left_join(tabela_autoria, by = "autoria_nome")

n_meses <- length(unique(dados$mes_leitura_fim))

dados |>
  summarise(
    quantidade_livros = n(),
    soma_paginas = sum(n_paginas),
    media_paginas_por_dia = round(soma_paginas / (n_meses*30))
  )


dados |>
  group_by(tipo) |>
  summarise(
    quantidade_livros = n(),
    soma_paginas = sum(n_paginas),
  )

dados |>
  count(editora, sort = TRUE)

dados |>
  count(autoria_nome, sort = TRUE) |>
  kable(col.names = c("Autor", "Quantidade de livros"))

dados |>
  count(decada_publicacao_original) |>
  ggplot() +
  aes(x = decada_publicacao_original, y = n) +
  geom_col()

dados |>
  slice_max(order_by = n_avaliacoes_skoob, n = 10) |>
  mutate(nome = fct_reorder(nome, n_avaliacoes_skoob)) |>
  ggplot() +
  aes(x = n_avaliacoes_skoob, y = nome, fill = autoria_pais) +
  geom_col() +
  theme_light()


dados |>
#  slice_max(order_by = nota_skoob, n = 10) |>
  mutate(nome = fct_reorder(nome, nota_skoob)) |>
  ggplot() +
  aes(x = nota_skoob, y = nome, fill = n_avaliacoes_skoob) +
  geom_col() +
  theme_light()



