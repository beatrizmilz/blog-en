library(dados)
library(tidyverse)
options(scipen = 999)

media <- mean(pinguins$massa_corporal, na.rm = TRUE)

desvio_padrao <- sd(pinguins$massa_corporal, na.rm = TRUE)


plot <- pinguins |>
  ggplot() +
  aes(x = massa_corporal) +
  geom_density(size = 1) +
  stat_function(
    fun = ~ dnorm(x = .x, mean = media, sd = desvio_padrao),
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  geom_text(
    x = 5000,
    y = 0.00047,
    label = "Distribuição normal",
    color = "red",
    size = 5
  ) +
    geom_text(
    x = 3100,
    y = 0.00047,
    label = "Dados reais",
    color = "black",
    size = 5
  ) +
  labs(title = "Exemplo de gráfico de densidade",
  subtitle = "Massa corporal dos pinguins do Arquipélago Palmer",
       caption = "Fonte: dados do pacote {dados} e {palmerpenguins}.",
       x = "Massa corporal (g)",
       y = "Densidade"
        ) +
  theme_light()

ggsave("posts/2025-03-04-curva-normal/pt/images/featured.png", plot, width = 10, height = 6, dpi = 300)
