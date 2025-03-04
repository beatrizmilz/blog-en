library(ggplot2)
library(dados)


pinguins |>
  ggplot() +
  aes(x = massa_corporal) +
  geom_density() +
  stat_function(fun = dnorm,
                color = "red")


media <- mean(pinguins$massa_corporal, na.rm = TRUE)
desvio_padrao <- sd(pinguins$massa_corporal, na.rm = TRUE)

pinguins |>
  ggplot() +
  aes(x = massa_corporal) +
  geom_density() +
  stat_function(fun = ~ dnorm(x = .x, mean = media, sd = desvio_padrao),
                color = "red")

