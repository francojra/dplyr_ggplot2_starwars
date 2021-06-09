# Tutorial Jeanne Franco
# dplyr 

library(dplyr) # Manipulação de dados
library(ggplot2) # Gráfico
library(cowplot) # Pacote utilizado para inserir imagens
library(forcats) # Alterar categorias de um fator
library(viridis) # Paleta de cores

data("starwars")
tibble(starwars)
View(starwars)

starwars |>
  select(height, gender) |>
  filter(gender != "NA") |> # gender %in% c("feminine", "masculine")
  group_by(gender) |>
  summarise(media = mean(height, na.rm = TRUE),
            desvio = sd(height, na.rm = T),
            mediana = median(height, na.rm = T),
            minimo = min(height, na.rm = T),
            maximo = max(height, na.rm = T))

star <- starwars |>
  select(mass, name) |>
  filter(name %in% c("Luke Skywalker", "Darth Vader",
         "Leia Organa", "Chewbacca", "Han Solo",
         "Yoda", "Palpatine", "Obi-Wan Kenobi")) |>
  mutate(name = fct_reorder(name, mass)) 

grafico <- ggplot(star, aes(x = name, y = mass, fill = name)) +
  geom_col() +
  scale_fill_viridis(discrete = T, option = "E",
                     name = "Personagens") +
  labs(y = "Massa (kg)", x = "") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))
  grafico

  ggdraw() + 
  draw_plot(grafico) +
  draw_image(image = "yoda.jpg",
             scale = 0.15, x = -0.4, y = - 0.21)
