## ----setup, include=FALSE-------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
 fig.path = "assets/img_l5/plots/",
 fig.retina = 2,
 warning = FALSE,
 error = FALSE,
 message = FALSE,
 fig.align = "center",
 dpi = 300,
 dev.args = list(bg = "transparent"),
 cache = TRUE
)

library(showtext)
library(ggplot2)
library(ggdag)
library(tidyverse)
font_add_google("Pangolin", "Pangolin")
showtext_auto()

# theme for chalkboard
theme_chalk <- function() {
 cowplot::theme_cowplot() %+replace%
 theme(
 axis.ticks = element_line(colour = "black", size = 0.25),
 text = element_text(colour = "black"),
 axis.text = element_text(colour = "black", family = "Pangolin", size = 16),
 axis.title = element_text(colour = "black", family = "Pangolin", size = 24),
 axis.line = element_line(colour = "black", size = 1),
 # panel.background = element_rect(colour = NA, fill = "transparent"),
 # plot.background = element_rect(colour = "white", fill = "transparent"),
 legend.position = "bottom",
 legend.title = element_blank(),
 panel.grid.minor = element_blank(),
 # panel.grid.major.x = element_line(colour = "white", size = 0.25),
 # panel.grid.major.y = element_line(colour = "white", size = 0.25),
 legend.text = element_text(size = 24)
 )
}
theme_set(theme_dag(base_size = 16))




## -------------------------------------------------------------------------------------------------------------------------
confounder_triangle() %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point() +
  geom_dag_edges(edge_width = 2) +
  geom_dag_text(size = 50) +
  theme_dag()


## -------------------------------------------------------------------------------------------------------------------------
confounder_triangle() %>%
  control_for("z") %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, col = adjusted, shape = adjusted)) +
  geom_dag_point() +
  geom_dag_edges(aes(edge_alpha = adjusted),
    edge_width = 2
    # region ,
    # start_cap = ggraph::circle(10, "mm"), end_cap = ggraph::circle(10, "mm")
  ) +
  geom_dag_text(size = 50, colour = "black") +
  theme_dag() +
  scale_adjusted() +
  theme(legend.position = "none")


## -------------------------------------------------------------------------------------------------------------------------
#|echo: true
mediation_triangle() %>%
 ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
 geom_dag_point() +
 geom_dag_edges(edge_width=2) +
 geom_dag_text(size = 50) +
 theme_dag()


## -------------------------------------------------------------------------------------------------------------------------
#|echo: true
mediation_triangle() |>
  control_for("m") %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, col = adjusted, shape = adjusted)) +
  geom_dag_point() +
  geom_dag_edges(aes(edge_alpha = adjusted), edge_width = 2) +
  geom_dag_collider_edges(linewidth = 2) +
  geom_dag_point() +
  geom_dag_text(size = 50, colour = "black") +
  theme_dag() +
  scale_adjusted() +
  theme(legend.position = "none")


## -------------------------------------------------------------------------------------------------------------------------
collider_triangle() %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point() +
  geom_dag_edges(edge_width = 2) +
  geom_dag_text(size = 50) +
  theme_dag()


## -------------------------------------------------------------------------------------------------------------------------
collider_triangle() |>
  control_for("m") %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, col = adjusted, shape = adjusted)) +
  geom_dag_point() +
  geom_dag_edges(aes(edge_alpha = adjusted),
    edge_width = 2,
    start_cap = ggraph::circle(20, "mm"), end_cap = ggraph::circle(20, "mm")
  ) +
  geom_dag_collider_edges(linewidth = 2) +
  geom_dag_text(size = 50, colour = "black") +
  theme_dag() +
  scale_adjusted() +
  theme(legend.position = "none")


## -------------------------------------------------------------------------------------------------------------------------
m_bias() %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point() +
  geom_dag_edges(edge_width = 2) +
  geom_dag_text(size = 50) +
  theme_dag()


## -------------------------------------------------------------------------------------------------------------------------
m_bias() |>
  control_for("m") %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, col = adjusted, shape = adjusted)) +
  geom_dag_point() +
  geom_dag_edges(aes(edge_alpha = adjusted), edge_width = 2) +
  geom_dag_collider_edges(linewidth = 2) +
  geom_dag_text(size = 50, colour = "black") +
  theme_dag() +
  scale_adjusted() +
  theme(legend.position = "none")


## -------------------------------------------------------------------------------------------------------------------------
butterfly_bias() %>%
 ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
 geom_dag_point() +
 geom_dag_edges(edge_width = 2) +
 geom_dag_text(size = 50) +
 theme_dag()


## -------------------------------------------------------------------------------------------------------------------------
butterfly_bias() |>
  control_for(c("m", "a")) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, col = adjusted, shape = adjusted)) +
  geom_dag_point() +
  geom_dag_edges(aes(edge_alpha = adjusted), edge_width = 2) +
  geom_dag_collider_edges(linewidth = 2) +
  geom_dag_text(size = 50, colour = "black") +
  theme_dag() +
  scale_adjusted() +
  theme(legend.position = "none")


## -------------------------------------------------------------------------------------------------------------------------
butterfly_bias() |>
  control_for(c("m", "b")) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, col = adjusted, shape = adjusted)) +
  geom_dag_point() +
  geom_dag_edges(aes(edge_alpha = adjusted), edge_width = 2) +
  geom_dag_collider_edges(linewidth = 2) +
  geom_dag_text(size = 50, colour = "black") +
  theme_dag() +
  scale_adjusted() +
  theme(legend.position = "none")


## -------------------------------------------------------------------------------------------------------------------------
butterfly_bias() |>
  control_for(c("m", "b", "a")) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, col = adjusted, shape = adjusted)) +
  geom_dag_point() +
  geom_dag_edges(aes(edge_alpha = adjusted), edge_width = 2) +
  geom_dag_collider_edges(linewidth = 2) +
  geom_dag_text(size = 50, colour = "black") +
  theme_dag() +
  scale_adjusted() +
  theme(legend.position = "none")


## -------------------------------------------------------------------------------------------------------------------------
coords <- tibble::tribble(
 ~name, ~x, ~y,
 "covid", 1, 2,
 "hospitalized", 2, 3,
 "broken_bone", 3, 2,
 "reckless", 4, 1,
 "drugs", 5, 2
)

hosp <- dagify(
 hospitalized ~ broken_bone + covid,
 broken_bone ~ reckless,
 drugs ~ reckless,
 labels = c(
 hospitalized = "Hospitalization",
 broken_bone = "Broken Bone",
 glioma = "Covid19",
 reckless = "Reckless \nBehavior",
 drugs = "Cocaine"
 ),
 coords = coords
)
hosp %>%
 ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
 geom_dag_point() +
 geom_dag_edges(edge_width = 2) +
 geom_dag_label(size = 50, fill = "black", color = "white", nudge_x = 0.2) +
 # geom_dag_text(size = 10, colour = "black") +
 theme_dag()


## -------------------------------------------------------------------------------------------------------------------------
hosp %>%
 control_for(c("hospitalized")) %>%
 ggplot(aes(x = x, y = y, xend = xend, yend = yend, col = adjusted, shape = adjusted)) +
 geom_dag_point() +
 geom_dag_edges(aes(edge_alpha = adjusted), edge_width = 2) +
 geom_dag_collider_edges(linewidth = 2) +
 geom_dag_label(aes(fill=adjusted), size = 50, color = "black", nudge_x = 0.1) +
theme_dag() +
 scale_adjusted() +
 theme(legend.position = "none")


## -------------------------------------------------------------------------------------------------------------------------
#| label: m_comp
#| eval: false
#| echo: true
## my_dag <- dagify(y ~ x + a + b,
##   x ~ a + b,
##   a ~ d,
##   exposure = "x",
##   outcome = "y"
## )
## my_dag %>%
##   ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
##   geom_dag_point() +
##   geom_dag_edges(edge_width = 2) +
##   geom_dag_text(size = 50) +
##   theme_dag()


## -------------------------------------------------------------------------------------------------------------------------
#| label: m_comp
#| eval: true
#| echo: false
my_dag <- dagify(y ~ x + a + b,
  x ~ a + b,
  a ~ d,
  exposure = "x",
  outcome = "y"
)
my_dag %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point() +
  geom_dag_edges(edge_width = 2) +
  geom_dag_text(size = 50) +
  theme_dag()

