library(showtext)
library(ggplot2)
font_add_google("Pangolin", "Pangolin")
showtext_auto()

# theme for chalkboard
theme_whiteboard <- function(...) {
  cowplot::theme_cowplot() %+replace%
    theme(
      axis.ticks = element_line(colour = "black", size = 0.25),
      text = element_text(colour = "black"),
      axis.text = element_text(colour = "black", family = "Pangolin", size = 16),
      axis.title = element_text(colour = "black", family = "Pangolin", size = 24),
      axis.line = element_line(colour = "black", linewidth = 1),
      panel.background = element_rect(colour = NA, fill = "transparent"),
      # plot.background = element_rect(colour = "white", fill = "transparent"),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      # panel.grid.major.x = element_line(colour = "white", size = 0.25),
      # panel.grid.major.y = element_line(colour = "white", size = 0.25),
      legend.text = element_text(size = 24),
      ...
    )
}


# theme for chalkboard
theme_blackboard <- function() {
  cowplot::theme_cowplot() %+replace%
    theme(
      axis.ticks = element_line(colour = "white", size = 0.25),
      text = element_text(colour = "white"),
      axis.text = element_text(colour = "white", family = "Pangolin", size = 16),
      axis.title = element_text(colour = "white", family = "Pangolin", size = 24),
      axis.line = element_line(colour = "white", linewidth = 1),
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

check_model_w <- function(mod){
  pp <- check_model(mod)
  p <- plot(pp, return_list = TRUE)
  out <- p & (theme_whiteboard() +
      theme(
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      )
  )
  out
}
