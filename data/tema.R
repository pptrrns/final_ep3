#-----
# Definir tema
tema <-   theme_ipsum(axis_col = "black") +
  theme(
    plot.title = element_text(size = 24, color = "#1B2128", face = 'bold'),
    plot.subtitle = element_text(size = 14, color = "#747577"),
    plot.caption = element_text(size = 11, color = "#747577", face = "plain"),
    axis.title.x = element_text(size = 13, color = "#1B2128", face = "bold", hjust = .5, margin = margin(t = .5, unit = "cm")),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 13, color = "#1B2128", face = "bold", hjust = .5, margin = margin(r = .5, unit = "cm")),
    axis.text.y = element_blank(),
    legend.title = element_text(size = 13, color = "#1B2128", face = "bold"),
    legend.text = element_text(size = 11, color = "#1B2128"),
    legend.position = "null",
    plot.margin = margin(1, 1, 1, 1, "cm"),
    axis.line = element_line(size = .6),
    panel.grid = element_line(colour = "white"),
    panel.grid.major = element_line(colour = "white"),
    panel.grid.minor = element_line(colour = "white"),
    plot.background = element_rect(color = "white", fill = "white")
  )

tema.postura <-   theme_ipsum(axis_col = "white") +
  theme(
    plot.title = element_text(size = 24, color = "#1B2128", face = 'bold'),
    plot.subtitle = element_text(size = 14, color = "#747577"),
    plot.caption = element_text(size = 11, color = "#747577", face = "plain"),
    axis.title.x = element_text(size = 13, color = "#1B2128", face = "bold", hjust = .5, margin = margin(t = .5, unit = "cm")),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 13, color = "#1B2128", face = "bold", hjust = .5, margin = margin(r = .5, unit = "cm")),
    axis.text.y = element_blank(),
    legend.title = element_text(size = 13, color = "#1B2128", face = "bold"),
    legend.text = element_text(size = 11, color = "#1B2128"),
    legend.position = "null",
    plot.margin = margin(10, 1, 10, 1, "cm"),
    axis.line.x = element_line(size = .6, color = "black"),
    panel.grid = element_line(colour = "white"),
    panel.grid.major = element_line(colour = "white"),
    panel.grid.minor = element_line(colour = "white"),
    plot.background = element_rect(color = "white", fill = "white")
  )