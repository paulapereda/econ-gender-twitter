estilo_paula <- function() {
  theme_minimal(base_family = "Palatino",
                base_size = 16) +
    theme(axis.line.y = element_line(),
          axis.line.x = element_line(),
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(), 
          plot.title = element_text(hjust = .5, 
                                    family = "Palatino-Bold"),
          plot.subtitle = element_text(hjust = .5),
          plot.caption = element_text(family = "Palatino-Light", hjust = 1, 
                                      margin = margin(t = 10)),
          panel.grid.major.y = element_line(),
          legend.position = "bottom")
}

theme_set(estilo_paula())
update_geom_defaults("text", list(family = theme_get()$text$family))
        

mauve <- "b5838d"
coral <- "ffb4a2"