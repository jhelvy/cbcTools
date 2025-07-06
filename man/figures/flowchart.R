library(ggplot2)
library(dplyr)

# Step labels and positions
steps <- tibble::tibble(
  step = c(
    "Generate\nProfiles",
    "Create\nPriors",
    "Generate\nDesign",
    "Inspect\nDesign",
    "Simulate\nChoices",
    "Assess\nPower"
  ),
  function_name = c(
    "cbc_profiles()",
    "cbc_priors()",
    "cbc_design()",
    "cbc_inspect()",
    "cbc_choices()",
    "cbc_power()"
  ),
  x = seq(0, by = 1.6, length.out = 6),
  y = 0
)

# Function to draw a chevron (flat left, pointed right)
make_chevron <- function(x, y, width = 1.8, height = 0.8, point_width = 0.4) {
  data.frame(
    x = c(
      x,
      x + width - point_width,
      x + width,
      x + width - point_width,
      x,
      x + point_width
    ),
    y = c(
      y + height / 2,
      y + height / 2,
      y,
      y - height / 2,
      y - height / 2,
      y
    )
  )
}

# Build chevrons
chevrons <- bind_rows(
  lapply(1:nrow(steps), function(i) {
    df <- make_chevron(steps$x[i], steps$y[i])
    df$step <- steps$step[i]
    df$function_name <- steps$function_name[i]
    df$id <- i
    df
  })
)

# Plot with tighter margins
p <- ggplot() +
  geom_polygon(
    data = chevrons,
    aes(x = x, y = y, group = id),
    fill = "#346DB6",
    linewidth = 0.5,
    alpha = 0.3
  ) +
  geom_text(
    data = steps,
    aes(x = x + 0.9, y = 0, label = step),
    size = 4.2,
    fontface = "bold",
    lineheight = 0.9,
    family = "Fira Sans Condensed"
  ) +
  geom_text(
    data = steps,
    aes(x = x + 0.8, y = -0.6, label = function_name),
    size = 3.5,
    family = "Monaco"
  ) +
  # Set exact limits to crop tightly
  xlim(-0.1, 9.9) +
  ylim(-1.0, 0.6) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA),
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm") # Minimal margins
  )

# Save with tight cropping
ggsave(
  file.path("man", "figures", "flowchart.png"),
  plot = p,
  width = 9,
  height = 1.5,
  dpi = 300,
  bg = "white"
)
