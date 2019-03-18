# Tidy workflow #
# Source: speedofanimals.com

# load R packages
library(tidyverse) ; library(rvest) ; library(ggplot2) ; library(scales) ; library(plotly)

# import data
html <- read_html("http://www.speedofanimals.com/land?g=t")
df <- tibble(name = html_text(html_nodes(html, ".animal_names strong")),
             kph = html_text(html_nodes(html, ".animal_speeds strong")))

# tidy data
tidy_df <- df %>% 
  mutate(name = str_to_title(name),
         kph = parse_number(kph))

# transform data
transform_df <- tidy_df %>% 
  mutate(mph = kph*0.62137,
         name = fct_reorder(name, mph))

# visualise data
ggplot(transform_df, aes(x = name, y = mph)) +
  geom_segment(aes(x = name, xend = name, y = 0, yend = mph), color = "grey") +
  geom_point(size = 3, color = "#2c7fb8") +
  scale_y_continuous(expand = c(0,0), limits = c(0, max(transform_df$mph) * 1.05)) +
  coord_flip() +
  labs(title = "Top speed of selected land animals",
       subtitle = "Source: speedofanimals.com",
       caption = "@traffordDataLab",
       x = NULL, y = "Miles per hour") +
  theme_minimal() +
  theme(
    plot.margin = unit(rep(30, 4), "pt"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(face = "bold")
  )
ggsave("speed_of_animals.png", dpi = 300, scale = 1)

# model data
model_df <- read_csv("speed_of_animals.csv") %>% 
  filter(habitat == "land")

p <- ggplot(model_df, aes(x = mass_kg, y = speed_kph, 
                          text = paste("Name:", name, "<br>",
                                       "Mass:", mass_kg, "<br>",
                                       "Speed:", speed_kph), group = 1)) +
  geom_point(shape = 1, colour = "#636363", size = 2) +
  geom_smooth(method = "lm", se = FALSE, aes(colour = "Linear regression")) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(colour = "2nd order polynomial")) +
  scale_x_continuous(trans = log_trans(), breaks=c(1,10,100,10000)) +
  scale_y_continuous(trans = log_trans(), breaks=c(1,10,100)) +
  labs(title = "Relationship between body mass and the speed of selected land animals",
       subtitle = "Source: speedofanimals.com ",
       caption = "@traffordDataLab",
       x = "Body mass (kg, log)",
       y = "Top speed (kph, log)") +
  scale_colour_manual(name = NULL, values = c("blue", "red")) +
  theme_minimal() +
  theme(
    plot.margin = unit(rep(30, 4), "pt"),
    plot.title = element_text(size = 16, face = "bold")
  )

ggplotly(p, tooltip = "text") 

