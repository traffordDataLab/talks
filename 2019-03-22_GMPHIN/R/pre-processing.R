# Speed of animals #
# Source: www.speedofanimals.com

library(tidyverse) ; library(rvest)

habitat <- c("land", "air", "water")
url <- paste0("http://www.speedofanimals.com/", habitat, "?u=m")

list <- map_df(url, function(i) {
  cat(".")
  html <- read_html(i)
  tibble(path = html_attr(html_nodes(html, ".clickable"), 'href'),
         habitat = str_extract(i, "(?<=m/)(.+)(?=\\?)"))
})

url <- paste0("http://www.speedofanimals.com", list$path, "?u=m")
df <- map_df(url, function(i) {
  cat(".")
  html <- read_html(i)
  tibble(name = html_text(html_nodes(html, ".half_width")),
         habitat = pull(filter(list, path == str_extract(i, "(?<=com)(.+)(?=\\?)")), habitat),
         length = html_text(html_nodes(html, ".big_number:nth-child(1)")),
         mass = html_text(html_nodes(html, ".big_number:nth-child(2)")),
         kph = html_text(html_nodes(html, ".big_number:nth-child(3)")))
})

stats <- df %>% 
  mutate(name = str_to_title(name),
         length_cm =
           case_when(
             str_detect(length, "m") ~ parse_number(length)*100,
             TRUE ~ parse_number(length)),
         mass_kg =
           case_when(
             str_detect(mass, "t") ~ parse_number(mass)*1000,
             str_detect(mass, "kg") ~ parse_number(mass),
             str_detect(mass, "g") ~ parse_number(mass)/1000),
         mass_kg = as.numeric(sprintf("%0.2f", mass_kg)),
         speed_kph = parse_number(kph)) %>% 
  select(name, habitat, length_cm, mass_kg, speed_kph)

write_csv(stats, "speed_of_animals.csv")
