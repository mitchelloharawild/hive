library(tidyverse)
library(rgbif)
library(ggbeeswarm)

data <- readr::read_csv("shiny/bee_data.csv") %>%
  filter(!(genus%in%c("Ctenoplectra","Nomada", "")),
         !is.na(genus))

apikey <- Sys.getenv("g_elevation_api")
ele_data <- elevation(
  latitude = data$latitude, longitude = data$longitude,
  key = apikey
)
write_csv(ele_data, "shiny/elevation_data.csv")

ele_data_dist <- group_by(ele_data, latitude, longitude) %>%
  summarise(elevation = mean(elevation))
data_a <- data %>%
  left_join(ele_data_dist, by = c("latitude", "longitude"))

data_a %>%
  # filter(year == 2017) %>%
  ggplot(aes(x = genus, y = elevation)) +
  geom_violin()
