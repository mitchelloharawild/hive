library(ALA4R)
library(tidyverse)
library(lubridate)

# cache to a permanent dir
ala_config(
  cache_directory = "data_cache",
  download_reason_id = 11, # "citizen science"
  warn_on_empty = TRUE
)

bee <- as_tibble(search_fulltext("Sunflo")$data)

# sunflower_obs <- as_tibble(occurrences(
#   fq = c("family:Asteraceae"),
#   wkt = "POLYGON((112.27030754089 -10.661994277738, 153.88651847839 -10.445985021005,154.06229972839 -44.27768127617,112.35819816589 -44.309136146459,112.35819816589 -10.748355247628,112.13847160339 -10.877850325879,112.27030754089 -10.661994277738))",
#   qa = "none"
# )$data)


bee_obs <- as_tibble(occurrences(
  fq = c("family:Apidae"),
  wkt = "POLYGON((112.27030754089 -10.661994277738, 153.88651847839 -10.445985021005,154.06229972839 -44.27768127617,112.35819816589 -44.309136146459,112.35819816589 -10.748355247628,112.13847160339 -10.877850325879,112.27030754089 -10.661994277738))",
  qa = "none"
)$data)


bee_obs %>%
  count(scientificName) %>%
  arrange(desc(n))

bee_obs %>%
  count(genus)

bee_obs$year %>% is.na %>% mean

library(leaflet)
pal <- colorFactor("Dark2", domain = NULL)
leaflet(bee_obs %>% filter(!(genus%in%c("Ctenoplectra","Nomada", "")))) %>%
  addTiles() %>%
  clearControls() %>%
  addCircleMarkers(lng = ~ longitude, lat = ~ latitude, color = ~ pal(genus)) %>%
  addLegend(pal = pal, values = ~ genus)


# Hmm, interesting. Seasonal effect for Aussie (?) bees!
bee_obs %>% filter(genus != "Apis") %>% count(month, state)  %>% ggplot(aes(x=month, y=n)) + geom_col() + facet_wrap(~ state)
