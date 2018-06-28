bee <- as_tibble(search_fulltext("bee")$data)

query_response <- occurrences(
  fq = c("family:Apidae"),
  qa = "none"
)

machine_bees <- as_tibble(occurrences(
  fq = c("family:Apidae"),
  wkt = "POLYGON((112.27030754089 -10.661994277738, 153.88651847839 -10.445985021005,154.06229972839 -44.27768127617,112.35819816589 -44.309136146459,112.35819816589 -10.748355247628,112.13847160339 -10.877850325879,112.27030754089 -10.661994277738))",
  qa = "none"
)$data)

machine_bees %>%
  count(scientificName) %>%
  arrange(desc(n))


machine_obs %>%
  filter(scientificName == "Amegilla (Zonamegilla) pulchra")


machine_bees %>%
  count(genus)

library(leaflet)
pal <- colorFactor("Dark2", domain = NULL)
leaflet(machine_bees %>% filter(!(genus%in%c("Ctenoplectra","Nomada", "")))) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~ longitude, lat = ~ latitude, color = ~ pal(genus)) %>%
  addLegend(pal = pal, values = ~ genus)


