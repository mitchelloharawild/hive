library(ALA4R)
library(tidyverse)
library(lubridate)

as_tibble(ala_fields("occurrence_indexed"))
# cache to a permanent dir
ala_config(
  cache_directory = "data_cache", 
  download_reason_id = 11, # "citizen science"
  warn_on_empty = TRUE
)
env_layers <- c("Precipitation - annual","Temperature - annual max mean")

## 1. a specific species
# numbat
numbat <- as_tibble(search_fulltext("kookaburra")$data)
tax_numbat <- paste("genus", numbat$name[1], sep = ":")

dat_occ <- as_tibble(occurrences(
    taxon = tax_numbat,
    # wkt = wkt,
    fq = c("basis_of_record:HumanObservation"),
    qa = "none"
  )$data) %>% 
  filter(rank %in% c("species", "subspecies"))

library(leaflet)

leaflet(dat_occ) %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~ longitude, lat = ~ latitude)

dat_occ %>% 
  mutate(eventDate = ymd(eventDate)) %>% 
  # filter(year > 1999) %>% 
  ggplot(aes(x = eventDate, y = factor(1))) +
  geom_point()

dat_occ %>% 
  filter(year > 1999) %>% 
  count(year) %>% 
  ggplot(aes(x = year, y = n)) +
  geom_col()

# looking at diversity in Brisbane area
# wkt <- "POLYGON((152.5 -35,152.5 -32,140 -32,140 -35,152.5 -35))"
library(sf)
shape <- st_read("data/1270055001_gccsa_2016_aust_shape/GCCSA_2016_AUST.shp")
brisbane <- shape %>% 
  filter(GCC_NAME16 == "Greater Brisbane")
wkt <- st_as_text(st_geometry(st_convex_hull(brisbane)))

brisbane_lst <- specieslist(
    wkt = wkt, fq = c("basis_of_record:HumanObservation")
  ) %>% 
  # specieslist(wkt = wkt, fq = "state_conservation:*") %>% 
  as_tibble() %>% 
  arrange(desc(occurrenceCount)) %>% 
  select(speciesName, commonName, occurrenceCount, everything())

dat_bris <- as_tibble(occurrences(
    # taxon = tax_numbat,
    wkt = wkt,
    fq = c("basis_of_record:MachineObservation"),
    qa = "none"
  )$data) %>% 
  filter(rank %in% c("species", "subspecies"))

bee1 <- dat_bris %>% 
  count(scientificName) %>% 
  arrange(desc(n)) %>% 
  pull(scientificName)

dat_bee1 <- as_tibble(occurrences(
    taxon = bee1[1],
    # wkt = wkt,
    fq = c("basis_of_record:MachineObservation"),
    qa = "none"
  )$data) %>% 
  filter(rank %in% c("species", "subspecies"))

leaflet(dat_bee1) %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~ longitude, lat = ~ latitude)
