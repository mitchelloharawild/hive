library(tidyverse)

fishes <- read_csv("data/Fishes.csv.zip")
View(colnames(fishes))

fishes_sub <- fishes %>% dplyr::select(id = `Record ID`,
                                data_resource = `Data Resource ID`,
                                scientificName,
                                taxon_concept_lsid = `taxonConceptID`,
                                taxon_rank = taxonRank,
                                kingdom, phylum, class, order, family, genus,
                                decimalLatitude,decimalLongitude, geodeticDatum,
                                sex, aus_conservation = austConservation.p,
                                state_conservation = stateConservation.p,
                                state = `Australian States and Territories`,
                                eventDate, maximumDepthInMeters, minimumDepthInMeters,
                                lifeStage, basisOfRecord)
