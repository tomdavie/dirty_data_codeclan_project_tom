library(tidyverse)
library(here)
library(readxl)
library(janitor)

here::here()

#Viewing all excel sheet names
excel_sheets(here("raw_data/seabirds.xls"))

# Loading in two useful sheets
bird_data_by_record_id <- read_excel(here("raw_data/seabirds.xls"), 2)
ship_data_by_record_id <- read_excel(here("raw_data/seabirds.xls"), 1) 
ship_data_codes <- read_excel(here("raw_data/seabirds.xls"), 3)
bird_data_codes <- read_excel(here("raw_data/seabirds.xls"), 4)

# Clean column names
bird_data_by_record_id <- clean_names(bird_data_by_record_id)
ship_data_by_record_id <- clean_names(ship_data_by_record_id)

# Selecting the columns required to answer the analysis questions
bird_data_by_record_id_clean <- bird_data_by_record_id %>% 
  select(record:species_abbreviation, count)
ship_data_by_record_id_clean <- ship_data_by_record_id %>% 
  select(record_id:long)

# Joining the two tables 
bird_ship_data_joined <- bird_data_by_record_id_clean %>% 
  left_join(ship_data_by_record_id_clean, by = "record_id")

# Remove all NAs in 'count' as without a number here there was no sighting!
bird_ship_data_joined <- bird_ship_data_joined %>% 
  na.omit(count)

# Refining species abbreviation to only contain species

bird_ship_data_joined <- bird_ship_data_joined %>% 
  mutate(species_abbreviation = str_extract(species_abbreviation, "[A-Z]+"))

bird_ship_data_jointed bird_ship_data_joined %>% 
  mutate(species_common_name_taxon_age_sex_plumage_phase = 
           str_remove(species_common_name_taxon_age_sex_plumage_phase, "[A-Z]+$"))

write_csv(bird_ship_data_joined, "clean_data/bird_ship_data_joined.csv")
