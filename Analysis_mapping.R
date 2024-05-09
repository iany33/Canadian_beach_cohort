
pacman::p_load(
  rio, 
  here,
  Matrix,
  tidyverse, 
  sf,
  tmap
)

# Import forward sortation area shapefile (first 3 postal code digits) from Canada Census

postal_codes <- read_sf("G:/My Drive/Research/Projects-TMU/Beach water illness/CIHR Bridge 2023-2024/Mapping/Canada_forward_sortation_shapefile_2021/lfsa000b21a_e.shp")

postal_codes <- postal_codes |> 
  filter(PRNAME %in% "Ontario") 

postal_codes2 <- postal_codes |> 
  filter((grepl("^[ML]", CFSAUID)))

# Convert survey postal code data to uppercase and store as new dataframe
# Summarize at household level (rather than individual)

data_postal <- data |>
  distinct(house_id, postal_postal_code, residence) |> 
  mutate(postal_code = toupper(postal_postal_code)) |>
  filter(residence %in% "ON") |> 
  group_by(postal_code) |> 
  summarize (Participants = n()) 

# Combine shapefile data with participant counts from survey data

data_postal1 <- data_postal |> 
  full_join(postal_codes, by = c("postal_code" = "CFSAUID")) |> 
  mutate(Participants = replace_na(Participants, 0)) |> 
  drop_na(postal_code, DGUID) |> 
  st_as_sf() 

data_postal2 <- data_postal |> 
  left_join(postal_codes, by = c("postal_code" = "CFSAUID")) |> 
  drop_na(postal_code, DGUID) |> 
  st_as_sf() 

# Map data

tmap_mode("plot")

tm_shape(data_postal1) +
  tm_polygons("Participants") +
  tm_layout(aes.palette = list(seq = "-viridis"))

tm_shape(data_postal2) +
  tm_polygons("Participants") +
  tm_layout(aes.palette = list(seq = "-inferno")) +
  tm_text("postal_code")

# Remove dataframes

remove(data_postal1, data_postal2, postal_codes)





