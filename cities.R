library(tidyverse)
library(stringr)
library(opencage)
library(countrycode)
library(sp)
library(sf)
library(tigris)

# Downloaded from: "http://data.un.org/Data.aspx?d=POP&f=tableCode%3a240&c=0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18&s=_countryEnglishNameOrderBy:asc,refYear:desc,areaCode:asc&v=1"

undata_raw <- read_csv("UNdata_Export.csv", 
                          col_types = cols(Area = col_character(), 
                                           `Area Code` = col_integer(), City = col_character(), 
                                           `City type` = col_character(), `Code of City` = col_character(), 
                                           `Code of City type` = col_integer(), 
                                           `Country or Area` = col_character(), 
                                           `Country or Area Code` = col_integer(), 
                                           `Record Type` = col_character(), 
                                           `Record Type Code` = col_integer(), 
                                           `Reference Date` = col_skip(), Reliability = col_character(), 
                                           `Reliability Code` = col_integer(), 
                                           Sex = col_character(), `Sex Code` = col_integer(), 
                                           `Source Year` = col_skip(), `Table Code` = col_integer(), 
                                           Year = col_integer())) %>% 
  rename_all(.funs=list(~str_replace_all(.," ", "_"))) %>% 
  rename_all(tolower) %>% 
  rename(country_code = country_or_area_code,
         country = country_or_area,
         city_code = code_of_city) %>% 
  filter(table_code == "240")

undata_clean <- undata_raw %>% 
  filter(sex_code == 0 & value > 0) %>% 
  select(country_code, country, city_code, city, year, value) %>% 
  group_by(country_code, country, city_code, city) %>% 
  mutate(max_year = max(year)) %>% 
  ungroup() %>% 
  filter(year == max_year) %>% 
  group_by(country_code, country, city_code, city) %>% 
  summarise(population = mean(value)) %>% 
  ungroup() %>% 
  mutate(country_code_iso2 = countrycode(country_code, "un", "iso2c")) %>% 
  filter(!is.na(country_code_iso2) & population > 250000)



# # https://happygitwithr.com/github-pat.html
# cat("OPENCAGE_KEY=c42f9452006747fdaf5c96099d2b73b8\n",
#     file=file.path(normalizePath("~/"), ".Renviron"),
#     append=FALSE)
# Sys.getenv("OPENCAGE_KEY")

saved_coords <- read_csv("city_coords.csv")

undata_coords <- undata_clean %>% 
  left_join(saved_coords, by = "city_code")

missing_coords <- undata_coords %>% 
  filter(is.na(lat) | is.na(lon))

found_coords <- missing_coords %>%
  select(-c(lat,lon)) %>% 
  mutate(test = map2(
    .x = city,
    .y = country_code_iso2,
    ~ opencage_forward(
      placename = .x,
      key = opencage_key(),
      countrycode = .y,
      limit = 1,
      no_annotations = TRUE
    )[["results"]])) %>%
  unnest(cols = test) %>%
  select(city_code,lat = geometry.lat, lon = geometry.lng)

if(exists("found_coords")) {
  coords <- bind_rows(saved_coords, found_coords) %>%
    distinct()
  write_csv(coords, "city_coords.csv")
} else coords <- saved_coords



city_lookup <- undata_clean %>% 
  inner_join(coords, by = "city_code")






# CBSAs and CSAs ----------------------------------------------------------

us_cbsas <- core_based_statistical_areas() %>% 
  st_as_sf() %>% 
  rename_all(tolower) %>% 
  mutate(name_short = str_sub(name, start = 1, end = if_else(is.na(str_locate(name, "-")[,1]), str_locate(name, ",")[,1], str_locate(name, "-")[,1]) -1))

us_csas <- combined_statistical_areas() %>%
  st_as_sf() %>% 
  rename_all(tolower) %>% 
  mutate(name_short = str_sub(name, start = 1, end = if_else(is.na(str_locate(name, "-")[,1]), str_locate(name, ",")[,1], str_locate(name, "-")[,1]) -1))


city_lookup_sf <- city_lookup %>% 
  select(city_code, lat, lon) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4269)


cbsa_lookup <- us_cbsas %>% 
  select(cbsafp, cbsa_name = name, cbsa_name_short = name_short)

csa_lookup <- us_csas %>% 
  select(csafp, csa_name = name, csa_name_short = name_short)

## Find what stations match to what locations: exactly and also with a wider net.
city_to_csa <- st_join(city_lookup_sf, csa_lookup)

city_to_cbsa <- st_join(city_to_csa, cbsa_lookup) %>% 
  filter(!is.na(csa_name) | !is.na(cbsa_name))
st_geometry(city_to_cbsa) <- NULL


location_lookup <- city_lookup %>% 
  left_join(city_to_cbsa, by = 'city_code') %>% 
  mutate(type = case_when(!is.na(csafp) ~ "csa",
                          !is.na(cbsafp) ~ "cbsa",
                          TRUE ~ "city"),
         location_id = case_when(!is.na(csafp) ~ paste0("csa",csafp),
                                 !is.na(cbsafp) ~ paste0("cbsa",cbsafp),
                                 TRUE ~ paste0("city", city_code)),
         name = case_when(!is.na(csa_name) ~ csa_name,
                          !is.na(cbsa_name) ~ cbsa_name,
                          TRUE ~ city),
         name_short = case_when(!is.na(csa_name_short) ~ csa_name_short,
                          !is.na(cbsa_name_short) ~ cbsa_name_short,
                          TRUE ~ city))

## Backup
write_csv(location_lookup, "locations_lookup.csv")

parent_location_lookup <- location_lookup %>% 
  select(type, country = country_code_iso2, location_id, name, name_short, population, lat, lon) %>% 
  group_by(type, country, location_id, name, name_short) %>% 
  summarise(population = sum(population),
            lat = mean(lat),
            lon = mean(lon))


ggplot(parent_location_lookup, aes(x=lon, y=lat)) + 
  geom_point(size = 1, alpha = 0.3)



# ## Wider net
# stations_to_locs_buffer <- st_join(locs_buffer, stations_sf) %>% 
#   mutate(match = "buffer") %>% 
#   filter(!is.na(usaf))
# st_geometry(stations_to_locs_buffer) <- NULL
# 
# ## Create a df of all loc to st matches, and have a flag which is exact and which is from buffer
# stations_to_locs <- union_all(
#   stations_to_locs_exact, 
#   anti_join(stations_to_locs_buffer, 
#             stations_to_locs_exact, 
#             by = c("csafp",  "cbsafp", "usaf", "wban")
#   )
# )
# 
# 
# ##Filter missing values
# stations_filtered <- stations_land %>% 
#   semi_join(stations_to_locs, by = c("usaf", "wban"))
# 
# locations_filtered <- locations_import %>% semi_join(stations_to_locs, by = "cbsafp")
# st_geometry(locations_filtered) <- NULL

