library(sf)
library(psrcelmer)
library(psrccensus)
library(tidyverse)
library(tidycensus)

# Inputs ------------------------------------------------------------------
generate_mic_buffers <- "no"

race_lookup <- data.frame(variable = c("B03002_001",
                                       "B03002_003",
                                       "B03002_004",
                                       "B03002_005",
                                       "B03002_006",
                                       "B03002_007",
                                       "B03002_008", "B03002_009",
                                       "B03002_012"),
                          grouping = c("Total",
                                       "white",
                                       "Black or African American",
                                       "American Indian and Alaska Native",
                                       "Asian",
                                       "Native Hawaiian and Other Pacific Islander", 
                                       "Other", "Other",
                                       "Hispanic or Latino"))

buffer_ord <- c("MIC Boundary", "1/4 mile buffer", "1/2 mile buffer", "3/4 mile buffer", "1 mile buffer")

mic_ord <- c("Ballard-Interbay", "Duwamish", "Kent MIC", "North Tukwila", "Sumner Pacific",
             "Puget Sound Industrial Center- Bremerton", 
             "Frederickson", "Port of Tacoma",
             "Cascade", "Paine Field / Boeing Everett",
             "All MICs")

census_year <- 2021

# Create MIC Buffers ------------------------------------------------------
if (generate_mic_buffers == "yes") {
  
  print("Loading MIC layer and creating buffers to flag parcels within these boundaries")
  mic_layer <- st_read_elmergeo("micen", project_to_wgs84 = FALSE) |> select(geography="mic")
  mic_qtr <- mic_layer |> st_buffer(dist = 0.25*5280)
  mic_hlf <- mic_layer |> st_buffer(dist = 0.50*5280)
  mic_3qtr <- mic_layer |> st_buffer(dist = 0.75*5280)
  mic_mil <- mic_layer |> st_buffer(dist = 1.00*5280)
  
  print("Loading Parcel data from Elmer to use to scale Census data to Blockgroups")
  parcel_layer <- st_read_elmergeo("parcels_urbansim_2018_pts", project_to_wgs84 = FALSE) |> select("parcel_id")
  
  mic_names <- unique(mic_layer$geography)
  
  mic_parcel_ids = NULL
  for (mic in mic_names) {
    
    # MIC Layer directly
    m <- mic_layer |> filter (geography == mic)
    p <- st_intersection(parcel_layer, m) |> st_drop_geometry() |> mutate(mic_buffer = "MIC Boundary")
    if(is_null(mic_parcel_ids)) {mic_parcel_ids = p} else {mic_parcel_ids = bind_rows(mic_parcel_ids, p)}
    
    # MIC 1/4 mile buffer
    m <- mic_qtr |> filter (geography == mic)
    p <- st_intersection(parcel_layer, m) |> st_drop_geometry() |> mutate(mic_buffer = "1/4 mile buffer")
    mic_parcel_ids = bind_rows(mic_parcel_ids, p)
    
    # MIC 1/2 mile buffer
    m <- mic_hlf |> filter (geography == mic)
    p <- st_intersection(parcel_layer, m) |> st_drop_geometry() |> mutate(mic_buffer = "1/2 mile buffer")
    mic_parcel_ids = bind_rows(mic_parcel_ids, p)
    
    # MIC 3/4 mile buffer
    m <- mic_3qtr |> filter (geography == mic)
    p <- st_intersection(parcel_layer, m) |> st_drop_geometry() |> mutate(mic_buffer = "3/4 mile buffer")
    mic_parcel_ids = bind_rows(mic_parcel_ids, p)
    
    # MIC 1 mile buffer
    m <- mic_mil |> filter (geography == mic)
    p <- st_intersection(parcel_layer, m) |> st_drop_geometry() |> mutate(mic_buffer = "1 mile buffer")
    mic_parcel_ids = bind_rows(mic_parcel_ids, p)
    
    rm(m, p)
    
  }
  
  saveRDS(mic_parcel_ids, "data/mic_parcel_ids.rds")
  
} else {
  
  mic_parcel_ids <- readRDS("data/mic_parcel_ids.rds")
  
}

# Load Parcels with Population and Blockgroup IDs ----------------------
parcel_dims <- get_query(sql = "SELECT parcel_dim_id, parcel_id, block_group_geoid10, block_group_geoid20 from small_areas.parcel_dim WHERE base_year = 2018 AND dummy_type=0")
parcel_pop <- get_query(sql = "SELECT parcel_dim_id, estimate_year, total_pop from ofm.parcelized_saep_facts WHERE ofm_vintage = 2023 AND estimate_year=2022")
parcel_values <- left_join(parcel_dims, parcel_pop, by=c("parcel_dim_id")) |> drop_na()
mic_parcel_ids <- left_join(mic_parcel_ids, parcel_values, by=c("parcel_id")) |> drop_na() |> select(-"parcel_dim_id")

# Calculate Population Share for each MIC and Blockgroup ------------------

# Determine the MIC population for each blockgroup in a MIC buffer from the parcels
mic_blockgroup10_population <- mic_parcel_ids |> group_by(geography, mic_buffer, block_group_geoid10) |> summarise(parcel_pop = sum(total_pop)) |> as_tibble()
mic_blockgroup20_population <- mic_parcel_ids |> group_by(geography, mic_buffer, block_group_geoid20) |> summarise(parcel_pop = sum(total_pop)) |> as_tibble()

# Get list of unique blockgroups for all MIC boundaries
blockgroup_ids_2010 <- unique(mic_blockgroup10_population$block_group_geoid10)
blockgroup_ids_2020 <- unique(mic_blockgroup20_population$block_group_geoid20)

# Get the total population for each blockgroup to use to figure out blockgroup shares
total_blockgroup10_population <- parcel_values |> filter(block_group_geoid10 %in% blockgroup_ids_2010) |> group_by(block_group_geoid10) |> summarise(total_pop = sum(total_pop))|> as_tibble()
total_blockgroup20_population <- parcel_values |> filter(block_group_geoid20 %in% blockgroup_ids_2020) |> group_by(block_group_geoid20) |> summarise(total_pop = sum(total_pop))|> as_tibble()

# Figure out share of each blockgroup in each MIC based on buffer
mic_blockgroup10_population <- left_join(mic_blockgroup10_population, total_blockgroup10_population, by=c("block_group_geoid10")) |> mutate(share = parcel_pop / total_pop) |> mutate(geoid=as.character(block_group_geoid10)) |> drop_na()
mic_blockgroup20_population <- left_join(mic_blockgroup20_population, total_blockgroup20_population, by=c("block_group_geoid20")) |> mutate(share = parcel_pop / total_pop) |> mutate(geoid=as.character(block_group_geoid20)) |> drop_na()

# ACS Population by Race --------------------------------------------------

if (census_year < 2020) {
  
  mic_blockgroup_splits <- mic_blockgroup10_population
  
} else {
  
  mic_blockgroup_splits <- mic_blockgroup20_population
  
}

population_by_race <- get_acs_recs(geography="block group", table.names = "B03002", years = census_year, acs.type = 'acs5') |>
  mutate(label = str_remove_all(label, "Estimate!!Total!!")) |>
  mutate(label = str_remove_all(label, ":")) |>
  mutate(label = str_replace_all(label, "!!", " ")) |>
  mutate(label = str_remove_all(label, "Estimate ")) |>
  mutate(label = str_trim(label, "both")) |>
  select(-"state") |>
  mutate(concept = str_to_title(concept))

population_by_race <- left_join(population_by_race, race_lookup, by=c("variable")) |>
  filter(!(is.na(grouping))) |>
  group_by(GEOID, year, grouping) |>
  summarise(estimate = sum(estimate),  moe = round(moe_sum(moe, estimate), 0)) |>
  as_tibble() |>
  rename(geoid="GEOID") |>
  pivot_wider(names_from = "grouping", values_from = c("estimate", "moe")) 

mic_population_by_race <- left_join(mic_blockgroup_splits, population_by_race, by=c("geoid")) |>
  mutate(`Estimate: American Indian and Alaska Native` = round(`estimate_American Indian and Alaska Native` * share, 0)) |>
  mutate(`Estimate: Asian` = round(`estimate_Asian` * share, 0)) |>
  mutate(`Estimate: Black or African American` = round(`estimate_Black or African American` * share, 0)) |>
  mutate(`Estimate: Native Hawaiian and Other Pacific Islander` = round(`estimate_Native Hawaiian and Other Pacific Islander` * share, 0)) |>
  mutate(`Estimate: Other` = round(`estimate_Other` * share, 0)) |>
  mutate(`Estimate: Hispanic or Latino` = round(`estimate_Hispanic or Latino` * share, 0)) |>
  mutate(`Estimate: white` = round(`estimate_white` * share, 0)) |>
  mutate(`Estimate: Total` = round(`estimate_Total` * share, 0)) |>
  mutate(`MoE: American Indian and Alaska Native` = round(`moe_American Indian and Alaska Native` * share, 0)) |>
  mutate(`MoE: Asian` = round(`moe_Asian` * share, 0)) |>
  mutate(`MoE: Black or African American` = round(`moe_Black or African American` * share, 0)) |>
  mutate(`MoE: Native Hawaiian and Other Pacific Islander` = round(`moe_Native Hawaiian and Other Pacific Islander` * share, 0)) |>
  mutate(`MoE: Other` = round(`moe_Other` * share, 0)) |>
  mutate(`MoE: Hispanic or Latino` = round(`moe_Hispanic or Latino` * share, 0)) |>
  mutate(`MoE: white` = round(`moe_white` * share, 0)) |>
  mutate(`MoE: Total` = round(`moe_Total` * share, 0)) |>
  group_by(geography, mic_buffer, year) |>
  summarise(`Estimate: American Indian and Alaska Native` = sum(`Estimate: American Indian and Alaska Native`), 
            `Estimate: Asian` = sum(`Estimate: Asian`), 
            `Estimate: Black or African American` = sum(`Estimate: Black or African American`), 
            `Estimate: Native Hawaiian and Other Pacific Islander` = sum(`Estimate: Native Hawaiian and Other Pacific Islander`),
            `Estimate: Other` = sum(`Estimate: Other`), 
            `Estimate: Hispanic or Latino` = sum(`Estimate: Hispanic or Latino`), 
            `Estimate: white` = sum(`Estimate: white`), 
            `Estimate: Total` = sum(`Estimate: Total`),
            `MoE: American Indian and Alaska Native` = round(moe_sum(`MoE: American Indian and Alaska Native`, `Estimate: American Indian and Alaska Native`), 0), 
            `MoE: Asian` = round(moe_sum(`MoE: Asian`, `Estimate: Asian`), 0), 
            `MoE: Black or African American` = round(moe_sum(`MoE: Black or African American`, `Estimate: Black or African American`), 0), 
            `MoE: Native Hawaiian and Other Pacific Islander` = round(moe_sum(`MoE: Native Hawaiian and Other Pacific Islander`, `Estimate: Native Hawaiian and Other Pacific Islander`), 0),
            `MoE: Other` = round(moe_sum(`MoE: Other`, `Estimate: Other`), 0), 
            `MoE: Hispanic or Latino` = round(moe_sum(`MoE: Hispanic or Latino`, `Estimate: Hispanic or Latino`), 0),
            `MoE: white` = round(moe_sum(`MoE: white`, `Estimate: white`), 0),
            `MoE: Total` = round(moe_sum(`MoE: Total`, `Estimate: Total`), 0)) |>
  as_tibble()

# Summary tables ----------------------------------------------------------

estimates <- mic_population_by_race |> 
  select("geography", "mic_buffer", "year", contains("Estimate")) |>
  mutate(`Share: American Indian and Alaska Native` = `Estimate: American Indian and Alaska Native` / `Estimate: Total`, 
         `Share: Asian` = `Estimate: Asian` / `Estimate: Total`, 
         `Share: Black or African American` = `Estimate: Black or African American` / `Estimate: Total`, 
         `Share: Native Hawaiian and Other Pacific Islander` = `Estimate: Native Hawaiian and Other Pacific Islander` / `Estimate: Total`,
         `Share: Other` = `Estimate: Other` / `Estimate: Total`, 
         `Share: Hispanic or Latino` = `Estimate: Hispanic or Latino` / `Estimate: Total`, 
         `Share: white` = `Estimate: white` / `Estimate: Total`) |>
  mutate(mic_buffer = factor(mic_buffer, levels=buffer_ord)) |>
  mutate(geography = factor(geography, levels=mic_ord)) |>
  arrange(mic_buffer, geography, year)

all_mics <- mic_population_by_race |> 
  select("mic_buffer", "year", contains("Estimate")) |>
  group_by(mic_buffer, year) |>
  summarise(across(contains("Estimate"), sum)) |>
  as_tibble() |>
  mutate(`Share: American Indian and Alaska Native` = `Estimate: American Indian and Alaska Native` / `Estimate: Total`, 
         `Share: Asian` = `Estimate: Asian` / `Estimate: Total`, 
         `Share: Black or African American` = `Estimate: Black or African American` / `Estimate: Total`, 
         `Share: Native Hawaiian and Other Pacific Islander` = `Estimate: Native Hawaiian and Other Pacific Islander` / `Estimate: Total`,
         `Share: Other` = `Estimate: Other` / `Estimate: Total`, 
         `Share: Hispanic or Latino` = `Estimate: Hispanic or Latino` / `Estimate: Total`, 
         `Share: white` = `Estimate: white` / `Estimate: Total`) |>
  mutate(mic_buffer = factor(mic_buffer, levels=buffer_ord)) |>
  arrange(mic_buffer, year) |>
  mutate(geography = "All MICs")

estimates <- bind_rows(estimates, all_mics) |> 
  mutate(mic_buffer = factor(mic_buffer, levels=buffer_ord)) |>
  mutate(geography = factor(geography, levels=mic_ord)) |>
  arrange(mic_buffer, geography, year)

write_csv(estimates, paste0("data/population_by_race_", census_year, ".csv"))
