library(sf)
hydro_summer_20203 <- readRDS(file = "data/hydro_summer_20203.RDS")
hydro_summer_11502 <- readRDS(file = "./data/hydro_summer_11502.RDS")
hydro_summer_10304 <- readRDS(file = "./data/hydro_summer_10304.RDS")
hydro_winter_20203 <- readRDS(file = "./data/hydro_winter_20203.RDS")
hydro_winter_11502 <- readRDS(file = "./data/hydro_winter_11502.RDS")
hydro_winter_10304 <- readRDS(file = "./data/hydro_winter_10304.RDS")
hydro_summer_10304_kbe <- readRDS(file = "data/hydro_summer_10304_kbe.RDS")
hydro_winter_10304_kbe <- readRDS(file = "data/hydro_winter_10304_kbe.RDS")
hydro_summer_11502_kbe <- readRDS(file = "data/hydro_summer_11502_kbe.RDS")
hydro_winter_11502_kbe <- readRDS(file = "data/hydro_winter_11502_kbe.RDS")
hydro_summer_20203_kbe <- readRDS(file = "data/hydro_summer_20203_kbe.RDS")
hydro_winter_20203_kbe <- readRDS(file = "data/hydro_winter_20203_kbe.RDS")

hydro_total <- readRDS(file = "./data/hydro_total.RDS")
pegel_prop <- readRDS(file = "./data/pegel_prop.RDS")
hydro_bavaria <- read_sf("./data/Geo-Daten_Uebersicht/shapefile/EZG_OHNE_Reservoir_UTM32.shp")

# Quelle Data https://hub.arcgis.com/datasets/esri-de-content::bundesl%C3%A4nder-2021-mit-einwohnerzahl/explore?location=51.947250%2C15.358806%2C5.85&showTable=true
admin_bavaria <- read_sf("./added_data/LAN_ew_21.shp") %>% filter(GEN %in% c("Bayern", "Bayern (Bodensee)"))
admin_bavaria <- st_as_sf(admin_bavaria, crs = 4326)
pegel_prop_sf <- st_as_sf(pegel_prop, coords = c("longitude", "latitude"), crs = 4326) # crs Code for WGS 84

pegel_prop_sf$ID <- as.factor(pegel_prop_sf$ID)

# https://geoportal.bafg.de/CSWView/od.xhtml
waterways <- read_sf("./added_data/germany-waterways-shape/waterways.shp")

waterways_three <- waterways %>%
    filter(name %in% c("Isar",
                       "Donau",
                       "Fränkische Saale",
                       "Main",
                       "Iller"))

# Add Pegel props
hydro_bavaria_20203 <- hydro_bavaria %>% filter(gridcode == 20203) %>%
    left_join(pegel_prop, by = c("gridcode" = "ID"))
hydro_bavaria_11502 <- hydro_bavaria %>% filter(gridcode == 11502)%>%
    left_join(pegel_prop, by = c("gridcode" = "ID"))
hydro_bavaria_10304 <- hydro_bavaria %>% filter(gridcode == 10304)%>%
    left_join(pegel_prop, by = c("gridcode" = "ID"))
hydro_bavaria_three <- hydro_bavaria %>% filter(gridcode %in% c(10304, 11502, 20203))%>%
    left_join(pegel_prop, by = c("gridcode" = "ID"))


# Descriptive Data
# By year
table_yearly_avg_min_groundwaterdepth_yearly <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_groundwaterdepth_yearly.RDS")
table_yearly_avg_max_groundwaterdepth_yearly <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_groundwaterdepth_yearly.RDS")
table_yearly_avg_min_soilwater_yearly <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_soilwater_yearly.RDS")
table_yearly_avg_max_soilwater_yearly <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_soilwater_yearly.RDS")
table_yearly_avg_min_snowstorage_yearly <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_snowstorage_yearly.RDS")
table_yearly_avg_max_snowstorage_yearly <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_snowstorage_yearly.RDS")
table_yearly_avg_min_airtmp_yearly <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_airtmp_yearly.RDS")
table_yearly_avg_max_airtmp_yearly <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_airtmp_yearly.RDS")
table_yearly_avg_max_precip_yearly <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_precip_yearly.RDS")
table_yearly_avg_min_precip_yearly <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_precip_yearly.RDS")
table_yearly_avg_max_glorad_yearly <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_glorad_yearly.RDS")
table_yearly_avg_min_glorad_yearly <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_glorad_yearly.RDS")
table_yearly_avg_max_relhum_yearly <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_relhum_yearly.RDS")
table_yearly_avg_min_relhum_yearly <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_relhum_yearly.RDS")
table_yearly_avg_max_infiltration_yearly <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_infiltration_yearly.RDS")
table_yearly_avg_min_infiltration_yearly <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_infiltration_yearly.RDS")

# by Hydro year
# Load tables
table_yearly_avg_min_groundwaterdepth <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_groundwaterdepth.RDS")
table_yearly_avg_max_groundwaterdepth <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_groundwaterdepth.RDS")
table_yearly_avg_min_soilwater <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_soilwater.RDS")
table_yearly_avg_max_soilwater <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_soilwater.RDS")
table_yearly_avg_min_snowstorage <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_snowstorage.RDS")
table_yearly_avg_max_snowstorage <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_snowstorage.RDS")
table_yearly_avg_min_airtmp <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_airtmp.RDS")
table_yearly_avg_max_airtmp <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_airtmp.RDS")
table_yearly_avg_max_precip <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_precip.RDS")
table_yearly_avg_min_precip <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_precip.RDS")
table_yearly_avg_max_glorad <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_glorad.RDS")
table_yearly_avg_min_glorad <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_glorad.RDS")
table_yearly_avg_max_relhum <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_relhum.RDS")
table_yearly_avg_min_relhum <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_relhum.RDS")
table_yearly_avg_max_infiltration <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_infiltration.RDS")
table_yearly_avg_min_infiltration <-
  readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_infiltration.RDS")
# QPR Data
qpr_hydro_summer_10304 <- readRDS("./added_data/tables/extreme_values/quantile_percents_ranges_hydro_summer_10304.RDS")
qpr_hydro_summer_11502 <- readRDS("./added_data/tables/extreme_values/quantile_percents_ranges_hydro_summer_11502.RDS")
qpr_hydro_summer_20203 <- readRDS("./added_data/tables/extreme_values/quantile_percents_ranges_hydro_summer_20203.RDS")

qpr_hydro_winter_10304 <- readRDS("./added_data/tables/extreme_values/quantile_percents_ranges_hydro_winter_10304.RDS")
qpr_hydro_winter_11502 <- readRDS("./added_data/tables/extreme_values/quantile_percents_ranges_hydro_winter_11502.RDS")
qpr_hydro_winter_20203 <- readRDS("./added_data/tables/extreme_values/quantile_percents_ranges_hydro_winter_20203.RDS")

# Full Spline Model load ----
# Winter
## Full Models
load(file = "./added_data/models/gam_all_winter_10304.Rdata")
load(file = "./added_data/models/gam_all_winter_11502.Rdata")
load(file = "./added_data/models/gam_all_winter_20203.Rdata")

## Selected 1 ----
load(file = "./added_data/models/gam_uni_selected_winter_10304.Rdata")
load(file = "./added_data/models/gam_uni_selected_winter_11502.Rdata")
load(file = "./added_data/models/gam_uni_selected_winter_20203.Rdata")

## Selected 2 ----
load(file = "./added_data/models/gam2_uni_selected_winter_10304.Rdata")
load(file = "./added_data/models/gam2_uni_selected_winter_11502.Rdata")
load(file = "./added_data/models/gam2_uni_selected_winter_20203.Rdata")

## Interactions ----
load(file = "./added_data/models/gam_uni_selected_interac_winter_10304.Rdata")
load(file = "./added_data/models/gam_uni_selected_interac_winter_11502.Rdata")
load(file = "./added_data/models/gam_uni_selected_interac_winter_20203.Rdata")

# Summer
## Full Models
load(file = "./added_data/models/gam_all_summer_10304.Rdata")
load(file = "./added_data/models/gam_all_summer_11502.Rdata")
load(file = "./added_data/models/gam_all_summer_20203.Rdata")

## Selected 1 ----
load(file = "./added_data/models/gam_uni_selected_summer_10304.Rdata")
load(file = "./added_data/models/gam_uni_selected_summer_11502.Rdata")
load(file = "./added_data/models/gam_uni_selected_summer_20203.Rdata")

## Selected 2 ----
load(file = "./added_data/models/gam2_uni_selected_summer_10304.Rdata")
load(file = "./added_data/models/gam2_uni_selected_summer_11502.Rdata")
load(file = "./added_data/models/gam2_uni_selected_summer_20203.Rdata")

## Interactions ----
load(file = "./added_data/models/gam_uni_selected_interac_summer_10304.Rdata")
load(file = "./added_data/models/gam_uni_selected_interac_summer_11502.Rdata")
load(file = "./added_data/models/gam_uni_selected_interac_summer_20203.Rdata")

# LE Model load ----
# Winter
## Full Models
load(file = "./added_data/le_models/le_gam_all_winter_10304.Rdata")
load(file = "./added_data/le_models/le_gam_all_winter_11502.Rdata")
load(file = "./added_data/le_models/le_gam_all_winter_20203.Rdata")

## Selected 1 ----
load(file = "./added_data/le_models/le_gam_uni_selected_winter_10304.Rdata")
load(file = "./added_data/le_models/le_gam_uni_selected_winter_11502.Rdata")
load(file = "./added_data/le_models/le_gam_uni_selected_winter_20203.Rdata")

## Selected 2 ----
load(file = "./added_data/le_models/le_gam2_uni_selected_winter_10304.Rdata")
load(file = "./added_data/le_models/le_gam2_uni_selected_winter_11502.Rdata")
load(file = "./added_data/le_models/le_gam2_uni_selected_winter_20203.Rdata")

## Interactions ----
load(file = "./added_data/le_models/le_gam_uni_selected_interac_winter_10304.Rdata")
load(file = "./added_data/le_models/le_gam_uni_selected_interac_winter_11502.Rdata")
load(file = "./added_data/le_models/le_gam_uni_selected_interac_winter_20203.Rdata")

# Summer
## Full Models
load(file = "./added_data/le_models/le_gam_all_summer_10304.Rdata")
load(file = "./added_data/le_models/le_gam_all_summer_11502.Rdata")
load(file = "./added_data/le_models/le_gam_all_summer_20203.Rdata")

## Selected 1 ----
load(file = "./added_data/le_models/le_gam_uni_selected_summer_10304.Rdata")
load(file = "./added_data/le_models/le_gam_uni_selected_summer_11502.Rdata")
load(file = "./added_data/le_models/le_gam_uni_selected_summer_20203.Rdata")

## Selected 2 ----
load(file = "./added_data/le_models/le_gam2_uni_selected_summer_10304.Rdata")
load(file = "./added_data/le_models/le_gam2_uni_selected_summer_11502.Rdata")
load(file = "./added_data/le_models/le_gam2_uni_selected_summer_20203.Rdata")

## Interactions ----
load(file = "./added_data/le_models/le_gam_uni_selected_interac_summer_10304.Rdata")
load(file = "./added_data/le_models/le_gam_uni_selected_interac_summer_11502.Rdata")
load(file = "./added_data/le_models/le_gam_uni_selected_interac_summer_20203.Rdata")


# Correlation Plots ----
## Summer ----
subset_hydro_summer_10304_kbe <- readRDS("./data/corrplots/subset_hydro_summer_10304_kbe.RDS")
subset_hydro_summer_11502_kbe <- readRDS("./data/corrplots/subset_hydro_summer_11502_kbe.RDS")
subset_hydro_summer_20203_kbe <- readRDS("./data/corrplots/subset_hydro_summer_20203_kbe.RDS")

subset_hydro_winter_10304_kbe <- readRDS("./data/corrplots/subset_hydro_winter_10304_kbe.RDS")
subset_hydro_winter_11502_kbe <- readRDS("./data/corrplots/subset_hydro_winter_11502_kbe.RDS")
subset_hydro_winter_20203_kbe <- readRDS("./data/corrplots/subset_hydro_winter_20203_kbe.RDS")
# Setup test set
hydro_winter_11502_test <- hydro_winter_11502 %>% filter(member != "kbe")
hydro_winter_11502_test$lowlevel <- as.logical(hydro_winter_11502_test$lowlevel)

hydro_summer_11502_test <- hydro_summer_11502 %>% filter(member != "kbe")
hydro_summer_11502_test$lowlevel <- as.logical(hydro_summer_11502_test$lowlevel)

hydro_winter_10304_test <- hydro_winter_10304 %>% filter(member != "kbe")
hydro_winter_10304_test$lowlevel <- as.logical(hydro_winter_10304_test$lowlevel)

hydro_summer_10304_test <- hydro_summer_10304 %>% filter(member != "kbe")
hydro_summer_10304_test$lowlevel <- as.logical(hydro_summer_10304_test$lowlevel)

hydro_winter_20203_test <- hydro_winter_20203 %>% filter(member != "kbe")
hydro_winter_20203_test$lowlevel <- as.logical(hydro_winter_20203_test$lowlevel)

hydro_summer_20203_test <- hydro_summer_20203 %>% filter(member != "kbe")
hydro_summer_20203_test$lowlevel <- as.logical(hydro_summer_20203_test$lowlevel)