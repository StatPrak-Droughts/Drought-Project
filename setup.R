renv::restore()
# Import packages
library(tidyverse)
library(mgcv)
library(lubridate)
library(timetk)
library(RcppRoll)
library(dLagM)
library(dlnm)
library(zoo)
theme_set(theme_gray())
library(knitr)
library(kableExtra)
library(sf)
library(tmap)
library(gridExtra)
library(lubridate)
library(timetk)
library(RcppRoll)
library(dLagM)
library(dlnm)
library(zoo)
theme_set(theme_gray())
library(js)
library(sjPlot)
library(verification)
library(ROCR)
library(pROC)

## Data import
pegel_eigen <- read_csv2(file = "data/Geo-Daten_Uebersicht/Eigenschaften_Pegel.csv")
pegel_eigen <- read.csv2("data/Geo-Daten_Uebersicht/Eigenschaften_Pegel.csv")
hydro_summer <- read.table("data/Zielvariablen/hydro_summer.txt")
hydro_winter <- read.table("data/Zielvariablen/hydro_winter.txt")
#data_Beobachtung <- read.table("data/Zielvariablen/gerinneabfluss_modelliert_mit_beobachtungsdaten.txt", header = TRUE)

pegel_ids <- c("20203", "11502", "10304")
names(pegel_ids) <- c("PEG-Fraenkische-Saale-Salz", "PEG-Iller-Kempten", "PEG-Isar-Mittenwald")
pegel_ids_list <- list("20203", "11502", "10304")
names(pegel_ids_list) <- c("Fraenkische Saale Salz", "Iller Kempten", "Isar Mittenwald")

## Overview
hydro_summer %>% count(member, sort = TRUE)
hydro_winter %>% count(member, sort = TRUE)
head(pegel_eigen)
head(hydro_summer)
head(hydro_winter)
unique(hydro_summer$slope)


## Data Manipulation

# Remove index row for pegel_eigen
pegel_eigen  <- pegel_eigen[-1]

# Filter for our 3 waterlevel
pegel_prop <- pegel_eigen %>% filter(ID %in% pegel_ids)

# Change name_waterlevel
hydro_summer$name_waterlevel <- sub('.', '', hydro_summer$name_waterlevel)
hydro_winter$name_waterlevel <- sub('.', '', hydro_winter$name_waterlevel)

hydro_summer$name_waterlevel <- as.factor(hydro_summer$name_waterlevel)
hydro_winter$name_waterlevel <- as.factor(hydro_winter$name_waterlevel)

hydro_summer$waterlevel <- hydro_summer$name_waterlevel
levels(hydro_summer$waterlevel) <- pegel_ids_list
hydro_winter$waterlevel <- hydro_winter$name_waterlevel
levels(hydro_winter$waterlevel) <- pegel_ids_list

# Adjust data types
hydro_summer$date <- as.Date(hydro_summer$date)
hydro_winter$date <- as.Date(hydro_winter$date)

hydro_summer$member <- as.factor(hydro_summer$member)
hydro_winter$member <- as.factor(hydro_winter$member)

hydro_summer$river <- as.factor(hydro_summer$river)
hydro_winter$river <- as.factor(hydro_winter$river)

hydro_summer$landuse <- as.factor(hydro_summer$landuse)
hydro_winter$landuse <- as.factor(hydro_winter$landuse)

hydro_summer$exposition <- as.factor(hydro_summer$exposition)
hydro_winter$exposition <- as.factor(hydro_winter$exposition)

hydro_summer$lowlevel_1d <- as.factor(hydro_summer$lowlevel_1d)
levels(hydro_summer$lowlevel_1d) <- c(FALSE, TRUE)
hydro_winter$lowlevel_1d <- as.factor(hydro_winter$lowlevel_1d)
levels(hydro_winter$lowlevel_1d) <- c(FALSE, TRUE)
hydro_summer$lowlevel <- as.factor(hydro_summer$lowlevel)
levels(hydro_summer$lowlevel) <- c(FALSE, TRUE)
hydro_winter$lowlevel <- as.factor(hydro_winter$lowlevel)
levels(hydro_winter$lowlevel) <- c(FALSE, TRUE)

hydro_summer$hydro_year <- as.factor(hydro_summer$hydro_year)
hydro_winter$hydro_year <- as.factor(hydro_winter$hydro_year)

# Create hydro total
hydro_total <- rbind(hydro_summer, hydro_winter)


## Adjustments
# Add rescaled soilwater
hydro_summer$soilwater100 <- hydro_summer$soilwater * 100
hydro_winter$soilwater100 <- hydro_winter$soilwater * 100

## add column max_precip
# winter
max_precip <- as.numeric(rep(NA, length = nrow(hydro_winter)))
for(k in 7:length(max_precip)) {
  if (all(hydro_winter$member[(k - 6):(k - 1)] == hydro_winter$member[k]) &&
      all(hydro_winter$name_waterlevel[(k - 6):(k - 1)] == hydro_winter$name_waterlevel[k])) {
    if (hydro_winter$MM[k] == 11) {
      if (all(hydro_winter$MM[(k - 6):(k - 1)] == 11)) {
        max_precip[k] <- max(hydro_winter$precip[(k - 6):k])
      }
    } else {
      max_precip[k] <- max(hydro_winter$precip[(k - 6):k])
    }
  }
}
hydro_winter <- cbind(hydro_winter, max_precip)

# summer
max_precip <- as.numeric(rep(NA, length = nrow(hydro_summer)))
for(k in 7:length(max_precip)) {
  if (all(hydro_summer$member[(k - 6):(k - 1)] == hydro_summer$member[k]) &&
      all(hydro_summer$name_waterlevel[(k - 6):(k - 1)] == hydro_summer$name_waterlevel[k])) {
    if (hydro_summer$MM[k] == 5) {
      if (all(hydro_summer$MM[(k - 6):(k - 1)] == 5)) {
        max_precip[k] <- max(hydro_summer$precip[(k - 6):k])
      }
    } else {
      max_precip[k] <- max(hydro_summer$precip[(k - 6):k])
    }
  }
}
hydro_summer <- cbind(hydro_summer, max_precip)

##Rescaling
hydro_winter$max_precip_scaled <- hydro_winter$max_precip 
hydro_summer$max_precip_scaled <- hydro_summer$max_precip 


# add column snowstorage_diff
# winter
snowstorage_diff <- as.numeric(rep(NA, length = nrow(hydro_winter)))
for(k in 2:length(snowstorage_diff)) {
  if (hydro_winter$member[k - 1] == hydro_winter$member[k] &&
      hydro_winter$name_waterlevel[k - 1] == hydro_winter$name_waterlevel[k]) {
    if(!any(is.na(hydro_winter$snowstorage[(k - 1):k]))) {
      if (hydro_winter$MM[k] == 11) {
        if (hydro_winter$MM[k - 1] == 11) {
          snowstorage_diff[k] <- hydro_winter$snowstorage[k] - hydro_winter$snowstorage[k - 1]
        }
      } else {
        snowstorage_diff[k] <- hydro_winter$snowstorage[k] - hydro_winter$snowstorage[k - 1]
      }
    }
  }
}
hydro_winter <- cbind(hydro_winter, snowstorage_diff) 

# summer
snowstorage_diff <- as.numeric(rep(NA, length = nrow(hydro_summer)))
for(k in 2:length(snowstorage_diff)) {
  if (hydro_summer$member[k - 1] == hydro_summer$member[k] &&
      hydro_summer$name_waterlevel[k - 1] == hydro_summer$name_waterlevel[k]) {
    if(!any(is.na(hydro_summer$snowstorage[(k - 1):k]))) {
      if (hydro_summer$MM[k] == 5) {
        if (hydro_summer$MM[k - 1] == 5) {
          snowstorage_diff[k] <- hydro_summer$snowstorage[k] - hydro_summer$snowstorage[k - 1]
        }
      } else {
        snowstorage_diff[k] <- hydro_summer$snowstorage[k] - hydro_summer$snowstorage[k - 1]
      }
    }
  }
}
hydro_summer <- cbind(hydro_summer, snowstorage_diff)


# add column snowstorage_drain
# winter
hydro_winter$snowstorage_drain <- hydro_winter$snowstorage_diff
for (k in 1:length(hydro_winter$snowstorage_drain)) {
  if (!is.na(hydro_winter$snowstorage_diff[k])) {
    if (hydro_winter$snowstorage_diff[k] < 0) {
      hydro_winter$snowstorage_drain[k] <- abs(hydro_winter$snowstorage_drain[k])
    } else if (hydro_winter$snowstorage_diff[k] >= 0) {
      hydro_winter$snowstorage_drain[k] <- 0
    }
  }
}

# summer
hydro_summer$snowstorage_drain <- hydro_summer$snowstorage_diff
for (k in 1:length(hydro_summer$snowstorage_drain)) {
  if (!is.na(hydro_summer$snowstorage_diff[k])) {
    if (hydro_summer$snowstorage_diff[k] < 0) {
      hydro_summer$snowstorage_drain[k] <- abs(hydro_summer$snowstorage_drain[k])
    } else if (hydro_summer$snowstorage_diff[k] >= 0) {
      hydro_summer$snowstorage_drain[k] <- 0
    }
  }
}

# add column avg_snowstorage_drain
# winter
avg_snowstorage_drain <- as.numeric(rep(NA, length = nrow(hydro_winter)))
for(k in 7:length(avg_snowstorage_drain)) {
  if (all(hydro_winter$member[(k - 6):(k - 1)] == hydro_winter$member[k]) &&
      all(hydro_winter$name_waterlevel[(k - 6):(k - 1)] == hydro_winter$name_waterlevel[k])) {
    if (hydro_winter$MM[k] == 11) {
      if (all(hydro_winter$MM[(k - 6):(k - 1)] == 11)) {
        avg_snowstorage_drain[k] <- mean(hydro_winter$snowstorage_drain[(k - 6):k], na.rm = TRUE)
      }
    } else {
      avg_snowstorage_drain[k] <- mean(hydro_winter$snowstorage_drain[(k - 6):k], na.rm = TRUE)
    }
  }
}
hydro_winter <- cbind(hydro_winter, avg_snowstorage_drain) 

# summer
avg_snowstorage_drain <- as.numeric(rep(NA, length = nrow(hydro_summer)))
for(k in 7:length(avg_snowstorage_drain)) {
  if (all(hydro_summer$member[(k - 6):(k - 1)] == hydro_summer$member[k]) &&
      all(hydro_summer$name_waterlevel[(k - 6):(k - 1)] == hydro_summer$name_waterlevel[k])) {
    if (hydro_summer$MM[k] == 5) {
      if (all(hydro_summer$MM[(k - 6):(k - 1)] == 5)) {
        avg_snowstorage_drain[k] <- mean(hydro_summer$snowstorage_drain[(k - 6):k], na.rm = TRUE)
      }
    } else {
      avg_snowstorage_drain[k] <- mean(hydro_summer$snowstorage_drain[(k - 6):k], na.rm = TRUE)
    }
  }
}
hydro_summer <- cbind(hydro_summer, avg_snowstorage_drain) 


## add column lowlevel_first
# winter
lowlevel_first <- as.numeric(rep(NA, length = nrow(hydro_winter)))
for(k in 1:length(lowlevel_first)) {
  if (!is.na(hydro_winter$lowlevel_days[k])) {
    if (hydro_winter$lowlevel_days[k] == 1) {
      lowlevel_first[k] <- TRUE
    } else {
      lowlevel_first[k] <- FALSE
    }
  }
}
hydro_winter <- cbind(hydro_winter, lowlevel_first) 


# summer
lowlevel_first <- as.numeric(rep(NA, length = nrow(hydro_summer)))
for(k in 1:length(lowlevel_first)) {
  if (!is.na(hydro_summer$lowlevel_days[k])) {
    if (hydro_summer$lowlevel_days[k] == 1) {
      lowlevel_first[k] <- TRUE
    } else {
      lowlevel_first[k] <- FALSE
    }
  }
}
hydro_summer <- cbind(hydro_summer, lowlevel_first) 




# Create subsets for hydrological half-year for each waterlevel
hydro_summer_20203 <- hydro_summer %>% filter(name_waterlevel == "20203")
hydro_summer_11502 <- hydro_summer %>% filter(name_waterlevel == "11502")
hydro_summer_10304 <- hydro_summer %>% filter(name_waterlevel == "10304")
hydro_winter_20203 <- hydro_winter %>% filter(name_waterlevel == "20203")
hydro_winter_11502 <- hydro_winter %>% filter(name_waterlevel == "11502")
hydro_winter_10304 <- hydro_winter %>% filter(name_waterlevel == "10304")


##Rescaling avg Variables
hydro_summer_10304$avg_soilwater_scaled <- hydro_summer_10304$avg_soilwater * 10
hydro_summer_10304$avg_snowstorage_scaled <- hydro_summer_10304$avg_snowstorage * 0.5
hydro_summer_10304$avg_airtmp_scaled <- hydro_summer_10304$avg_airtmp * 3.3
hydro_summer_10304$avg_precip_scaled <- hydro_summer_10304$avg_precip * 5
hydro_summer_10304$avg_glorad_scaled <- hydro_summer_10304$avg_glorad * 0.1
hydro_summer_10304$avg_relhum_scaled<- hydro_summer_10304$avg_relhum * 1
hydro_summer_10304$avg_infiltration_scaled <- hydro_summer_10304$avg_infiltration * 50
hydro_summer_10304$groundwaterdepth_scaled <- hydro_summer_10304$groundwaterdepth * 33
hydro_summer_10304$avg_snowstorage_drain_scaled <- hydro_summer_10304$avg_snowstorage_drain * 0.33


hydro_summer_11502$avg_soilwater_scaled <- hydro_summer_11502$avg_soilwater * 10
hydro_summer_11502$avg_snowstorage_scaled <- hydro_summer_11502$avg_snowstorage * 0.5
hydro_summer_11502$avg_airtmp_scaled <- hydro_summer_11502$avg_airtmp * 3.3
hydro_summer_11502$avg_precip_scaled <- hydro_summer_11502$avg_precip  * 5
hydro_summer_11502$avg_glorad_scaled <- hydro_summer_11502$avg_glorad * 0.1
hydro_summer_11502$avg_relhum_scaled <- hydro_summer_11502$avg_relhum * 1
hydro_summer_11502$avg_infiltration_scaled <- hydro_summer_11502$avg_infiltration * 50
hydro_summer_11502$groundwaterdepth_scaled <- hydro_summer_11502$groundwaterdepth * 33
hydro_summer_11502$avg_snowstorage_drain_scaled <- hydro_summer_11502$avg_snowstorage_drain * 0.33


hydro_summer_20203$avg_soilwater_scaled <- hydro_summer_20203$avg_soilwater * 10
hydro_summer_20203$avg_snowstorage_scaled <- hydro_summer_20203$avg_snowstorage * 0.5
hydro_summer_20203$avg_airtmp_scaled <- hydro_summer_20203$avg_airtmp * 3.3
hydro_summer_20203$avg_precip_scaled <- hydro_summer_20203$avg_precip * 5
hydro_summer_20203$avg_glorad_scaled <- hydro_summer_20203$avg_glorad * 0.1
hydro_summer_20203$avg_relhum_scaled <- hydro_summer_20203$avg_relhum * 1
hydro_summer_20203$avg_infiltration_scaled <- hydro_summer_20203$avg_infiltration * 50
hydro_summer_20203$groundwaterdepth_scaled <- hydro_summer_20203$groundwaterdepth * 33
hydro_summer_20203$avg_snowstorage_drain_scaled <- hydro_summer_20203$avg_snowstorage_drain * 0.33



hydro_winter_10304$avg_soilwater_scaled <- hydro_winter_10304$avg_soilwater * 10
hydro_winter_10304$avg_snowstorage_scaled <- hydro_winter_10304$avg_snowstorage * 0.5
hydro_winter_10304$avg_airtmp_scaled <- hydro_winter_10304$avg_airtmp * 3.3
hydro_winter_10304$avg_precip_scaled <- hydro_winter_10304$avg_precip * 5
hydro_winter_10304$avg_glorad_scaled <- hydro_winter_10304$avg_glorad * 0.1
hydro_winter_10304$avg_relhum_scaled <- hydro_winter_10304$avg_relhum * 1
hydro_winter_10304$avg_infiltration_scaled <- hydro_winter_10304$avg_infiltration * 50
hydro_winter_10304$groundwaterdepth_scaled <- hydro_winter_10304$groundwaterdepth * 33
hydro_winter_10304$avg_snowstorage_drain_scaled <- hydro_winter_10304$avg_snowstorage_drain * 0.33


hydro_winter_11502$avg_soilwater_scaled <- hydro_winter_11502$avg_soilwater * 10
hydro_winter_11502$avg_snowstorage_scaled <- hydro_winter_11502$avg_snowstorage * 0.5
hydro_winter_11502$avg_airtmp_scaled <- hydro_winter_11502$avg_airtmp * 3.3
hydro_winter_11502$avg_precip_scaled <- hydro_winter_11502$avg_precip  * 5
hydro_winter_11502$avg_glorad_scaled <- hydro_winter_11502$avg_glorad * 0.1
hydro_winter_11502$avg_relhum_scaled <- hydro_winter_11502$avg_relhum * 1
hydro_winter_11502$avg_infiltration_scaled <- hydro_winter_11502$avg_infiltration * 50
hydro_winter_11502$groundwaterdepth_scaled <- hydro_winter_11502$groundwaterdepth * 33
hydro_winter_11502$avg_snowstorage_drain_scaled <- hydro_winter_11502$avg_snowstorage_drain * 0.33


hydro_winter_20203$avg_soilwater_scaled <- hydro_winter_20203$avg_soilwater * 10
hydro_winter_20203$avg_snowstorage_scaled <- hydro_winter_20203$avg_snowstorage * 0.5
hydro_winter_20203$avg_airtmp_scaled <- hydro_winter_20203$avg_airtmp * 3.3
hydro_winter_20203$avg_precip_scaled <- hydro_winter_20203$avg_precip * 5
hydro_winter_20203$avg_glorad_scaled <- hydro_winter_20203$avg_glorad * 0.1
hydro_winter_20203$avg_relhum_scaled <- hydro_winter_20203$avg_relhum * 1
hydro_winter_20203$avg_infiltration_scaled <- hydro_winter_20203$avg_infiltration * 50
hydro_winter_20203$groundwaterdepth_scaled <- hydro_winter_20203$groundwaterdepth * 33
hydro_winter_20203$avg_snowstorage_drain_scaled <- hydro_winter_20203$avg_snowstorage_drain * 0.33


# separate datasets for each waterlevel, season and member
# winter, 10304
for (j in levels(hydro_total$member)) {
  assign(paste0("hydro_winter_10304", "_", j), hydro_winter_10304 %>% filter(member %in% j))
}

# summer, 10304
for (j in levels(hydro_total$member)) {
  assign(paste0("hydro_summer_10304", "_", j), hydro_summer_10304 %>% filter(member %in% j))
}

# winter, 11502
for (j in levels(hydro_total$member)) {
  assign(paste0("hydro_winter_11502", "_", j), hydro_winter_11502 %>% filter(member %in% j))
}

# summer, 11502
for (j in levels(hydro_total$member)) {
  assign(paste0("hydro_summer_11502", "_", j), hydro_summer_11502 %>% filter(member %in% j))
}

# winter, 20203
for (j in levels(hydro_total$member)) {
  assign(paste0("hydro_winter_20203", "_", j), hydro_winter_20203 %>% filter(member %in% j))
}

# summer, 20203
for (j in levels(hydro_total$member)) {
  assign(paste0("hydro_summer_20203", "_", j), hydro_summer_20203 %>% filter(member %in% j))
}



## Save data


# Save pegel_prop
saveRDS(object = pegel_prop, file = "data/pegel_prop.RDS")

# Save hydro total
saveRDS(object = hydro_total, file = "data/hydro_total.RDS")

# Save hydrological half years
saveRDS(object = hydro_summer, file = "data/hydro_summer.RDS")
saveRDS(object = hydro_winter, file = "data/hydro_winter.RDS")

# Save subsets for hydrological half-year for each waterlevel
saveRDS(object = hydro_summer_20203, file = "data/hydro_summer_20203.RDS")
saveRDS(object = hydro_summer_11502, file = "data/hydro_summer_11502.RDS")
saveRDS(object = hydro_summer_10304, file = "data/hydro_summer_10304.RDS")
saveRDS(object = hydro_winter_20203, file = "data/hydro_winter_20203.RDS")
saveRDS(object = hydro_winter_11502, file = "data/hydro_winter_11502.RDS")
saveRDS(object = hydro_winter_10304, file = "data/hydro_winter_10304.RDS")

# Save subsets for each member, each hydrological half-year and for each waterlevel
saveRDS(object = hydro_summer_20203_kbe, file = "data/hydro_summer_20203_kbe.RDS")
saveRDS(object = hydro_summer_20203_kbj, file = "data/hydro_summer_20203_kbj.RDS")
saveRDS(object = hydro_summer_20203_kbo, file = "data/hydro_summer_20203_kbo.RDS")
saveRDS(object = hydro_summer_20203_kbt, file = "data/hydro_summer_20203_kbt.RDS")
saveRDS(object = hydro_summer_20203_kby, file = "data/hydro_summer_20203_kby.RDS")
saveRDS(object = hydro_summer_20203_kcd, file = "data/hydro_summer_20203_kcd.RDS")
saveRDS(object = hydro_summer_20203_kci, file = "data/hydro_summer_20203_kci.RDS")
saveRDS(object = hydro_summer_20203_kcn, file = "data/hydro_summer_20203_kcn.RDS")
saveRDS(object = hydro_summer_20203_kcs, file = "data/hydro_summer_20203_kcs.RDS")
saveRDS(object = hydro_summer_20203_kcx, file = "data/hydro_summer_20203_kcx.RDS")

saveRDS(object = hydro_summer_11502_kbe, file = "data/hydro_summer_11502_kbe.RDS")
saveRDS(object = hydro_summer_11502_kbj, file = "data/hydro_summer_11502_kbj.RDS")
saveRDS(object = hydro_summer_11502_kbo, file = "data/hydro_summer_11502_kbo.RDS")
saveRDS(object = hydro_summer_11502_kbt, file = "data/hydro_summer_11502_kbt.RDS")
saveRDS(object = hydro_summer_11502_kby, file = "data/hydro_summer_11502_kby.RDS")
saveRDS(object = hydro_summer_11502_kcd, file = "data/hydro_summer_11502_kcd.RDS")
saveRDS(object = hydro_summer_11502_kci, file = "data/hydro_summer_11502_kci.RDS")
saveRDS(object = hydro_summer_11502_kcn, file = "data/hydro_summer_11502_kcn.RDS")
saveRDS(object = hydro_summer_11502_kcs, file = "data/hydro_summer_11502_kcs.RDS")
saveRDS(object = hydro_summer_11502_kcx, file = "data/hydro_summer_11502_kcx.RDS")

saveRDS(object = hydro_summer_10304_kbe, file = "data/hydro_summer_10304_kbe.RDS")
saveRDS(object = hydro_summer_10304_kbj, file = "data/hydro_summer_10304_kbj.RDS")
saveRDS(object = hydro_summer_10304_kbo, file = "data/hydro_summer_10304_kbo.RDS")
saveRDS(object = hydro_summer_10304_kbt, file = "data/hydro_summer_10304_kbt.RDS")
saveRDS(object = hydro_summer_10304_kby, file = "data/hydro_summer_10304_kby.RDS")
saveRDS(object = hydro_summer_10304_kcd, file = "data/hydro_summer_10304_kcd.RDS")
saveRDS(object = hydro_summer_10304_kci, file = "data/hydro_summer_10304_kci.RDS")
saveRDS(object = hydro_summer_10304_kcn, file = "data/hydro_summer_10304_kcn.RDS")
saveRDS(object = hydro_summer_10304_kcs, file = "data/hydro_summer_10304_kcs.RDS")
saveRDS(object = hydro_summer_10304_kcx, file = "data/hydro_summer_10304_kcx.RDS")

saveRDS(object = hydro_winter_20203_kbe, file = "data/hydro_winter_20203_kbe.RDS")
saveRDS(object = hydro_winter_20203_kbj, file = "data/hydro_winter_20203_kbj.RDS")
saveRDS(object = hydro_winter_20203_kbo, file = "data/hydro_winter_20203_kbo.RDS")
saveRDS(object = hydro_winter_20203_kbt, file = "data/hydro_winter_20203_kbt.RDS")
saveRDS(object = hydro_winter_20203_kby, file = "data/hydro_winter_20203_kby.RDS")
saveRDS(object = hydro_winter_20203_kcd, file = "data/hydro_winter_20203_kcd.RDS")
saveRDS(object = hydro_winter_20203_kci, file = "data/hydro_winter_20203_kci.RDS")
saveRDS(object = hydro_winter_20203_kcn, file = "data/hydro_winter_20203_kcn.RDS")
saveRDS(object = hydro_winter_20203_kcs, file = "data/hydro_winter_20203_kcs.RDS")
saveRDS(object = hydro_winter_20203_kcx, file = "data/hydro_winter_20203_kcx.RDS")

saveRDS(object = hydro_winter_11502_kbe, file = "data/hydro_winter_11502_kbe.RDS")
saveRDS(object = hydro_winter_11502_kbj, file = "data/hydro_winter_11502_kbj.RDS")
saveRDS(object = hydro_winter_11502_kbo, file = "data/hydro_winter_11502_kbo.RDS")
saveRDS(object = hydro_winter_11502_kbt, file = "data/hydro_winter_11502_kbt.RDS")
saveRDS(object = hydro_winter_11502_kby, file = "data/hydro_winter_11502_kby.RDS")
saveRDS(object = hydro_winter_11502_kcd, file = "data/hydro_winter_11502_kcd.RDS")
saveRDS(object = hydro_winter_11502_kci, file = "data/hydro_winter_11502_kci.RDS")
saveRDS(object = hydro_winter_11502_kcn, file = "data/hydro_winter_11502_kcn.RDS")
saveRDS(object = hydro_winter_11502_kcs, file = "data/hydro_winter_11502_kcs.RDS")
saveRDS(object = hydro_winter_11502_kcx, file = "data/hydro_winter_11502_kcx.RDS")

saveRDS(object = hydro_winter_10304_kbe, file = "data/hydro_winter_10304_kbe.RDS")
saveRDS(object = hydro_winter_10304_kbj, file = "data/hydro_winter_10304_kbj.RDS")
saveRDS(object = hydro_winter_10304_kbo, file = "data/hydro_winter_10304_kbo.RDS")
saveRDS(object = hydro_winter_10304_kbt, file = "data/hydro_winter_10304_kbt.RDS")
saveRDS(object = hydro_winter_10304_kby, file = "data/hydro_winter_10304_kby.RDS")
saveRDS(object = hydro_winter_10304_kcd, file = "data/hydro_winter_10304_kcd.RDS")
saveRDS(object = hydro_winter_10304_kci, file = "data/hydro_winter_10304_kci.RDS")
saveRDS(object = hydro_winter_10304_kcn, file = "data/hydro_winter_10304_kcn.RDS")
saveRDS(object = hydro_winter_10304_kcs, file = "data/hydro_winter_10304_kcs.RDS")
saveRDS(object = hydro_winter_10304_kcx, file = "data/hydro_winter_10304_kcx.RDS")


# Corrplot Data

hydro_winter_10304_kbe <- readRDS(file = "./data/hydro_winter_10304_kbe.RDS")
hydro_winter_11502_kbe <- readRDS(file = "./data/hydro_winter_11502_kbe.RDS")
hydro_winter_20203_kbe <- readRDS(file = "./data/hydro_winter_20203_kbe.RDS")
hydro_summer_10304_kbe <- readRDS(file = "./data/hydro_summer_10304_kbe.RDS")
hydro_summer_11502_kbe <- readRDS(file = "./data/hydro_summer_11502_kbe.RDS")
hydro_summer_20203_kbe <- readRDS(file = "./data/hydro_summer_20203_kbe.RDS")

subset_hydro_winter_10304_kbe <- subset(hydro_winter_10304_kbe, select = c(avg_precip, avg_airtmp, avg_glorad, avg_relhum, avg_soilwater, avg_snowstorage, groundwaterdepth, avg_infiltration, max_precip, avg_snowstorage_drain))
colnames(subset_hydro_winter_10304_kbe) <- c("Mittlerer Niederschlag", "Mittlere Lufttemperatur", "Mittlere Strahlung", "Mittlere relative Luftfeuchte", "Mittlere Bodenfeuchte", "Mittlerer Schneespeicher", "Grundwasserstand", "Mittlere Versickerung", "Maximaler Niederschlag", "Mittlere Schneeschmelze")

subset_hydro_winter_11502_kbe <- subset(hydro_winter_11502_kbe, select = c(avg_precip, avg_airtmp, avg_glorad, avg_relhum, avg_soilwater, avg_snowstorage, groundwaterdepth, avg_infiltration, max_precip, avg_snowstorage_drain))
colnames(subset_hydro_winter_11502_kbe) <- c("Mittlerer Niederschlag", "Mittlere Lufttemperatur", "Mittlere Strahlung", "Mittlere relative Luftfeuchte", "Mittlere Bodenfeuchte", "Mittlerer Schneespeicher", "Grundwasserstand", "Mittlere Versickerung", "Maximaler Niederschlag", "Mittlere Schneeschmelze")

subset_hydro_winter_20203_kbe <- subset(hydro_winter_20203_kbe, select = c(avg_precip, avg_airtmp, avg_glorad, avg_relhum, avg_soilwater, avg_snowstorage, groundwaterdepth, avg_infiltration, max_precip, avg_snowstorage_drain))
colnames(subset_hydro_winter_20203_kbe) <- c("Mittlerer Niederschlag", "Mittlere Lufttemperatur", "Mittlere Strahlung", "Mittlere relative Luftfeuchte", "Mittlere Bodenfeuchte", "Mittlerer Schneespeicher", "Grundwasserstand", "Mittlere Versickerung", "Maximaler Niederschlag", "Mittlere Schneeschmelze")

subset_hydro_summer_10304_kbe <- subset(hydro_summer_10304_kbe, select = c(avg_precip, avg_airtmp, avg_glorad, avg_relhum, avg_soilwater, avg_snowstorage, groundwaterdepth, avg_infiltration, max_precip, avg_snowstorage_drain))
colnames(subset_hydro_summer_10304_kbe) <- c("Mittlerer Niederschlag", "Mittlere Lufttemperatur", "Mittlere Strahlung", "Mittlere relative Luftfeuchte", "Mittlere Bodenfeuchte", "Mittlerer Schneespeicher", "Grundwasserstand", "Mittlere Versickerung", "Maximaler Niederschlag", "Mittlere Schneeschmelze")

subset_hydro_summer_11502_kbe <- subset(hydro_summer_11502_kbe, select = c(avg_precip, avg_airtmp, avg_glorad, avg_relhum, avg_soilwater, avg_snowstorage, groundwaterdepth, avg_infiltration, max_precip, avg_snowstorage_drain))
colnames(subset_hydro_summer_11502_kbe) <- c("Mittlerer Niederschlag", "Mittlere Lufttemperatur", "Mittlere Strahlung", "Mittlere relative Luftfeuchte", "Mittlere Bodenfeuchte", "Mittlerer Schneespeicher", "Grundwasserstand", "Mittlere Versickerung", "Maximaler Niederschlag", "Mittlere Schneeschmelze")

subset_hydro_summer_20203_kbe <- subset(hydro_summer_20203_kbe, select = c(avg_precip, avg_airtmp, avg_glorad, avg_relhum, avg_soilwater, avg_snowstorage, groundwaterdepth, avg_infiltration, max_precip, avg_snowstorage_drain))
colnames(subset_hydro_summer_20203_kbe) <- c("Mittlerer Niederschlag", "Mittlere Lufttemperatur", "Mittlere Strahlung", "Mittlere relative Luftfeuchte", "Mittlere Bodenfeuchte", "Mittlerer Schneespeicher", "Grundwasserstand", "Mittlere Versickerung", "Maximaler Niederschlag", "Mittlere Schneeschmelze")

dir.create("./data/corrplots")
saveRDS(subset_hydro_winter_10304_kbe, "./data/corrplots/subset_hydro_winter_10304.RDS")
saveRDS(subset_hydro_winter_11502_kbe, "./data/corrplots/subset_hydro_winter_11502.RDS")
saveRDS(subset_hydro_winter_20203_kbe, "./data/corrplots/subset_hydro_winter_20203.RDS")

saveRDS(subset_hydro_summer_10304_kbe, "./data/corrplots/subset_hydro_summer_10304.RDS")
saveRDS(subset_hydro_summer_11502_kbe, "./data/corrplots/subset_hydro_summer_11502.RDS")
saveRDS(subset_hydro_summer_20203_kbe, "./data/corrplots/subset_hydro_summer_20203.RDS")




# Volle Modelle für alle Pegel und Halbjahre

gam_all_winter_10304 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps") + s(max_precip, bs = "ps") + s(avg_snowstorage_drain, bs = "ps"), family = binomial(), data = hydro_winter_10304_kbe, method = "REML")
summary(gam_all_winter_10304)
AIC(gam_all_winter_10304)

gam_all_summer_10304 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps") + s(max_precip, bs = "ps") + s(avg_snowstorage_drain, bs = "ps"), family = binomial(), data = hydro_summer_10304_kbe, method = "REML")
summary(gam_all_summer_10304)
AIC(gam_all_summer_10304)

gam_all_winter_11502 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps") + s(max_precip, bs = "ps") + s(avg_snowstorage_drain, bs = "ps"), family = binomial(), data = hydro_winter_11502_kbe, method = "REML")
summary(gam_all_winter_11502)
AIC(gam_all_winter_11502)

gam_all_summer_11502 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps") + s(max_precip, bs = "ps") + s(avg_snowstorage_drain, bs = "ps"), family = binomial(), data = hydro_summer_11502_kbe, method = "REML")
summary(gam_all_summer_11502)
AIC(gam_all_summer_11502)


gam_all_winter_20203 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps") + s(max_precip, bs = "ps") + s(avg_snowstorage_drain, bs = "ps"), family = binomial(), data = hydro_winter_20203_kbe, method = "REML")
summary(gam_all_winter_20203)
AIC(gam_all_winter_20203)


gam_all_summer_20203 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps") + s(max_precip, bs = "ps") + s(avg_snowstorage_drain, bs = "ps"), family = binomial(), data = hydro_summer_20203_kbe, method = "REML")
summary(gam_all_summer_20203)
AIC(gam_all_summer_20203)


# Selektierte Modelle für alle Pregel und Halbjahre
## 1

# nur Treiber: avg_precip, avg_airtmp, avg_glorad, avg_soilwater, avg_snowstorage, groundwaterdepth um Multikollinearität zu vermeiden (in keinem der 6 Datensätze eine Korrelation von > 0.8 bzw. <-0.8)
gam_uni_selected_winter_10304 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps"), family = binomial(), data = hydro_winter_10304_kbe, method = "REML")
summary(gam_uni_selected_winter_10304)
AIC(gam_uni_selected_winter_10304)
BIC(gam_uni_selected_winter_10304)
concurvity(gam_uni_selected_winter_10304)
#plot(gam_uni_selected_winter_10304, ylim = c(-7, 7))


gam_uni_selected_summer_10304 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps"), family = binomial(), data = hydro_summer_10304_kbe, method = "REML")
summary(gam_uni_selected_summer_10304)
AIC(gam_uni_selected_summer_10304)
BIC(gam_uni_selected_summer_10304)
concurvity(gam_uni_selected_summer_10304)
#plot(gam_uni_selected_summer_10304, ylim = c(-7, 7))


gam_uni_selected_winter_11502 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps"), family = binomial(), data = hydro_winter_11502_kbe, method = "REML")
summary(gam_uni_selected_winter_11502)
AIC(gam_uni_selected_winter_11502)
BIC(gam_uni_selected_winter_11502)
concurvity(gam_uni_selected_winter_11502)
#plot(gam_uni_selected_winter_11502, ylim = c(-7, 7))


gam_uni_selected_summer_11502 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps"), family = binomial(), data = hydro_summer_11502_kbe, method = "REML")
summary(gam_uni_selected_summer_11502)
AIC(gam_uni_selected_summer_11502)
BIC(gam_uni_selected_summer_11502)
concurvity(gam_uni_selected_summer_11502)
#plot(gam_uni_selected_summer_11502, ylim = c(-7, 7))


gam_uni_selected_winter_20203 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps"), family = binomial(), data = hydro_winter_20203_kbe, method = "REML")
summary(gam_uni_selected_winter_20203)
AIC(gam_uni_selected_winter_20203)
BIC(gam_uni_selected_winter_20203)
concurvity(gam_uni_selected_winter_20203)
#plot(gam_uni_selected_winter_20203, ylim = c(-7, 7))


gam_uni_selected_summer_20203 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps"), family = binomial(), data = hydro_summer_20203_kbe, method = "REML")
summary(gam_uni_selected_summer_20203)
AIC(gam_uni_selected_summer_20203)
BIC(gam_uni_selected_summer_20203)
concurvity(gam_uni_selected_summer_20203)
#plot(gam_uni_selected_summer_20203, ylim = c(-7, 7))

# Effect Plots


# Isar Winter
# Soilwater
jpeg("./Plots/Max/soilwater_winter_isar.jpeg", width = 750, height = 620)
plot(gam_uni_selected_winter_10304, ylim = c(-7, 7), xlim = c(-6,6), xlab = "Rolling mean oberflächennahe rel. Bodenfeuchte \n (letzte 60 Tage; zentriert, skaliert)", select = 4, main = "Isar Mittenwald \n Effekt der mittleren oberfl. rel. Bodenfeuchte", ylab = "log. Odds")
dev.off()
# Glorad
jpeg("./Plots/Max/glorad_winter_isar.jpeg", width = 750, height = 620)
plot(gam_uni_selected_winter_10304, ylim = c(-7, 7), xlim = c(-250, 600), xlab = "Rolling mean einfallende kurzwellige Strahlung \n (letzte 7 Tage; zentriert)", select = 3, main = "Isar Mittenwald \n Effekt der mittleren einfallenden kurzwelligen Strahlung", ylab = "log. Odds")
dev.off()
# Lufttemperatur
jpeg("./Plots/Max/airtmp_winter_isar.jpeg", width = 750, height = 620)
plot(gam_uni_selected_winter_10304, ylim = c(-7, 7), xlim = c(-15, 15), xlab = "Rolling mean Lufttemperatur \n (letzte 7 Tage; zentriert)", select = 2, main = "Isar Mittenwald \n Effekt der mittleren Lufttemperatur", ylab = "log. Odds")
dev.off()

# Saale Winter

# Soilwater
jpeg("./Plots/Max/soilwater_winter_saale.jpeg", width = 750, height = 620)
plot(gam_uni_selected_winter_20203, ylim = c(-7, 7), xlim = c(-6, 6), xlab = "Rolling mean oberflächennahe rel. Bodenfeuchte \n (letzte 60 Tage; zentriert, skaliert)", select = 4, main = "Fränkische Saale Salz \n Effekt der mittleren oberfl. rel. Bodenfeuchte", ylab = "log. Odds")
dev.off()

# Glorad
jpeg("./Plots/Max/glorad_winter_saale.jpeg", width = 750, height = 620)
plot(gam_uni_selected_winter_20203, ylim = c(-7, 7), xlab = "Rolling mean einfallende kurzwellige Strahlung \n (letzte 7 Tage; zentriert)", xlim = c(-250, 600), select = 3, main = "Fränkische Saale Salz \n Effekt der mittleren einfallenden kurzwelligen Strahlung", ylab = "log. Odds")
dev.off()

# Lufttemperatur
jpeg("./Plots/Max/airtmp_winter_saale.jpeg", width = 750, height = 620)
plot(gam_uni_selected_winter_20203, ylim = c(-7, 7), xlim = c(-15, 15), xlab = "Rolling mean Lufttemperatur \n (letzte 7 Tage; zentriert)", select = 2, main = "Fränkische Saale Salz \n Effekt der mittleren Lufttemperatur", ylab = "log. Odds")
dev.off()

# Summer
# Isar Summer
# Soilwater
jpeg("./Plots/Max/soilwater_summer_isar.jpeg", width = 750, height = 620)
plot(gam_uni_selected_summer_10304, ylim = c(-7, 7), xlim = c(-5, 2), xlab = "Rolling mean oberflächennahe rel. Bodenfeuchte \n (letzte 60 Tage; zentriert, skaliert)", select = 4, main = "Isar Mittenwald \n Effekt der mittleren oberfl. rel. Bodenfeuchte", ylab = "log. Odds")
dev.off()
# Snowstorage
jpeg("./Plots/Max/snow_summer_isar.jpeg", width = 750, height = 620)
plot(gam_uni_selected_summer_10304, ylim = c(-7, 7), xlim =  c(-50, 50), xlab = "Rolling mean Schneespeicher \n (letzte 30 Tage; zentriert)", select = 5, main = "Isar Mittenwald \n Effekt des mittleren Schneespeichers", ylab = "log. Odds")
dev.off()
# Niederschlag
jpeg("./Plots/Max/precip_summer_isar.jpeg", width = 750, height = 620)
plot(gam_uni_selected_summer_10304, ylim = c(-7, 7), xlim = c(-5, 5), xlab = "Rolling mean Niederschlag \n (letzte 7 Tage; zentriert)", select = 1, main = "Isar Mittenwald \n Effekt des mittleren Niederschlags", ylab = "log. Odds")
dev.off()

# Iller Summer
# Soilwater
jpeg("./Plots/Max/soilwater_summer_iller.jpeg", width = 750, height = 620)
plot(gam_uni_selected_summer_11502, ylim = c(-7, 7), xlim = c(-5, 2), xlab = "Rolling mean oberflächennahe rel. Bodenfeuchte \n (letzte 60 Tage; zentriert, skaliert)", select = 4, main = "Iller Kempten \n Effekt der mittleren oberfl. rel. Bodenfeuchte", ylab = "log. Odds")
dev.off()
# Snowstorage
jpeg("./Plots/Max/snow_summer_iller.jpeg", width = 750, height = 620)
plot(gam_uni_selected_summer_11502, ylim = c(-7, 7), xlim =  c(-50, 50), xlab = "Rolling mean Schneespeicher \n (letzte 30 Tage; zentriert)", select = 5, main = "Iller Kempten \n Effekt des mittleren Schneespeichers", ylab = "log. Odds")
dev.off()
# Niederschlag
jpeg("./Plots/Max/precip_summer_iller.jpeg", width = 750, height = 620)
plot(gam_uni_selected_summer_11502, ylim = c(-7, 7), xlim = c(-5, 5), xlab = "Rolling mean Niederschlag \n (letzte 7 Tage; zentriert)", select = 1, main = "Iller Kempten \n Effekt des mittleren Niederschlags", ylab = "log. Odds")
dev.off()


# Selektierte Modelle für alle Pregel und Halbjahre
## 2

# nur Treiber: avg_airtmp, avg_relhum, avg_soilwater, avg_snowstorage, groundwaterdepth, avg_infiltration um Multikollinearität zu vermeiden (in keinem der 6 Datensätze eine Korrelation von > 0.8 bzw. <-0.8)
gam2_uni_selected_winter_10304 <- gam(formula = lowlevel ~ YY + s(avg_airtmp, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps"), family = binomial(), data = hydro_winter_10304_kbe, method = "REML")
summary(gam2_uni_selected_winter_10304)
AIC(gam2_uni_selected_winter_10304)
BIC(gam2_uni_selected_winter_10304)
concurvity(gam2_uni_selected_winter_10304)
#plot(gam2_uni_selected_winter_10304, ylim = c(-7, 7))


gam2_uni_selected_summer_10304 <- gam(formula = lowlevel ~ YY + s(avg_airtmp, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps"), family = binomial(), data = hydro_summer_10304_kbe, method = "REML")
summary(gam2_uni_selected_summer_10304)
AIC(gam2_uni_selected_summer_10304)
BIC(gam2_uni_selected_summer_10304)
concurvity(gam2_uni_selected_summer_10304)
#plot(gam2_uni_selected_summer_10304, ylim = c(-7, 7))


gam2_uni_selected_winter_11502 <- gam(formula = lowlevel ~ YY + s(avg_airtmp, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps"), family = binomial(), data = hydro_winter_11502_kbe, method = "REML")
summary(gam2_uni_selected_winter_11502)
AIC(gam2_uni_selected_winter_11502)
BIC(gam2_uni_selected_winter_11502)
concurvity(gam2_uni_selected_winter_11502)
#plot(gam2_uni_selected_winter_11502, ylim = c(-7, 7))


gam2_uni_selected_summer_11502 <- gam(formula = lowlevel ~ YY + s(avg_airtmp, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps"), family = binomial(), data = hydro_summer_11502_kbe, method = "REML")
summary(gam2_uni_selected_summer_11502)
AIC(gam2_uni_selected_summer_11502)
BIC(gam2_uni_selected_summer_11502)
concurvity(gam2_uni_selected_summer_11502)
#plot(gam2_uni_selected_summer_11502, ylim = c(-7, 7))


gam2_uni_selected_winter_20203 <- gam(formula = lowlevel ~ YY + s(avg_airtmp, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps"), family = binomial(), data = hydro_winter_20203_kbe, method = "REML")
summary(gam2_uni_selected_winter_20203)
AIC(gam2_uni_selected_winter_20203)
BIC(gam2_uni_selected_winter_20203)
concurvity(gam2_uni_selected_winter_20203)
#plot(gam2_uni_selected_winter_20203, ylim = c(-7, 7))


gam2_uni_selected_summer_20203 <- gam(formula = lowlevel ~ YY + s(avg_airtmp, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps"), family = binomial(), data = hydro_summer_20203_kbe, method = "REML")
summary(gam2_uni_selected_summer_20203)
AIC(gam2_uni_selected_summer_20203)
BIC(gam2_uni_selected_summer_20203)
concurvity(gam2_uni_selected_summer_20203)
#plot(gam2_uni_selected_summer_20203, ylim = c(-7, 7))



# Selektierte Modelle für alle Pregel und Halbjahre mit individuellen Interaktionen

# 10304, Winter
gam_uni_selected_interac_winter_10304 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps") + s(avg_snowstorage_drain, bs = "ps") + ti(avg_glorad, avg_snowstorage, bs = "ps") + ti(avg_soilwater, groundwaterdepth, bs = "ps"), family = binomial(), data = hydro_winter_10304_kbe, method = "REML")
AIC(gam_uni_selected_interac_winter_10304)
summary(gam_uni_selected_interac_winter_10304)

# 10304, Summer
gam_uni_selected_interac_summer_10304 <- gam(formula = lowlevel ~ YY + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps") + ti(avg_soilwater, groundwaterdepth, bs = "ps") + ti(avg_airtmp, avg_infiltration, bs = "ps"), family = binomial(), data = hydro_summer_10304_kbe, method = "REML")
AIC(gam_uni_selected_interac_summer_10304)
summary(gam_uni_selected_interac_summer_10304)

# 11502, Winter
gam_uni_selected_interac_winter_11502 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps") + s(avg_snowstorage_drain, bs = "ps") + ti(avg_snowstorage, groundwaterdepth, bs = "ps"), family = binomial(), data = hydro_winter_11502_kbe, method = "REML")
AIC(gam_uni_selected_interac_winter_11502)
summary(gam_uni_selected_interac_winter_11502)

# 11502, Summer
gam_uni_selected_interac_summer_11502 <- gam(formula = lowlevel ~ YY  + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps") + ti(avg_soilwater, groundwaterdepth, bs = "ps") + ti(avg_airtmp, avg_infiltration, bs = "ps"), family = binomial(), data = hydro_summer_11502_kbe, method = "REML")
AIC(gam_uni_selected_interac_summer_11502)
summary(gam_uni_selected_interac_summer_11502)

# 20203, Winter
gam_uni_selected_interac_winter_20203 <- gam(formula = lowlevel ~ YY  + s(avg_airtmp, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps") + s(avg_snowstorage_drain, bs = "ps") + ti(avg_airtmp, groundwaterdepth, bs = "ps") + ti(avg_soilwater, avg_snowstorage, bs = "ps"), family = binomial(), data = hydro_winter_20203_kbe, method = "REML")
AIC(gam_uni_selected_interac_winter_20203)
summary(gam_uni_selected_interac_winter_20203)

# 20203, Summer
# avg_snowstorage und avg_snowstorage_drain aufgrund von mangelnder Varianz in den Daten herausgenommen  
gam_uni_selected_interac_summer_20203 <- gam(formula = lowlevel ~ YY  + s(avg_airtmp, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps")  + ti(avg_soilwater, groundwaterdepth, bs = "ps") + ti(avg_airtmp, avg_infiltration, bs = "ps"), family = binomial(), data = hydro_summer_20203_kbe, method = "REML")
AIC(gam_uni_selected_interac_summer_20203)
summary(gam_uni_selected_interac_summer_20203)

# Save Models ----
## Volle Modelle ----

save(gam_all_summer_10304, file = "added_data/models/gam_all_summer_10304.Rdata")
save(gam_all_summer_11502, file = "added_data/models/gam_all_summer_11502.Rdata")
save(gam_all_summer_20203, file = "added_data/models/gam_all_summer_20203.Rdata")

save(gam_all_winter_10304, file = "added_data/models/gam_all_winter_10304.Rdata")
save(gam_all_winter_11502, file = "added_data/models/gam_all_winter_11502.Rdata")
save(gam_all_winter_20203, file = "added_data/models/gam_all_winter_20203.Rdata")

## Selected Modelle 1 ----

save(gam_uni_selected_summer_10304, file = "added_data/models/gam_uni_selected_summer_10304.Rdata")
save(gam_uni_selected_summer_11502, file = "added_data/models/gam_uni_selected_summer_11502.Rdata")
save(gam_uni_selected_summer_20203, file = "added_data/models/gam_uni_selected_summer_20203.Rdata")

save(gam_uni_selected_winter_10304, file = "added_data/models/gam_uni_selected_winter_10304.Rdata")
save(gam_uni_selected_winter_11502, file = "added_data/models/gam_uni_selected_winter_11502.Rdata")
save(gam_uni_selected_winter_20203, file = "added_data/models/gam_uni_selected_winter_20203.Rdata")

## Selected Modelle 2 ----

save(gam2_uni_selected_summer_10304, file = "added_data/models/gam2_uni_selected_summer_10304.Rdata")
save(gam2_uni_selected_summer_11502, file = "added_data/models/gam2_uni_selected_summer_11502.Rdata")
save(gam2_uni_selected_summer_20203, file = "added_data/models/gam2_uni_selected_summer_20203.Rdata")

save(gam2_uni_selected_winter_10304, file = "added_data/models/gam2_uni_selected_winter_10304.Rdata")
save(gam2_uni_selected_winter_11502, file = "added_data/models/gam2_uni_selected_winter_11502.Rdata")
save(gam2_uni_selected_winter_20203, file = "added_data/models/gam2_uni_selected_winter_20203.Rdata")

# GAM Interactions ----

save(gam_uni_selected_interac_summer_10304, file = "added_data/models/gam_uni_selected_interac_summer_10304.Rdata")
save(gam_uni_selected_interac_summer_11502, file = "added_data/models/gam_uni_selected_interac_summer_11502.Rdata")
save(gam_uni_selected_interac_summer_20203, file = "added_data/models/gam_uni_selected_interac_summer_20203.Rdata")

save(gam_uni_selected_interac_winter_10304, file = "added_data/models/gam_uni_selected_interac_winter_10304.Rdata")
save(gam_uni_selected_interac_winter_11502, file = "added_data/models/gam_uni_selected_interac_winter_11502.Rdata")
save(gam_uni_selected_interac_winter_20203, file = "added_data/models/gam_uni_selected_interac_winter_20203.Rdata")



# LE Models


# Winter
# Full Model ----
# 20203 (Precip, infiltration, snowdrain)
summary(gam_all_winter_20203)
le_gam_all_winter_20203 <- gam(lowlevel ~ YY + avg_precip + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater,bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + avg_infiltration + s(max_precip,bs = "ps") + avg_snowstorage_drain,
                               family = binomial(), data = hydro_winter_20203_kbe, method = "REML")
summary(le_gam_all_winter_20203)
# 11502 (precip, relhum, max_precip, glorad)
summary(gam_all_winter_11502)
le_gam_all_winter_11502 <- gam(lowlevel ~ YY + avg_precip+ s(avg_airtmp, bs = "ps") + avg_glorad + avg_relhum + s(avg_soilwater,bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps") + max_precip + s(avg_snowstorage_drain, bs = "ps"),
                               family = binomial(), data = hydro_winter_11502_kbe, method = "REML")
summary(le_gam_all_winter_11502)
# 10304 (airtmp, infiltration)
summary(gam_all_winter_10304)
le_gam_all_winter_10304 <- gam(lowlevel ~ YY + s(avg_precip, bs = "ps") + avg_airtmp + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater,bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + avg_infiltration + s(max_precip,bs = "ps") + s(avg_snowstorage_drain, bs = "ps"),
                               family = binomial(), data = hydro_winter_10304_kbe, method = "REML")
summary(le_gam_all_winter_10304)
# Selected 1 ----
# 20203 (NONE)
summary(gam_uni_selected_winter_20203)
le_gam_uni_selected_winter_20203 <- gam_uni_selected_winter_20203
# 11502 (avg_glorad, when I remove glorad precip gets linear)
summary(gam_uni_selected_winter_11502)
le_gam_uni_selected_winter_11502 <- gam(formula = lowlevel ~ YY + avg_precip + s(avg_airtmp, bs = "ps") + avg_glorad + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps"), family = binomial(), data = hydro_winter_11502_kbe, method = "REML")
summary(le_gam_uni_selected_winter_11502)
# 10304 (NONE)
summary(gam_uni_selected_winter_10304)
le_gam_uni_selected_winter_10304 <- gam_uni_selected_winter_10304
# Selected 2 DONE ----
# 20203 (None)
summary(gam2_uni_selected_winter_20203)
le_gam2_uni_selected_winter_20203 <- gam2_uni_selected_winter_20203
# 11502 (None)
summary(gam2_uni_selected_winter_11502)
le_gam2_uni_selected_winter_11502 <- gam2_uni_selected_winter_11502
# 10304 (None)
summary(gam2_uni_selected_winter_10304)
le_gam2_uni_selected_winter_10304 <- gam2_uni_selected_winter_10304

# Interactions ----
# 20203 (airtmp, snowstorage, infiltration)
summary(gam_uni_selected_interac_winter_20203)
le_gam_uni_selected_interac_winter_20203 <- gam(formula = lowlevel ~ YY  + avg_airtmp + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps") + avg_infiltration + s(avg_snowstorage_drain, bs = "ps") + ti(avg_airtmp, groundwaterdepth, bs = "ps") + ti(avg_soilwater, avg_snowstorage, bs = "ps"), family = binomial(), data = hydro_winter_20203_kbe, method = "REML")
summary(le_gam_uni_selected_interac_winter_20203)
# 11502 (relhum, precip)
summary(gam_uni_selected_interac_winter_11502)
le_gam_uni_selected_interac_winter_11502 <- gam(formula = lowlevel ~ YY + avg_precip + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + avg_relhum + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps") + s(avg_snowstorage_drain, bs = "ps") + ti(avg_snowstorage, groundwaterdepth, bs = "ps"), family = binomial(), data = hydro_winter_11502_kbe, method = "REML")# 10304 (groundwaterdepth)
summary(le_gam_uni_selected_interac_winter_11502)

# 10304 (groundwaterdepth)
summary(gam_uni_selected_interac_winter_10304)
le_gam_uni_selected_interac_winter_10304 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + groundwaterdepth + s(avg_infiltration, bs = "ps") + s(avg_snowstorage_drain, bs = "ps") + ti(avg_glorad, avg_snowstorage, bs = "ps") + ti(avg_soilwater, groundwaterdepth, bs = "ps"), family = binomial(), data = hydro_winter_10304_kbe, method = "REML")
summary(le_gam_uni_selected_interac_winter_10304)

# Summer ----
# Full Model ----
# 20203 (Precip, snowstorage, snowdrain)
summary(gam_all_summer_20203)
le_gam_all_summer_20203 <- gam(formula = lowlevel ~ YY + avg_precip + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps") + s(max_precip, bs = "ps") + avg_snowstorage_drain, family = binomial(), data = hydro_summer_20203_kbe, method = "REML")
summary(le_gam_all_summer_20203)
# 11502 (precip, snowstorage, snowstorage_drain)
summary(gam_all_summer_11502)
le_gam_all_summer_11502 <- gam(formula = lowlevel ~ YY + avg_precip + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps") + s(max_precip, bs = "ps") + avg_snowstorage_drain, family = binomial(), data = hydro_summer_11502_kbe, method = "REML")
summary(le_gam_all_summer_11502)
# 10304 (snowstorage, infiltration, max_precip, snowdrain)
summary(gam_all_summer_10304)
le_gam_all_summer_10304 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps") + avg_infiltration + max_precip + avg_snowstorage_drain, family = binomial(), data = hydro_summer_10304_kbe, method = "REML")
summary(le_gam_all_summer_10304)

# Selected 1 (Nota copy paste error snowstorage always linear) ---- 
# 20203 (snowstorage) 
summary(gam_uni_selected_summer_20203)
le_gam_uni_selected_summer_20203 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps"), family = binomial(), data = hydro_summer_20203_kbe, method = "REML")
summary(le_gam_uni_selected_summer_20203)
# 11502 (snowstorage)
summary(gam_uni_selected_summer_11502)
le_gam_uni_selected_summer_11502 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps"), family = binomial(), data = hydro_summer_11502_kbe, method = "REML")
summary(le_gam_uni_selected_summer_11502)
# 10304 (snowstorage)
summary(gam_uni_selected_summer_10304)
le_gam_uni_selected_summer_10304 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps"), family = binomial(), data = hydro_summer_10304_kbe, method = "REML")
summary(le_gam_uni_selected_summer_10304)

# Selected 2 ----
# 20203 (airtmp snowstorage)
summary(gam2_uni_selected_summer_20203)
le_gam2_uni_selected_summer_20203 <- gam(formula = lowlevel ~ YY + avg_airtmp + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps"), family = binomial(), data = hydro_summer_20203_kbe, method = "REML")
summary(le_gam2_uni_selected_summer_20203)
# 11502 (snowstorage)
summary(gam2_uni_selected_summer_11502)
le_gam2_uni_selected_summer_11502 <- gam(formula = lowlevel ~ YY + s(avg_airtmp, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps"), family = binomial(), data = hydro_summer_11502_kbe, method = "REML")
summary(le_gam2_uni_selected_summer_11502)
# 10304 (relhum, snowstorage)
summary(gam2_uni_selected_summer_10304)
le_gam2_uni_selected_summer_10304 <- gam(formula = lowlevel ~ YY + s(avg_airtmp, bs = "ps") + avg_relhum + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps"), family = binomial(), data = hydro_summer_10304_kbe, method = "REML")
summary(le_gam2_uni_selected_summer_10304)
# Interactions ----
# 20203 (infiltration)
summary(gam_uni_selected_interac_summer_20203)
le_gam_uni_selected_interac_summer_20203 <- gam(formula = lowlevel ~ YY  + s(avg_airtmp, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(groundwaterdepth, bs = "ps") + avg_infiltration  + ti(avg_soilwater, groundwaterdepth, bs = "ps") + ti(avg_airtmp, avg_infiltration, bs = "ps"), family = binomial(), data = hydro_summer_20203_kbe, method = "REML")
summary(le_gam_uni_selected_interac_summer_20203)
# 11502 (relhum, snowstorage)
summary(gam_uni_selected_interac_summer_11502)
le_gam_uni_selected_interac_summer_11502 <- gam(formula = lowlevel ~ YY  + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + avg_relhum + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps") + ti(avg_soilwater, groundwaterdepth, bs = "ps") + ti(avg_airtmp, avg_infiltration, bs = "ps"), family = binomial(), data = hydro_summer_11502_kbe, method = "REML")
summary(le_gam_uni_selected_interac_summer_11502)
# 10304 (groundwaterdepth, snowstorage, relhum)
summary(gam_uni_selected_interac_summer_10304)
le_gam_uni_selected_interac_summer_10304 <- gam(formula = lowlevel ~ YY + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + avg_relhum + s(avg_soilwater, bs = "ps") + avg_snowstorage + groundwaterdepth + s(avg_infiltration, bs = "ps") + ti(avg_soilwater, groundwaterdepth, bs = "ps") + ti(avg_airtmp, avg_infiltration, bs = "ps"), family = binomial(), data = hydro_summer_10304_kbe, method = "REML")
summary(le_gam_uni_selected_interac_summer_10304)


# Save LE Models
## Volle Modelle ----

save(le_gam_all_summer_10304, file = "added_data/le_models/le_gam_all_summer_10304.Rdata")
save(le_gam_all_summer_11502, file = "added_data/le_models/le_gam_all_summer_11502.Rdata")
save(le_gam_all_summer_20203, file = "added_data/le_models/le_gam_all_summer_20203.Rdata")

save(le_gam_all_winter_10304, file = "added_data/le_models/le_gam_all_winter_10304.Rdata")
save(le_gam_all_winter_11502, file = "added_data/le_models/le_gam_all_winter_11502.Rdata")
save(le_gam_all_winter_20203, file = "added_data/le_models/le_gam_all_winter_20203.Rdata")

## Selected Modelle 1 ----

save(le_gam_uni_selected_summer_10304, file = "added_data/le_models/le_gam_uni_selected_summer_10304.Rdata")
save(le_gam_uni_selected_summer_11502, file = "added_data/le_models/le_gam_uni_selected_summer_11502.Rdata")
save(le_gam_uni_selected_summer_20203, file = "added_data/le_models/le_gam_uni_selected_summer_20203.Rdata")

save(le_gam_uni_selected_winter_10304, file = "added_data/le_models/le_gam_uni_selected_winter_10304.Rdata")
save(le_gam_uni_selected_winter_11502, file = "added_data/le_models/le_gam_uni_selected_winter_11502.Rdata")
save(le_gam_uni_selected_winter_20203, file = "added_data/le_models/le_gam_uni_selected_winter_20203.Rdata")

## Selected Modelle 2 ----

save(le_gam2_uni_selected_summer_10304, file = "added_data/le_models/le_gam2_uni_selected_summer_10304.Rdata")
save(le_gam2_uni_selected_summer_11502, file = "added_data/le_models/le_gam2_uni_selected_summer_11502.Rdata")
save(le_gam2_uni_selected_summer_20203, file = "added_data/le_models/le_gam2_uni_selected_summer_20203.Rdata")

save(le_gam2_uni_selected_winter_10304, file = "added_data/le_models/le_gam2_uni_selected_winter_10304.Rdata")
save(le_gam2_uni_selected_winter_11502, file = "added_data/le_models/le_gam2_uni_selected_winter_11502.Rdata")
save(le_gam2_uni_selected_winter_20203, file = "added_data/le_models/le_gam2_uni_selected_winter_20203.Rdata")

# GAM Interactions ----

save(le_gam_uni_selected_interac_summer_10304, file = "added_data/le_models/le_gam_uni_selected_interac_summer_10304.Rdata")
save(le_gam_uni_selected_interac_summer_11502, file = "added_data/le_models/le_gam_uni_selected_interac_summer_11502.Rdata")
save(le_gam_uni_selected_interac_summer_20203, file = "added_data/le_models/le_gam_uni_selected_interac_summer_20203.Rdata")

save(le_gam_uni_selected_interac_winter_10304, file = "added_data/le_models/le_gam_uni_selected_interac_winter_10304.Rdata")
save(le_gam_uni_selected_interac_winter_11502, file = "added_data/le_models/le_gam_uni_selected_interac_winter_11502.Rdata")
save(gam_uni_selected_interac_winter_20203, file = "added_data/le_models/le_gam_uni_selected_interac_winter_20203.Rdata")

