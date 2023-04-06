library(tidyverse)
filepath_list <- list(airtmp = "observed_data/Treiber/airtmp.txt",
                      glorad = "observed_data/Treiber/glorad.txt",
                      groundwaterdepth = "observed_data/Treiber/groundwaterdepth.txt",
                      precip = "observed_data/Treiber/precip.txt",
                      qinfiltartionfirstlayer = "observed_data/Treiber/qinfiltartionfirstlayer.txt",
                      relhum = "observed_data/Treiber/relhum.txt",
                      snowstorage = "observed_data/Treiber/snowstorage.txt",
                      soilwaterrootzone = "observed_data/Treiber/soilwaterrootzone.txt")

file_list <- lapply(filepath_list, read.table, header = TRUE)

a <- file_list$airtmp
a
