library(tidyverse)
# Treiber Variabless
filepath_list <- list(airtmp = "observed_data/Treiber/airtmp.txt",
                      glorad = "observed_data/Treiber/glorad.txt",
                      groundwaterdepth = "observed_data/Treiber/groundwaterdepth.txt",
                      qinfiltartionfirstlayer = "observed_data/Treiber/qinfiltartionfirstlayer.txt",
                      relhum = "observed_data/Treiber/relhum.txt",
                      snowstorage = "observed_data/Treiber/snowstorage.txt",
                      soilwaterrootzone = "observed_data/Treiber/soilwaterrootzone.txt")

file_list <- lapply(filepath_list, read.table, header = TRUE)



pivaggregate_treiber <- function(variable, file_list) {
  df <- file_list[[variable]]
  
  df  <- tidyr::unite(df, col='key', c('YY', 'MM', "DD", "HH"), sep='-')
  df$key <- as.factor(df$key)
  df <- gather(df, catchment, variable, X11502:X20203, factor_key=TRUE)
  df <- tidyr::separate(df, col = "key", c('YY', 'MM', "DD", "HH"), sep='-')
  df$YY <- as.numeric(df$YY)
  df$MM <- as.numeric(df$MM)
  df$DD <- as.numeric(df$DD)
  df  <- tidyr::unite(df, col='key', c('YY', 'MM', "DD", "catchment"), sep='-', remove = FALSE)
  df <- df %>%
    group_by(YY, MM, DD, catchment, key) %>%
    summarise(variable = mean(variable)) %>% as.data.frame()
}


df_airtmp <- pivaggregate_treiber("airtmp", file_list) %>% rename("airtmp" = "variable")
df_glorad <- pivaggregate_treiber("glorad", file_list) %>% rename("glorad" = "variable")
df_groundwaterdepth <- pivaggregate_treiber("groundwaterdepth", file_list) %>% rename("groundwaterdepth" = "variable")
df_qinfiltartionfirstlayer <- pivaggregate_treiber("qinfiltartionfirstlayer", file_list) %>% rename("infiltration" = "variable")
df_relhum <- pivaggregate_treiber("relhum", file_list) %>% rename("relhum" = "variable")
df_snowstorage <- pivaggregate_treiber("snowstorage", file_list) %>% rename("snowstorage" = "variable")
df_soilwaterrootzone <- pivaggregate_treiber("soilwaterrootzone", file_list) %>% rename("soilwater" = "variable")
## Add Precip which uses different aggregate function
df_precip <- read.table("observed_data/Treiber/precip.txt", header = TRUE)
df_precip  <- tidyr::unite(df_precip, col='key', c('YY', 'MM', "DD", "HH"), sep='-')
df_precip$key <- as.factor(df_precip$key)
df_precip <- gather(df_precip, catchment, variable, X11502:X20203, factor_key=TRUE)
df_precip <- tidyr::separate(df_precip, col = "key", c('YY', 'MM', "DD", "HH"), sep='-')
df_precip$YY <- as.numeric(df_precip$YY)
df_precip$MM <- as.numeric(df_precip$MM)
df_precip$DD <- as.numeric(df_precip$DD)
df_precip  <- tidyr::unite(df_precip, col='key', c('YY', 'MM', "DD", "catchment"), sep='-', remove = FALSE)
df_precip <- df_precip %>%
  group_by(YY, MM, DD, catchment, key) %>%
  summarise(precip = sum(variable)) %>% as.data.frame()

df <- df_airtmp
df <- df %>% left_join(df_glorad, by=c("key", "YY", "MM", "DD", "catchment"))
df <- df %>% left_join(df_groundwaterdepth, by=c("key", "YY", "MM", "DD", "catchment"))
df <- df %>% left_join(df_qinfiltartionfirstlayer, by=c("key", "YY", "MM", "DD", "catchment"))
df <- df %>% left_join(df_relhum, by=c("key", "YY", "MM", "DD", "catchment"))
df <- df %>% left_join(df_snowstorage, by=c("key", "YY", "MM", "DD", "catchment"))
df <- df %>% left_join(df_soilwaterrootzone, by=c("key", "YY", "MM", "DD", "catchment"))
df <- df %>% left_join(df_precip, by=c("key", "YY", "MM", "DD", "catchment"))
df <- df %>% select(- key)
