# Modellierte Zielvariable
target_mod <- read.table("observed_data/Target/gerinneabfluss_modelliert_mit_Beobachtungsdaten.txt", header = TRUE)
target_mod$YY <- as.numeric(target_mod$YY)
target_mod$MM <- as.numeric(target_mod$MM)
target_mod$DD <- as.numeric(target_mod$DD)

# 11502
target_mod_11502 <- target_mod[1:5]
target_mod_11502 <- target_mod_11502 %>%
  group_by(YY, MM, DD) %>%
  summarise(X11502 = mean(X11502)) %>% as.data.frame()

# 10304
target_mod_10304 <- cbind(target_mod[1:4],target_mod[6]) 
target_mod_10304 <- target_mod_10304 %>%
  group_by(YY, MM, DD) %>%
  summarise(X10304 = mean(X10304)) %>% as.data.frame()

# 20203
target_mod_20203 <- cbind(target_mod[1:4],target_mod[7])
target_mod_20203 <- target_mod_20203 %>%
  group_by(YY, MM, DD) %>%
  summarise(X20203 = mean(X20203)) %>% as.data.frame()

# Gemessene Zielvariable
target_measured <- read.table("observed_data/Target/gerinneabfluss_gemessen.txt", skip = 1)
target_measured$YY <- as.numeric(target_measured$YY)
target_measured$MM <- as.numeric(target_measured$MM)
target_measured$DD <- as.numeric(target_measured$DD)

# 11502
target_measured_11502 <- target_measured[1:5]
target_measured_11502 <- target_measured_11502 %>%
  group_by(YY, MM, DD) %>%
  summarise(X11502 = mean(X11502)) %>% as.data.frame()

# 10304
target_measured_10304 <- cbind(target_measured[1:4],target_measured[6]) 
target_measured_10304 <- target_measured_10304 %>%
  group_by(YY, MM, DD) %>%
  summarise(X10304 = mean(X10304)) %>% as.data.frame()

# 20203
target_measured_20203 <- cbind(target_measured[1:4],target_measured[7])
target_measured_20203 <- target_measured_20203 %>%
  group_by(YY, MM, DD) %>%
  summarise(X20203 = mean(X20203)) %>% as.data.frame()
