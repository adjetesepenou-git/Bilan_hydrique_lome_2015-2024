
# 0_validation_ET0.R
# Ce fichier à pour vocation de valider le script de et0_fao() contre le package Evapotranspiration

library(nasapower)
library(dplyr)
library(Evapotranspiration)
source("0_fonctions.R")   # charge et0_fao()


### 1. Chargement des données NASA pour 2023 ###

cat("=== Chargement des données NASA ===\n")

meteo_2023 <- get_power(
  community    = "ag",
  lonlat       = c(1.23, 6.17),
  pars         = c("T2M_MAX", "T2M_MIN", "RH2M", "WS2M",
                   "ALLSKY_SFC_SW_DWN"),
  dates        = c("2023-01-01", "2023-12-31"),
  temporal_api = "daily"
) %>%
  mutate(
    Rs   = ALLSKY_SFC_SW_DWN,   # en MJ/m²/jour
    elev = 23,
    lat  = LAT
  )

cat("Nombre de jours chargés :", nrow(meteo_2023), "\n\n")




### 2. Préparation des données pour le package Evapotranspiration ###

meteo_pkg <- meteo_2023 %>%
  mutate(
    Year  = YEAR,
    Month = MM,
    Day   = DD,
    Tmax  = T2M_MAX,
    Tmin  = T2M_MIN,
    RHmax = RH2M,   # approximation : RH2M = RHmax = RHmin
    RHmin = RH2M,
    u2    = WS2M
  ) %>%
  select(Year, Month, Day, Tmax, Tmin, RHmax, RHmin, Rs, u2)

constantes <- list(
  Elev    = 23,
  lambda  = 2.45,
  lat_rad = 6.17 * pi / 180,
  Gsc     = 0.0820,
  z       = 2,
  sigma   = 4.903e-9,
  G       = 0
)

data_processed <- ReadInputs(
  varnames    = c("Tmax", "Tmin", "RHmax", "RHmin", "Rs", "u2"),
  climatedata = meteo_pkg,
  constants   = constantes,
  stopmissing = c(10, 10, 10),
  timestep    = "daily",
  message     = "no"
)


### 3. Calculons ET0 avec le package Evapotranspiration ###

ET_ref <- ET.PenmanMonteith(
  data      = data_processed,
  constants = constantes,
  ts        = "daily",
  solar     = "data",
  wind      = "yes",
  crop      = "short",
  message   = "no"
)

ET_ref_daily <- as.numeric(ET_ref$ET.Daily)
cat("ET0 package : moyenne =", mean(ET_ref_daily, na.rm = TRUE), "mm/jour\n\n")

### 4. Calculons ET0 avec la fonction et0_fao()  ###

cat("=== Calcul ET0 (ta fonction et0_fao) ===\n")

ET_moi <- et0_fao(
  T2M_MAX = meteo_pkg$Tmax,
  T2M_MIN = meteo_pkg$Tmin,
  RH2M    = meteo_pkg$RHmax,
  WS2M    = meteo_pkg$u2,
  Rs      = meteo_pkg$Rs,
  lat     = 6.17,
  elev    = 23,
  doy     = meteo_2023$DOY 
)

cat("ET0 de la fonction et0_fao() : moyenne =", mean(ET_moi, na.rm = TRUE), "mm/jour\n\n")


### 5. Comparons la fonction et0_fao() au package ###
# Vérification : les deux vecteurs ont la même longueur
stopifnot(length(ET_moi) == length(ET_ref_daily))

validation <- data.frame(
  date   = as.Date(paste(meteo_pkg$Year, meteo_pkg$Month, meteo_pkg$Day, sep = "-")),
  ET_ref = ET_ref_daily,
  ET_moi = ET_moi,
  ecart  = ET_moi - ET_ref_daily       
)

# les statistiques de comparaison
ecart_moyen  <- mean(validation$ecart, na.rm = TRUE)      # biais 
ecart_abs    <- mean(abs(validation$ecart), na.rm = TRUE) # écart absolu 
ecart_max    <- max(abs(validation$ecart), na.rm = TRUE)  # écart max
rmse         <- sqrt(mean(validation$ecart^2, na.rm = TRUE))

cat("Biais signé (moyen) :", ecart_moyen, "mm/jour\n")
cat("Écart absolu moyen  :", ecart_abs,    "mm/jour\n")
cat("Écart max           :", ecart_max,    "mm/jour\n")
cat("RMSE                :", rmse,         "mm/jour\n")

### 6. Sauvegarde ###

write.csv(validation, "validation_ET0.csv", row.names = FALSE)


