# ============================================================
# Données NASA POWER — Lomé (Togo)
# Source : NASA POWER (https://power.larc.nasa.gov/)
# - Coordonnées : lon = 1.23, lat = 6.17
# - Période : 2015-01-01 à 2024-12-31
# - Altitude : elev = 23 m (Lomé)
# - Rs : ALLSKY_SFC_SW_DWN en MJ/m²/jour 
# ============================================================


library(nasapower)
library(dplyr)
library(ggplot2)
source("0_fonctions.R")


#récupération des données nasapower
#coordonnées de lomé
lon <- 1.23
lat <- 6.17

meteo <- get_power(
  community = "ag",
  lonlat = c(lon,lat),
  pars = c("T2M_MAX", "T2M_MIN", "RH2M", "WS2M",
           "ALLSKY_SFC_SW_DWN", "PRECTOTCORR"),
  dates = c("2015-01-01", "2024-12-31"),
  temporal_api = "daily"
)


# création de colone Rs, elev
meteo <- meteo %>%
  mutate(
    Rs = ALLSKY_SFC_SW_DWN,
    elev = 23,
    lat = LAT
  )


### FAO56 evapotranspiration potentiel

meteo <- meteo %>%
  mutate(ET0 = et0_fao(
    T2M_MAX = T2M_MAX,
    T2M_MIN = T2M_MIN,
    RH2M = RH2M,
    WS2M = WS2M,
    Rs = Rs,
    lat = lat,
    elev = elev,
    doy = DOY
  ))

# Sauvegarder le data frame complet avec ET0
saveRDS(meteo, file = "meteo_lome_ET0.rds")
write.csv(meteo, "meteo_lome_ET0.csv", row.names = FALSE) 


#création de groupe et classement de ET0 par mois
bilan_mensuel <- meteo %>%
  group_by(MM)%>%
  summarise(
    ET0_moy = mean(ET0, na.rm = TRUE),
    Pluie_moy = mean(PRECTOTCORR, na.rm = TRUE),
    .groups = "drop"
    )
### SAUVEGARDE DU BILAN MENSUEL ###
write.csv(bilan_mensuel,"bilan_mensuel.csv", row.names = FALSE)

## Graphique du bilan
p_bilan_mensuel <- bilan_mensuel %>%
  ggplot(aes(x=MM,y=ET0_moy))+
  geom_col(aes(y = Pluie_moy), fill = "steelblue", alpha = 0.6)+
  geom_line(aes(y = ET0_moy), color = "darkred", linewidth = 1)+
  scale_x_continuous(breaks = 1:12, labels = month.abb)+
  labs(
    title = "Bilan hydrique mensuel - Lomé 2015-2024",
    x = "Mois",
    y = "mm/jr",
    subtitle = "Bleu = pluie | Rouge = ET0"
  )+
  theme_minimal()
print(p_bilan_mensuel)
ggsave("figure_bilan_mensuel.png", plot = p_bilan_mensuel, 
       width = 10, height = 5, dpi = 150)



### ET0 Journalière de 2023 ###

meteo_2023 <- meteo %>% filter(YEAR == 2023) # Filtrer sur 2023

p_et0_2023 <- ggplot(meteo_2023, aes(x = YYYYMMDD, y = ET0)) +
    geom_line(color = "steelblue", linewidth = 0.6) +
  labs(
    title    = "ET0 journalière — Lomé (2023)",
    subtitle = "Formule FAO-56 Penman-Monteith",
    x        = "Date",
    y        = "ET0 (mm/jour)"
  ) +
  theme_minimal()
print(p_et0_2023)
ggsave("figure_ET0_2023.png", plot = p_et0_2023, 
       width = 10, height = 4, dpi = 150)

