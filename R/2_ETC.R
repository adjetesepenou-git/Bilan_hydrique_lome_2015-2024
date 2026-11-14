# ============================================================
# Coefficient cultural Kc — Maïs grain
# Sources des données utilisées :
# - Valeurs Kc : FAO-56 (Allen et al., 1998), Tableau 12
#   Kc_ini = 0.30, Kc_mid = 1.20, Kc_end = 0.35
#   (0.35 retenu pour une variété locale récolté après séchage au champ)
# - Durées de stades : 15 / 25 / 30 / 20 jours (cycle 90 j)
#   Adapté de la variété Ikenne (cycle court, Togo, Saragoni et al. 1990)
#   et du Tableau 11 FAO-56 (maïs cycle 80-110 j)
# ============================================================


library(ggplot2)
library(dplyr)
library(tidyr)
source("0_fonctions.R")

###   Kc DU MAIS   ###
###création d'une séquence de 90 jours et d'une table data_kc

jours <- 1:90
data_kc <- data.frame(jours , Kc = kc_mais(jours))

### Courbe de Kc
p_kc <- ggplot(data_kc, aes(x = jours, y = Kc)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_vline(xintercept = c(15, 40, 70), linetype = "dashed", alpha = 0.5) +
  annotate("text", x = c(7, 27, 55, 80), y = 1.30,
           label = c("Initial", "Développement", "Mi-saison", "Fin"),
           color = "darkgreen", size = 4) +
  labs(
    title    = "Courbe Kc maïs - Variété 90 jours",
    subtitle = "Kc initial = 0.30 · Kc mi-saison = 1.20 · Kc fin = 0.35",
    x        = "Jours après semis", y = "Kc"
  ) +
  theme_minimal()

print(p_kc)
ggsave("figure_courbe_Kc.png", plot = p_kc, width = 8, height = 5, dpi = 150)
           
# Charger les données préparées
meteo <- readRDS("meteo_lome_ET0.rds")


###   EVAPOTRANSPIRATION CULTURAL   ###
## grande saison pluivieuse : semis debut avril
date_avril   <- as.Date("2023-04-01")
etc_avril  <- etc_saison(date_avril)

## petite saison pluvieuse : semis debut septembre
date_sept   <- as.Date("2023-09-01")
etc_sept  <- etc_saison(date_sept)


### graphique d'Avril

p_etc_avril <- etc_avril %>%
  mutate(cinq_jr = ceiling(jour_depuis_semis / 5),
         jour_cumul = (cinq_jr - 1) * 5 + 1) %>%
  group_by(jour_cumul) %>%
  summarise(ETc_5jr = sum(ETc, na.rm = TRUE),
            Pluie_5jr = sum(PRECTOTCORR, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_longer(cols = c(ETc_5jr, Pluie_5jr),
               names_to = "Variable", values_to = "mm") %>%
  ggplot(aes(x = jour_cumul, y = mm, fill = Variable)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_vline(xintercept = c(15, 40, 70), linetype = "dashed", color = "gray40") +
  scale_fill_manual(
    values = c("ETc_5jr" = "darkred", "Pluie_5jr" = "steelblue"),
    labels = c("ETc_5jr" = "ETc", "Pluie_5jr" = "Pluie")
  ) +
  labs(
    title = "ETc vs Pluie par cumul de 5 jours - Semis avril 2023",
    x     = "Jours après semis (début du bloc de 5 jours)",
    y     = "Cumul de 5 jours (mm)", fill = ""
  ) +
  theme_minimal()

print(p_etc_avril)
ggsave("figure_ETc_pluie_avril.png", plot = p_etc_avril, 
       width = 10, height = 5, dpi = 150)


### graphique de septembre 
p_etc_sept <- etc_sept %>%
  mutate(cinq_jr = ceiling(jour_depuis_semis / 5),
         jour_cumul = (cinq_jr - 1) * 5 + 1) %>%
  group_by(jour_cumul) %>%
  summarise(ETc_5jr = sum(ETc, na.rm = TRUE),
            Pluie_5jr = sum(PRECTOTCORR, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_longer(cols = c(ETc_5jr, Pluie_5jr),
               names_to = "Variable", values_to = "mm") %>%
  ggplot(aes(x = jour_cumul, y = mm, fill = Variable)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_vline(xintercept = c(15, 40, 70), linetype = "dashed", color = "gray40") +
  scale_fill_manual(
    values = c("ETc_5jr" = "darkred", "Pluie_5jr" = "steelblue"),
    labels = c("ETc_5jr" = "ETc", "Pluie_5jr" = "Pluie")
  ) +
  labs(
    title = "ETc vs Pluie par cumul de 5 jours - Semis septembre 2023",
    x     = "Jours après semis (début du bloc de 5 jours)",
    y     = "Cumul de 5 jours (mm)", fill = ""
  ) +
  theme_minimal()

print(p_etc_sept)
ggsave("figure_ETc_pluie_septembre.png", plot = p_etc_sept, 
       width = 10, height = 5, dpi = 150)



### Calcul de ETc par saison sur les differents années ###

# Boucle sur 10 ans
resultats_avril <- do.call(rbind,
                           lapply(2015:2024, bilan_annee, mois_jour = "04-01"))

resultats_sept <- do.call(rbind,
                          lapply(2015:2024, bilan_annee, mois_jour = "09-01"))

bilan_total <- rbind(resultats_avril, resultats_sept)
print(bilan_total)

bilan_total %>%
  group_by(saison) %>%
  summarise(
    Bilan_moyen = mean(Bilan, na.rm = TRUE),
    Bilan_min   = min(Bilan, na.rm = TRUE),
    Bilan_max   = max(Bilan, na.rm = TRUE),
    Bilan_sd    = sd(Bilan, na.rm = TRUE),
    N_deficits  = sum(Bilan < 0, na.rm = TRUE),
    .groups = "drop"
  )

p_bilan_annuel <- ggplot(bilan_total, aes(x = annee, y = Bilan, color = saison)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Avril" = "steelblue", "Septembre" = "orange")) +
  scale_x_continuous(breaks = 2015:2024) +
  # Annotation du point 2020 (avril)
  labs(
    title    = "Bilan pluie - ETc sur le cycle de 90 jours (2015–2024)",
    subtitle = "Avril : moy. 236 mm, médiane 219 mm, 194 mm sans 2020",
    x        = "Année", y = "Bilan (Pluie - ETc) en mm", color = "Saison"
  ) +
  theme_minimal()

print(p_bilan_annuel)
ggsave("figure_bilan_annuel.png", plot = p_bilan_annuel, width = 10, height = 5, dpi = 150)

