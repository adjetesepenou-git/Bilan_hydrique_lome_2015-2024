# ============================================================
# Bilan hydrique du sol - FAO-56
# Sources des données utilisés :
# - θcc, θpf : FAO-56 Tableau 19 (Moyenne de plage)
#   sableux        : θcc = 0.12, θpf = 0.045
#   limono_sableux : θcc = 0.23, θpf = 0.11
#   argileux       : θcc = 0.36, θpf = 0.22
# - p = 0.55 (fraction d'épuisement sans stress) : FAO-56 Tableau 22 (maïs)
# - Zr = 1.2 m (profondeur racinaire) : FAO-56 Tableau 22 (dans la plage 1 à 1.7 m)
# - elev = 23 m : altitude de Lomé (Togo)
# ============================================================
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(grid)
source("0_fonctions.R")
meteo <- readRDS("meteo_lome_ET0.rds")


### combinaison de plusieurs facteur dans un tableau

combinaisons <- expand.grid(
  annee  = 2015:2024,
  saison = c("Avril", "Septembre"),
  sol    = c("sableux", "limono_sableux", "argileux"),
  stringsAsFactors = FALSE
)

### boucle sur les conbinasons 

resultats_60 <- data.frame()

for (i in 1:nrow(combinaisons)) {
  
  annee  <- combinaisons$annee[i]
  saison <- combinaisons$saison[i]
  sol    <- combinaisons$sol[i]
  
  # Date de semis selon la saison
  mois_jour  <- ifelse(saison == "Avril", "04-01", "09-01")
  date_semis <- as.Date(paste0(annee, "-", mois_jour))
  
  # Calcul du bilan
  df <- bilan_sol(date_semis, sol = sol)
  
  # Ignorer les cycles incomplets
  if (is.null(df)) next
  
  # Statistiques
  stats <- stats_stress(df)
  stats$annee  <- annee
  stats$saison <- saison
  stats$sol    <- sol
  
  resultats_60 <- rbind(resultats_60, stats)
}

nrow(resultats_60)   # doit afficher 60

resultats_60 <- resultats_60 %>%
  select(annee, saison, sol, 
         Jours_stress, etc_reelle, etc_potentielle, 
         Ratio_ETc, Drainage_total)

print(resultats_60)

resultats_60 %>%
  as_tibble() %>%
  print(n = 60)


## classement des resultats par sol et par saison 

resultats_60 %>%
  group_by(saison, sol) %>%
  summarise(
    Jours_stress_moy = mean(Jours_stress),
    Ratio_moy        = mean(Ratio_ETc),
    Drainage_moy     = mean(Drainage_total),
    .groups = "drop"
  )

write.csv(resultats_60, "resultats_60.csv", row.names = FALSE)


### Evolution du déficit Dr au cours du cycle de septembre 2023

# le bilan pour septembre 2023, pour le sos limono-sableux
df_sept <- bilan_sol(as.Date("2023-09-01"), sol = "limono_sableux")

# --Figure du deficit de septembre 2023

p_dr <- ggplot(df_sept, aes(x = jour_depuis_semis)) +
  geom_line(aes(y = Dr), color = "darkblue", linewidth = 1) +
  geom_hline(aes(yintercept = TAW), color = "red", linetype = "dashed", linewidth = 0.8) +
  geom_hline(aes(yintercept = RAW), color = "orange", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = 5, y = df_sept$TAW[1], 
           label = "TAW (point de flétrissement)", 
           hjust = 0, vjust = -0.5, color = "red", size = 4) +
  annotate("text", x = 5, y = df_sept$RAW[1], 
           label = "RAW (seuil de stress)", 
           hjust = 0, vjust = -0.5, color = "orange", size = 4) +
  geom_vline(xintercept = c(15, 40, 70), linetype = "dotted", 
             color = "gray50", alpha = 0.7) +
  annotate("text", x = c(7, 27, 55, 80), y = max(df_sept$TAW) * 1.15,
           label = c("Initial", "Dév.", "Mi-saison", "Fin"),
           color = "gray40", size = 3.5) +
  labs(
    title    = "Déficit hydrique Dr - Septembre 2023, sol limono-sableux",
    subtitle = "Scénario de référence : 0 jour de stress",
    x        = "Jours après semis", y = "Déficit Dr (mm)"
  ) +
  theme_minimal()

print(p_dr)
ggsave("figure_Dr_septembre_2023.png", plot = p_dr, width = 8, height = 5, dpi = 150)


### carte de chaleur pour le stress

# créons une colonne combinée saison + sol

carte_chaleur <- resultats_60 %>%
  mutate(
    combinaison = paste(saison, gsub("_", "-", sol), sep = " - "),
    combinaison = factor(
      combinaison,
      levels = c(
        "Avril - sableux", "Avril - limono-sableux", "Avril - argileux",
        "Septembre - sableux", "Septembre - limono-sableux", "Septembre - argileux"
      )
    )
  )%>%

# Tracons la carte de chaleur

  ggplot(aes(x = factor(annee), y = combinaison, fill = Ratio_ETc)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(Ratio_ETc, 2)), size = 3, color = "black") +
  scale_fill_gradient2(
    low      = "red",
    mid      = "yellow",
    high     = "darkgreen",
    midpoint = 0.90,
    limits   = c(0.75, 1.00),
    name     = "Ratio ETc"
  ) +
  scale_y_discrete(limits = rev,
                   labels = function(x) gsub("_", "-", x)               ) +
  labs(
    title = "Ratio ETc réelle / potentielle - 10 ans (2015–2024)",
    subtitle = "Scénario de référence",
    x     = "Année",
    y     = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Affichage
print(carte_chaleur)

ggsave("figure_heatmap.png", carte_chaleur, width = 10, height = 4, dpi = 150)



###### TEST DE SENSIBILITE ######


# --- Cas de référence ---
ref <- bilan_sol(as.Date("2023-09-01"), sol = "limono_sableux")
stats_ref <- stats_stress(ref)

# --- Test 1 / Profondeur racinaire : Zr (0.6, 0.9, 1.2) ---
sens_zr <- data.frame()
for (zr in c(0.6, 0.9, 1.2)) {
  df <- bilan_sol(as.Date("2023-09-01"), sol = "limono_sableux", Zr = zr)
  stats <- stats_stress(df)
  stats$Zr <- zr
  sens_zr <- rbind(sens_zr, stats)
}
cat("\n=== Sensibilité à la profondeur racinaire ===\n")
print(sens_zr %>% select(Zr, Jours_stress, Ratio_ETc, Drainage_total))

# --- Test 2 / déficit en eau du sol : Dr0 (0%, 50%, 75% de TAW) ---
sens_dr0 <- data.frame()
for (dr in c(0, 0.5, 0.75)) {
  df <- bilan_sol(as.Date("2023-09-01"), sol = "limono_sableux", Dr0_frac = dr)
  stats <- stats_stress(df)
  stats$Dr0_frac <- dr
  sens_dr0 <- rbind(sens_dr0, stats)
}
cat("\n=== Sensibilité au déficit en eau du sol ===\n")
print(sens_dr0 %>% select(Dr0_frac, Jours_stress, Ratio_ETc, Drainage_total))

# --- Test 3 / Ruissellement r (0%, 10%, 20%) ---
sens_ruiss <- data.frame()
for (r in c(0, 0.10, 0.20)) {
  df <- bilan_sol(as.Date("2023-09-01"), sol = "limono_sableux", fraction_ruissellement = r)
  stats <- stats_stress(df)
  stats$Ruissellement <- r
  sens_ruiss <- rbind(sens_ruiss, stats)
}
cat("\n=== Sensibilité au ruissellement ===\n")
print(sens_ruiss %>% select(Ruissellement, Jours_stress, Ratio_ETc, Drainage_total))

# --- Affichage des 3 tests de sensibilités ---

sens_zr_resume <- sens_zr %>%
  mutate(parametre = "Zr (m)", valeur = as.character(Zr)) %>%
  select(parametre, valeur, Jours_stress, Ratio_ETc, Drainage_total)

sens_dr0_resume <- sens_dr0 %>%
  mutate(parametre = "Dr0 (fraction TAW)", valeur = paste0(Dr0_frac * 100, " %")) %>%
  select(parametre, valeur, Jours_stress, Ratio_ETc, Drainage_total)

sens_ruiss_resume <- sens_ruiss %>%
  mutate(parametre = "Ruissellement", valeur = paste0(Ruissellement * 100, " %")) %>%
  select(parametre, valeur, Jours_stress, Ratio_ETc, Drainage_total)

sensibilite_resume <- rbind(sens_zr_resume, sens_dr0_resume, sens_ruiss_resume)

cat("\n=== RÉSUMÉ DES TESTS DE SENSIBILITÉ ===\n")
print(sensibilite_resume)
write.csv(sensibilite_resume, "sensibilite_resume.csv", row.names = FALSE)



####### Scénario prudent ######

resultats_60_prudent <- data.frame()

for (i in 1:nrow(combinaisons)) {
  
  annee  <- combinaisons$annee[i]
  saison <- combinaisons$saison[i]
  sol    <- combinaisons$sol[i]
  
  mois_jour  <- ifelse(saison == "Avril", "04-01", "09-01")
  date_semis <- as.Date(paste0(annee, "-", mois_jour))
  
  dr0_frac <- ifelse(saison == "Avril", 0.5, 0.3)
  
  df <- bilan_sol(date_semis, sol = sol, 
                  Zr = 0.9, 
                  Dr0_frac = dr0_frac, 
                  fraction_ruissellement = 0.10)
  
  if (is.null(df)) next
  
  stats <- stats_stress(df)
  stats$annee  <- annee
  stats$saison <- saison
  stats$sol    <- sol
  
  resultats_60_prudent <- rbind(resultats_60_prudent, stats)
}


# --- Harmoniser les deux tableaux ---

# Scénario de référence
resultats_60$scenario <- "Référence"

# Scénario prudent
resultats_60_prudent$scenario <- "Prudent"

# Choisissons que les colonnes communes aux deux scenario 
colonnes_communes <- c("annee", "saison", "sol", 
                       "Jours_stress", "etc_reelle", "etc_potentielle", 
                       "Ratio_ETc", "Drainage_total", "scenario")

resultats_60         <- resultats_60         %>% select(all_of(colonnes_communes))
resultats_60_prudent <- resultats_60_prudent %>% select(all_of(colonnes_communes))

# Vérifions que les colonnes sont identiques
setdiff(names(resultats_60), names(resultats_60_prudent))   #  character(0)
setdiff(names(resultats_60_prudent), names(resultats_60))   #  character(0)

# fusionnons les deux scénario ensemble 
comparaison <- rbind(resultats_60, resultats_60_prudent)
nrow(comparaison)               # 120 / vérification
table(comparaison$scenario)     

write.csv(comparaison, "comparaison.csv", row.names = FALSE)


##### Tableau récapitulatif (moyennes par scénario × saison × sol)#####

comparaison_resume <- comparaison %>%
  group_by(scenario, saison, sol) %>%
  summarise(
    Jours_stress_moy = mean(Jours_stress, na.rm = TRUE),
    Ratio_ETc_moy    = mean(Ratio_ETc, na.rm = TRUE),
    Drainage_moy     = mean(Drainage_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(saison, sol, scenario)

print(comparaison_resume, n = 20)
write.csv(comparaison_resume, "comparaison_resume.csv", row.names = FALSE)


p_comparaison <- ggplot(comparaison_resume, 
                        aes(x = saison, y = Jours_stress_moy, fill = scenario)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = round(Jours_stress_moy, 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3.5) +
  facet_wrap(~ sol, ncol = 3,
             labeller = labeller(sol = c(
               "sableux"        = "Sableux",
               "limono_sableux" = "Limono-sableux",
               "argileux"       = "Argileux"
             ))) +
  scale_fill_manual(values = c("Référence" = "steelblue", "Prudent" = "orange")) +
  labs(
    title    = "Impact du scénario prudent sur le stress hydrique",
    subtitle = "Nombre moyen de jours de stress sur 10 ans (2015-2024)",
    x        = "Saison", y = "Jours de stress (moyenne sur 10 ans)", fill = "Scénario"
  ) +
  theme_minimal()

print(p_comparaison)
ggsave("figure_comparaison_scenarios.png", plot = p_comparaison, 
       width = 10, height = 4, dpi = 150)


#### Zoom 2015 ####
# Scénario du mois d'Avril pour un sol sableux

df_ref_2015 <- bilan_sol(as.Date("2015-04-01"), sol = "sableux")
df_pru_2015 <- bilan_sol(as.Date("2015-04-01"), sol = "sableux", 
                         Zr = 0.9, Dr0_frac = 0.5, fraction_ruissellement = 0.10)
df_ref_2015$scenario <- "Référence"
df_pru_2015$scenario <- "Prudent"
df_compare <- rbind(
  df_ref_2015 %>% select(jour_depuis_semis, Dr, scenario, TAW, RAW),
  df_pru_2015 %>% select(jour_depuis_semis, Dr, scenario, TAW, RAW)
)
zoom_2015 <- ggplot(df_compare, aes(x = jour_depuis_semis, y = Dr, color = scenario)) +
  geom_line(linewidth = 1.2) +
  geom_hline(data = df_ref_2015, aes(yintercept = unique(TAW)), 
             color = "steelblue", linetype = "dashed", linewidth = 0.8) +
  geom_hline(data = df_ref_2015, aes(yintercept = unique(RAW)), 
             color = "steelblue", linetype = "dotted", linewidth = 0.8) +
  geom_hline(data = df_pru_2015, aes(yintercept = unique(TAW)), 
             color = "orange", linetype = "dashed", linewidth = 0.8) +
  geom_hline(data = df_pru_2015, aes(yintercept = unique(RAW)), 
             color = "orange", linetype = "dotted", linewidth = 0.8) +
  annotate("text", x = 5, y = unique(df_ref_2015$TAW), 
           label = paste0("TAW réf. = ", round(unique(df_ref_2015$TAW), 1), " mm"),
           hjust = 0, vjust = -0.5, color = "steelblue", size = 3.5) +
  annotate("text", x = 5, y = unique(df_ref_2015$RAW), 
           label = paste0("RAW réf. = ", round(unique(df_ref_2015$RAW), 1), " mm"),
           hjust = 0, vjust = -0.5, color = "steelblue", size = 3.5) +
  annotate("text", x = 5, y = unique(df_pru_2015$TAW), 
           label = paste0("TAW prudent = ", round(unique(df_pru_2015$TAW), 1), " mm"),
           hjust = 0, vjust = -0.5, color = "orange", size = 3.5) +
  annotate("text", x = 5, y = unique(df_pru_2015$RAW), 
           label = paste0("RAW prudent = ", round(unique(df_pru_2015$RAW), 1), " mm"),
           hjust = 0, vjust = -0.5, color = "orange", size = 3.5) +
  scale_color_manual(values = c("Référence" = "steelblue", "Prudent" = "orange")) +
  labs(
    title    = "Impact du scénario prudent - Avril 2015, sol sableux",
    subtitle = "Référence : 17 jours de stress (ratio 0.92) · Prudent : 29 jours (ratio 0.80)",
    caption  = "Note : chaque courbe se lit par rapport à son propre RAW, pas au RAW de l'autre scénario.",
    x        = "Jours après semis", y = "Déficit Dr (mm)", color = "Scénario"
  ) +
  theme_minimal()

print(zoom_2015)
ggsave("figure_zoom_2015_sableux.png", plot = zoom_2015, width = 9, height = 5, dpi = 150)


### Tableaau des pires cas ###
pires_cas <- comparaison %>%
  group_by(scenario, saison, sol) %>%
  summarise(
    Ratio_moy  = mean(Ratio_ETc, na.rm = TRUE),
    Ratio_min  = min(Ratio_ETc, na.rm = TRUE),
    Annee_pire = ifelse(Ratio_min < 1, annee[which.min(Ratio_ETc)], NA),
    N_stress10 = sum(Jours_stress >= 10, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(saison, sol, scenario)

print(pires_cas, n = 20)
write.csv(pires_cas, "pires_cas.csv", row.names = FALSE)


#### tableau des écart entre les deux scénario ####
comparaison_ecart <- comparaison_resume %>%
   pivot_wider(
    id_cols     = c(saison, sol),      # ← identifiants
    names_from  = scenario,             # ← devient "Prudent" et "Référence"
    values_from = c(Jours_stress_moy, Ratio_ETc_moy)
  ) %>%
  mutate(
    Delta_Jours = Jours_stress_moy_Prudent - Jours_stress_moy_Référence,
    Delta_Ratio = Ratio_ETc_moy_Prudent - Ratio_ETc_moy_Référence
  ) %>%
  select(saison, sol, Delta_Jours, Delta_Ratio) %>%
  arrange(saison, sol)

print(comparaison_ecart, n = 20)
write.csv(comparaison_ecart, "comparaison_ecart.csv", row.names = FALSE)


### STRESS PAR STADE 2015-2024 ###

stress_stades_resultats <- data.frame()

for (annee_nom in 2015:2024) {
  for (saison_nom in c("Avril", "Septembre")) {
    for (sol_nom in c("sableux", "limono_sableux", "argileux")) {
      for (scenario_nom in c("Référence", "Prudent")) {
        
        mois_jour <- ifelse(saison_nom == "Avril", "04-01", "09-01")
        date_semis <- as.Date(paste0(annee_nom, "-", mois_jour))
        
        if (scenario_nom == "Référence") {
          dr0_frac <- 0
          zr <- 1.2
          ruiss <- 0
        } else {
          dr0_frac <- ifelse(saison_nom == "Avril", 0.5, 0.3)
          zr <- 0.9
          ruiss <- 0.10
        }
        
        df <- bilan_sol(date_semis, sol = sol_nom,
                        Zr = zr, Dr0_frac = dr0_frac, 
                        fraction_ruissellement = ruiss)
        
        if (is.null(df)) next
        
        stats_stade <- stress_par_stade(df)
        stats_stade$annee    <- annee_nom
        stats_stade$saison   <- saison_nom
        stats_stade$sol      <- sol_nom
        stats_stade$scenario <- scenario_nom
        
        stress_stades_resultats <- rbind(stress_stades_resultats, stats_stade)
      }
    }
  }
}

# Moyenne par stade × saison × sol × scénario
# --- Stress par stade : avril ET septembre ---
stress_stades_moyen <- stress_stades_resultats %>%
  group_by(scenario, saison, sol, stade) %>%
  summarise(
    Jours_stress_moy = mean(Jours_stress, na.rm = TRUE),
    Ratio_ETc_moy    = mean(Ratio_ETc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(stade = factor(stade, 
                        levels = c("Initial", "Développement", "Mi-saison", "Fin")))

p_stress_stade <- stress_stades_moyen %>%
  ggplot(aes(x = stade, y = Jours_stress_moy, fill = scenario)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = round(Jours_stress_moy, 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.4, size = 2.5) +
  facet_grid(saison ~ sol, 
             labeller = labeller(
               sol = c("sableux"        = "Sableux",
                       "limono_sableux" = "Limono-sableux",
                       "argileux"       = "Argileux")
             )) +
  scale_y_continuous(limits = c(0, 17), expand = c(0, 0)) +
  scale_fill_manual(values = c("Référence" = "steelblue", "Prudent" = "orange")) +
  scale_x_discrete(labels = c("Initial" = "Init.", "Développement" = "Dév.",
                              "Mi-saison" = "Mi-saison", "Fin" = "Fin")) +
  labs(
    title    = "Jours de stress par stade - Avril et Septembre (2015–2024)",
    subtitle = "Avril : stress en mi-saison (floraison) · Septembre : stress en fin de cycle",
    x        = "Stade", y = "Jours de stress (moyenne sur 10 ans)", fill = "Scénario"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,color ="black"),
        strip.text.y = element_text(size = 12, face = "bold"),
        axis.line.x = element_line(color = "black", linewidth = 0.6),
        axis.line.y = element_line(color = "black", linewidth = 0.6))

print(p_stress_stade)
ggsave("figure_stress_par_stade.png", plot = p_stress_stade, 
       width = 12, height = 7, dpi = 150)
