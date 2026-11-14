### Fichier à ne pas exécuter ###

### Ce fichier stocke tous les fonctions indispensables pour les calculs
### Il se charge automatiquement dans les autres fichiers 

library(dplyr)

### FAO56 evapotranspiration potentiel

et0_fao <- function(T2M_MAX, T2M_MIN, RH2M, WS2M,
           Rs, lat, elev, doy, G = 0){
  
  #...constantes physiques
  sigma <- 4.903e-9    # Constante de Stefan-Boltzmann [MJ K-4 m-2 j-1]
  Gsc   <- 0.0820       # Constante solaire            [MJ m-2 min-1]
  albedo <- 0.23       # Albedo de l'herbe de référence
  

  #...Température Moyenne 
  tmean <- (T2M_MAX + T2M_MIN)/2
  
  #...Pression atmospherique (kPa)
  P  <- 101.3*((293-0.0065*elev)/293)^5.26
  
  # --- Constante psychrométrique gamma [kPa/°C] ---
  gamma <- 0.000665 * P
  
  #...Pression de vapeur saturante à la température moyenne kPa
  es_tmean <- 0.6108*exp((17.27*tmean)/(tmean+237.3))
  
  #...Pression de vapeur saturante à Tmax et Tmin (kPa)
  es_tmax  <- 0.6108*exp((17.27*T2M_MAX)/(T2M_MAX+237.3))
  es_tmin  <- 0.6108*exp((17.27*T2M_MIN)/(T2M_MIN+237.3))
  
  #...Pression de la vapeur saturante moyenne (kPa)
  
  es  <- (es_tmax+es_tmin)/2
  
  #...Pression de vapeur réelle (kPa)
  ea  <- es*(RH2M / 100)
  
  #...Pente de la courbe de vapeur saturante kPa/°C
  delta <- (4098*es_tmean)/(tmean+237.3)^2
  
  #...Rayonnement extraterrestre Ra [MJ/m²/jour]
  phi  <- lat*pi/180 #latitude en radian
  dr  <- 1+0.033*cos(2*pi*doy/365) #distance terre soleil
  decl <- 0.409*sin(2*pi*doy/365-1.39) #déclinaison solaire 
  ws  <- acos(-tan(phi)*tan(decl)) #angle horaire au couché du soleil
  Ra  <- (24*60/pi)*Gsc*dr*(ws*sin(phi)*sin(decl)+cos(phi)*cos(decl)*sin(ws))

  #...Rayonneemnt solaire net (MJ/m²/jour)
  Rns  <- (1-albedo)*Rs
  
  #...Rayonnement par ciel claire
  Rso  <- (0.75+2e-5*elev)*Ra
  
  #...Rayonnement net longueur d'onde
  Rnl  <- sigma*((T2M_MAX+273.16)^4+(T2M_MIN+273.16)^4)/2*
    (0.34-0.14*sqrt(ea))*
    (1.35*pmin(Rs/Rso,1)-0.35)
  
  #...Rayonnement net total 
  Rn  <- Rns-Rnl
  
  #...Numérateur et dénominateur de Penman-Monteith
  num  <- 0.408*delta*(Rn-G)+gamma*(900/(tmean+273))*WS2M*(es-ea)
  den  <- delta+gamma*(1+0.34*WS2M)
  
  #...ET0 (mm/jr)
  ET0 <- num/den
    
  #...Plafonnement : ET0 négatif donne 0
  ET0 <- pmax(ET0, 0)
 
  return(ET0)
}




###   Kc DU MAIS   ###

kc_mais <- function(jour_depuis_semis, duree_cycle=90,
                    d_ini=15, d_dev=25,d_mid=30, d_fin=20,
                    kc_ini=0.30, kc_mid=1.2, kc_fin=0.35){
  ### Vérification des données entrées
  stopifnot(d_ini+d_dev+d_mid+d_fin == duree_cycle)
  
  kc <- numeric(length(jour_depuis_semis)) # création de la taille de la boucle
  
  fin_ini <- d_ini
  fin_dev <- d_ini + d_dev
  fin_mid <- d_ini + d_dev + d_mid
  
  for(i in seq_along(jour_depuis_semis)){
    j <- jour_depuis_semis [i] # i est une variable d'itération
    
    if (is.na(j) || j < 1 || j > duree_cycle) {
      kc[i] <- NA #  Si j est manquant, OU plus petit que 1, OU plus grand que 90, alors Kc = NA
    } else if (j <= fin_ini){
      kc[i] <- kc_ini
    } else if (j <= fin_dev){
      kc[i] <- kc_ini + (kc_mid - kc_ini)*(j - fin_ini) / d_dev
    } else if (j <= fin_mid){
      kc[i] <- kc_mid
    } else {
      kc[i] <- kc_mid + (kc_fin - kc_mid)*(j - fin_mid) / d_fin
    }
    
  }
  
  return(kc)
}



###   EVAPOTRANSPIRATION CULTURAL   ###

etc_saison <- function(date_semis, duree_cycle = 90){
  
  #date de fin de cycle
  date_fin  <- date_semis + duree_cycle - 1
  
  # Filtrer sur les date de semis et de fin de cycle
  resultat  <- meteo %>%
    filter(YYYYMMDD >= date_semis, YYYYMMDD  <= date_fin) %>%
    mutate(
      jour_depuis_semis = as.integer(YYYYMMDD - date_semis) + 1,
      Kc = kc_mais(jour_depuis_semis, duree_cycle),
      ETc = Kc * ET0
    )
  return(resultat)
}




### Calcul de ETc par stade sur les differents années ###

bilan_stade_saison <- function(date_semis, duree_cycle = 90) {
  
  # ETc par cycle
  etc <- etc_saison(date_semis, duree_cycle) 
  
  # Contrôle : cycle complet 
  if (nrow(etc) != duree_cycle) {
    warning(paste("Cycle incomplet pour", date_semis,
                  ":", nrow(etc), "jours sur", duree_cycle))
    return(NULL)
  }
  
  etc <-  etc %>%
    mutate(
      stade = case_when(
        jour_depuis_semis <= 15 ~ "Initial",
        jour_depuis_semis <= 40 ~ "Développement",
        jour_depuis_semis <= 70 ~ "Mi-saison",
        TRUE                    ~ "Fin"
      )
    )
  
  # Bilan par stade
  bilan_stade <- etc %>%
    group_by(stade) %>%
    summarise(
      ETc_stade   = sum(ETc, na.rm = TRUE),
      Pluie_stade = sum(PRECTOTCORR, na.rm = TRUE),
      Bilan       = Pluie_stade - ETc_stade,
      N_jours     = n(),
      .groups = "drop"
    ) %>%
    mutate(
      date_semis = date_semis,
      annee      = as.integer(format(date_semis, "%Y")),
      saison     = ifelse(format(date_semis, "%m") == "04", "Avril", "Septembre")
    ) %>%
    
    arrange(factor(stade, levels = c("Initial", "Développement", "Mi-saison", "Fin")))
  
  return(bilan_stade)
  
}




### Fonction pour calculer le bilan d'une année ###

bilan_annee <- function(annee, mois_jour) {
  date_semis <- as.Date(paste0(annee, "-", mois_jour))
  etc <- etc_saison(date_semis)
  
  if (nrow(etc) != 90) {
    warning(paste("Cycle incomplet pour", annee, mois_jour, 
                  ": seulement", nrow(etc), "jours sur 90"))
    return(NULL)   
  }
  
  data.frame(
    annee        = annee,
    saison       = ifelse(mois_jour == "04-01", "Avril", "Septembre"),
    ETc_cumul    = sum(etc$ETc, na.rm = TRUE),
    Pluie_cumul  = sum(etc$PRECTOTCORR, na.rm = TRUE),
    Bilan        = sum(etc$PRECTOTCORR, na.rm = TRUE) - sum(etc$ETc, na.rm = TRUE)
  )
}




### Fonction du bilan du sol ###
## les données utilisé sont des références du Tableau 19 de FAO56

bilan_sol <- function(date_semis, sol = "limono_sableux",
                      fraction_ruissellement = 0,Zr = 1.2,
                      Dr0_frac = 0, duree_cycle = 90) {
  
  ## Sol FAO56
  sols <- list(
    sableux = list(theta_cc = 0.12, theta_pf = 0.045),
    limono_sableux = list(theta_cc = 0.23, theta_pf = 0.11),
    argileux = list(theta_cc = 0.36, theta_pf = 0.22)
  )
  
  ## paramètre du sol ps
  ps <- sols[[sol]]
  theta_cc <- ps$theta_cc
  theta_pf <- ps$theta_pf
  
  ## paramètre du maïs 
  p <- 0.55
  
  ## capacité de stockage en eau du sol
  
  TAW <- 1000 * (theta_cc - theta_pf) * Zr
  RAW <- p * TAW
  
  # Convertir la fraction en mm
  Dr0 <- Dr0_frac * TAW
  
  ## Données ETc du cycle
  
  etc <- etc_saison(date_semis,duree_cycle)
  if (nrow(etc) != duree_cycle){
    warning("Cycle incomplet")
    return(NULL)
  }
  
  n <- nrow(etc)
  Dr <- numeric(n)
  Ks <- numeric(n)
  etc_adj <- numeric(n)
  Drainage <- numeric(n)
  Dr[1] <- Dr0 
  
  ## Boucle jour par jour
  for(i in 1:n){
    
    #plue efficace 
    pluie_eff <- etc$PRECTOTCORR[i] * (1 - fraction_ruissellement)
    
    #Etc potentielle 
    etc_pot <- etc$ETc[i]
    
    #Dr de la veille 
    Dr_veille <- if(i==1) Dr[1] else Dr[i-1]
    
    #Ks
    if (Dr_veille <= RAW) {
      Ks[i] <- 1
    } else{
      Ks[i] <- max(0, (TAW - Dr_veille) / ((1-p) * TAW)) 
    }
    
    #ETc ajsuté
    etc_adj[i]  <- Ks[i] * etc_pot
    
    #Bilan
    Dr_i <- Dr_veille - pluie_eff + etc_adj[i]
    
    # Drainage
    if (Dr_i < 0){
      Drainage[i]  <- -Dr_i
      Dr_i <- 0
    } else {
      Drainage[i] <- 0
    }
    
    Dr[i]  <- min(max(Dr_i, 0), TAW)
  }
  
  ## resultats 
  etc$Dr       <- Dr
  etc$Ks       <- Ks
  etc$etc_adj  <- etc_adj
  etc$Drainage <- Drainage
  etc$TAW      <- TAW
  etc$RAW      <- RAW
  
  # colonne stade
  etc <- etc %>%
    mutate(
      stade = case_when(
        jour_depuis_semis <= 15 ~ "Initial",
        jour_depuis_semis <= 40 ~ "Développement",
        jour_depuis_semis <= 70 ~ "Mi-saison",
        TRUE                    ~ "Fin"
      )
    )
  
  return(etc)
  
}                   





### Fonction stress par stade de devellopement du maïs ###
stress_par_stade <- function(df) {
  df %>%
    mutate(
      stade = factor(stade, 
                     levels = c("Initial", "Développement", "Mi-saison", "Fin"))
    ) %>%
    group_by(stade) %>%
    summarise(
      Jours_stress    = sum(Ks < 1, na.rm = TRUE),
      ETc_reelle      = sum(etc_adj, na.rm = TRUE),
      ETc_potentielle = sum(ETc, na.rm = TRUE),
      Ratio_ETc       = ETc_reelle / ETc_potentielle,
      .groups = "drop"
    )
}



### ....statistique de stress....###

stats_stress <- function(df) {
  data.frame(
    Jours_stress    = sum(df$Ks < 1, na.rm = TRUE),
    etc_reelle      = sum(df$etc_adj, na.rm = TRUE),
    etc_potentielle = sum(df$ETc, na.rm = TRUE),
    Ratio_ETc       = sum(df$etc_adj, na.rm = TRUE) / sum(df$ETc, na.rm = TRUE),
    Drainage_total  = sum(df$Drainage, na.rm = TRUE)
  )
}
