Bilan hydrique du maïs à Lomé (Togo), 2015–2024
Quand, et dans quels sols, une culture de maïs manque-t-elle d'eau à Lomé ?
Première approche simplifiée, réalisée en autodidacte. Les résultats sont à interpréter avec prudence (voir Limites).

Résumé

Le projet propose un bilan hydrique journalier du sol, sur dix ans (2015–2024), avec des données satellitaires NASA POWER et la méthode FAO-56 :
1.	ET0 : Penman-Monteith FAO-56, codée à la main et validée contre le paquet R Evapotranspiration.
2.	ETc : courbe de coefficient cultural (Kc) en quatre stades pour un maïs de cycle court (90 jours), semé le 1er avril ou le 1er septembre.
3.	Réservoir de sol : réserve utile totale (TAW), réserve facilement utilisable (RAW) et coefficient de stress (Ks).
4.	Comparaison de trois sols (sableux, limono-sableux, argileux) et de deux scénarios (référence et prudent), avec des tests de sensibilité.

Principaux résultats

•	La petite saison (semis en septembre) est plus risquée que celle d'avril, et le sol sableux est le plus vulnérable : en septembre, il connaît du stress 8 années sur 10 en référence.
•	Le sol joue un rôle de tampon : en septembre, 13,3 jours de stress en moyenne en sol sableux, 5,0 en limono-sableux, 3,3 en argileux (référence).
•	Le moment du stress diffère : en fin de cycle en septembre, en mi-saison (floraison) en avril.
•	Le scénario prudent augmente le stress partout (de +2,8 à +8,0 jours en moyenne) sans changer le classement des sols ni des saisons.


Exécution

Prérequis : R et les paquets suivants: 
    install.packages(c("nasapower", "dplyr", "ggplot2", "tidyr", "tibble", "Evapotranspiration"))

Ordre	Script	Rôle	Principales sorties

1.	1_DonneeNASA_ET0.R : Télécharge NASA POWER (connexion internet nécessaire), calcule l'ET0 et les fichiersmeteo_lome_ET0.rds, meteo_lome_ET0.csv, bilan_mensuel.csv, figure_bilan_mensuel.png, figure_ET0_2023.png
2.	2_ETC.R	: Courbe Kc, ETc, bilan pluie − ETc sur le cycle	figure_courbe_Kc.png, etc_avril_2023.csv, etc_sept_2023.csv, figure_ETc_pluie_avril.png, figure_ETc_pluie_septembre.png, bilan_annuel.csv, figure_bilan_annuel.png
3.	3_BILAN_SOL.R	: Réservoir de sol, 2 scénarios × 3 sols × 10 ans × 2 saisons, sensibilité	resultats_60.csv, comparaison.csv, comparaison_resume.csv, comparaison_ecart.csv, pires_cas.csv, sensibilite_resume.csv, stress_par_stade_moyen.csv, figure_heatmap.png, figure_comparaison_scenarios.png, figure_Dr_septembre_2023.png, figure_zoom_2015_sableux.png, figure_stress_par_stade.png
4.	0_validation_ET0.R :	Valide l'ET0 contre le paquet de référence (télécharge aussi 2023)	validation_ET0.csv

0_fonction_ET0.R n'est pas à lancer seul : il est chargé par les autres scripts.

Sans connexion internet : le fichier meteo_lome_ET0.rds est fourni, ce qui permet de relancer directement les scripts 2 et 3.

Données

•	Source : NASA POWER, données journalières, cellule de grille de Lomé (6,17 °N ; 1,23 °E), du 2015-01-01 au 2024-12-31 (3 653 jours, sans valeur manquante).

•	Variables: T2M_MAX, T2M_MIN (°C), RH2M (%), WS2M (m/s à 2 m), ALLSKY_SFC_SW_DWN (rayonnement, MJ/m²/jour), PRECTOTCORR (pluie, mm/jour).

•	Altitude : 23 m, valeur supposée pour Lomé.

•	Remarque sur la pluie: moyenne de 1 264 mm/an (de 860 à 1 517 mm), probablement élevée pour Lomé , avec une valeur extrême de 174,7 mm en un jour en 2020, non vérifiée.

Méthode

ET0 (FAO-56) :
ET0 = [0,408*Δ*(Rn − G) + γ*900/(T + 273)*u2*(es − ea)] / [Δ + γ*(1 + 0,34*u2)]

Les termes intermédiaires (pression, γ, es, ea, Δ, rayonnement extraterrestre Ra, Rns, Rnl) suivent le chapitre 3 de FAO-56.
ETc : ETc = Kc × ET0, avec une courbe Kc linéaire par morceaux (initial, développement, mi-saison, fin).

Bilan du sol (déficit en eau de la zone racinaire Dr, en mm) :
TAW = 1000 *(θcc - θpf)* Zr          RAW = p * TAW
Ks  = 1                               si Dr ≤ RAW
Ks  = (TAW - Dr) / ((1 - p) · TAW)    si Dr > RAW
ETc ajustée = Ks * Kc * ET0
Dr(j) = Dr(j−1) - (P - ruissellement) - irrigation + ETc ajustée + drainage
Indicateurs : nombre de jours de stress (Ks < 1) et ratio ETc réelle / ETc potentielle, par saison, sol, année et stade.

Paramètres

Kc initial / mi-saison / fin	0,30 / 1,20 / 0,35	FAO-56, tableau 12 (Kc de fin : récolte après séchage du grain au champ)
Durée des stades	15 / 25 / 30 / 20 jours (cycle de 90 jours)	Hypothèse de travail pour un cycle court, proportions proches de FAO-56 (tableau 11) ; à adapter à une variété locale
Fraction d'épuisement p	0,55	FAO-56, tableau 22 (maïs)
Zr	1,2 m (référence), 0,9 m (prudent)	FAO-56, tableau 22 : 1,0 à 1,7 m pour le maïs ; 0,9 m est volontairement en dessous (cycle court, enracinement limité)
θcc / θpf, sableux	0,12 / 0,045	Valeurs génériques inspirées de FAO-56, tableau 19
θcc / θpf, limono-sableux	0,23 / 0,11	idem
θcc / θpf, argileux	0,36 / 0,22	idem
TAW (Zr = 1,2 m)	90 / 144 / 168 mm	Calculée (sableux / limono-sableux / argileux)
Ruissellement	0 % (référence), 10 % (prudent)	Hypothèse (fraction fixe de la pluie)
Déficit initial Dr0	0 (référence) ; 50 % de TAW en avril et 0 en septembre (prudent)	Hypothèse
Dates de semis	1er avril et 1er septembre	Hypothèse (grande et petite saison)
Altitude	23 m	Valeur supposée

Validation
•	ET0 (0_validation_ET0.R, Lomé, 2023, 365 jours) contre le paquet Evapotranspiration : biais +0,0005, RMSE 0,0005, écart maximal 0,0009 mm/jour (environ 0,02 %). Détail dans validation_ET0.csv.
•	Bilan du sol : les sorties ont aussi été recalculées par une réimplémentation indépendante (hors R, non incluse dans ce dépôt) : écarts numériques nuls (de l'ordre de 10⁻¹²).
•	Aucune validation sur des mesures d'humidité du sol ou de rendement.

Limites

•	La pluie NASA POWER (données de grille) n'a pas été comparée à des stations ; sa moyenne est probablement élevée pour Lomé, et la valeur extrême de 2020 n'est pas vérifiée.
•	Les paramètres de sol sont génériques, pas ceux d'une terre de barre mesurée.
•	Dates de semis fixes, cycle de 90 jours, Zr constant sur le cycle, ruissellement en fraction fixe, Kc standard non ajusté au climat local.
•	Aucune calibration ni validation sur des mesures ; sensibilité testée sur un seul cas ; un seul site.
•	Modèle très simplifié par rapport à un modèle de surface complet.

Perspectives

1.	Remplacer la pluie de grille par des données de station.
2.	Estimer les propriétés hydriques des sols de la zone (terre de barre) à partir de la granulométrie, de la densité apparente et du carbone organique.
3.	Faire croître la profondeur racinaire avec le cycle.
4.	Tester des scénarios climatiques simples et une règle d'irrigation.
5.	Passer à un modèle de surface de type CLASSIC et à des sols forestiers.

Usage de l'IA

Le code R a été écrit en autodidacte, avec l'aide d'outils d'IA utilisés comme assistants (explications, débogage, relecture). Chaque étape du modèle peut être expliquée.

Références

•	Allen, R.G., Pereira, L.S., Raes, D., Smith, M. (1998). Crop evapotranspiration — Guidelines for computing crop water requirements. FAO Irrigation and Drainage Paper 56. Rome : FAO.
•	Doorenbos, J., Kassam, A.H. (1979). Yield response to water. FAO Irrigation and Drainage Paper 33. Rome : FAO.
•	NASA Langley Research Center, POWER Project. Prediction Of Worldwide Energy Resources. https://power.larc.nasa.gov (données consultées en 2026).
•	Paquets R (CRAN) : nasapower, Evapotranspiration, dplyr, ggplot2, tidyr, tibble.

Licence

Projet sous licence MIT.

Contact

SEPENOU Dégbey Adjété Emmanuel — adjetesepenou@gmail.com — https://github.com/adjetesepenou-git/Bilan_hydrique_lome_2015-2026.git
