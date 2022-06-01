# Création des tableaux et variables en rapport avec l'artisticité --------------------------------------------------

# Création d'une autre base pour plus de facilités
baseprimale3<-baseprimale 

# Recodage numérique de la variable indispensable, interchangeable
baseprimale3$indis_inter<-baseprimale3$G04Q48..Dans.votre.métier..vous.vous.considérez.comme......
baseprimale3$indis_inter<-fct_collapse(baseprimale3$indis_inter, "1"="Indispensable (vous êtes nécessaire au projet)", "0"="Interchangeable (quelqu'un d'autre pourrait faire votre métier à votre place)", "2"="Aucun des deux")

##Création de la variable d'artisticité--------------------------------------------------
##

# Renommer les variables nécessaires à la variable scorée pour qu'elles prennent moins de place 
baseprimale3$jeu_art <-baseprimale3$G04Q44..Pensez.vous.que.le.jeu.vidéo.est.un.art...
baseprimale3$G04Q79.SQ001<-baseprimale3$G04Q79.SQ001...En.dehors.de.votre.temps.de.travail..réalisez.vous.des.projets.personnels.en.rapport.avec.le.jeu.vidéo.....
baseprimale3$G01Q51.SQ004 <- baseprimale3$G01Q51.SQ004...Demandez.vous.l.avis.de.vos.pairs.sur.vos.travaux.en.entreprise.....Fréquence.
baseprimale3$G04Q120.SQ001 <- baseprimale3$G04Q120.SQ001...Demandez.vous.l.avis.de.vos.pairs.sur.vos.travaux.personnels.....Fréquence..

# Création de la variable d'artisticité avec un score de 0 initial 
baseprimale3$artisticité <- 0

# Ajout d'un score en fonction de la variable artiste_artisan
## artiste +5, artisan +3, aucun+0
baseprimale3$artiste_artisan[is.na(baseprimale3$artiste_artisan)] = "Aucun"
baseprimale3$artisticité = ifelse(
  baseprimale3$artiste_artisan == "Artiste", 
  baseprimale3$artisticité + 5, baseprimale3$artisticité)
baseprimale3$artisticité = ifelse(
  baseprimale3$artiste_artisan  == "Artisan", 
  baseprimale3$artisticité + 3, baseprimale3$artisticité)
baseprimale3$artisticité = ifelse(
  baseprimale3$artiste_artisan == "Aucun", 
  baseprimale3$artisticité + 0, baseprimale3$artisticité)


# Ajout d'un score en fonction de la signification qu'ils donnent à leur travail
## Portée esthétique +5, démonstration savoir faire+3, portée fonctionnelle +1, impact carière+0
baseprimale3$travail_art[is.na(baseprimale3$travail_art)] = "Impact sur la carrière"
baseprimale3$artisticité = ifelse(
  baseprimale3$travail_art == "Portée esthétique", 
  baseprimale3$artisticité + 5, baseprimale3$artisticité)
baseprimale3$artisticité = ifelse(
  baseprimale3$travail_art == "Portée fonctionnelle", 
  baseprimale3$artisticité + 1, baseprimale3$artisticité)
baseprimale3$artisticité = ifelse(
  baseprimale3$travail_art == "Démonstration savoir faire", 
  baseprimale3$artisticité + 3, baseprimale3$artisticité)
baseprimale3$artisticité = ifelse(
  baseprimale3$travail_art == "Impact sur la carrière", 
  baseprimale3$artisticité + 0, baseprimale3$artisticité)


#Application d'un score en fonction de la réalisation de projets extérieurs 
## Toujours +5, Souvent +3, Assez peu +1, Jamais+0

baseprimale3$G04Q79.SQ001[is.na(baseprimale3$G04Q79.SQ001)] = "Jamais"
baseprimale3$artisticité = ifelse(
  baseprimale3$G04Q79.SQ001== "Jamais", 
  baseprimale3$artisticité + 0, baseprimale3$artisticité)
baseprimale3$artisticité = ifelse(
  baseprimale3$G04Q79.SQ001 == "Assez peu", 
  baseprimale3$artisticité + 1, baseprimale3$artisticité)
baseprimale3$artisticité = ifelse(
  baseprimale3$G04Q79.SQ001 == "Souvent", 
  baseprimale3$artisticité + 3, baseprimale3$artisticité)
baseprimale3$artisticité = ifelse(
  baseprimale3$G04Q79.SQ001 == "Toujours", 
  baseprimale3$artisticité + 5, baseprimale3$artisticité)


#Application d'un score si on demande l'avis des pairs aux travaux effectués en entreprise
## Toujours +10, Souvent +8, Assez peu +1, Jamais +0

baseprimale3$G01Q51.SQ004[is.na(baseprimale3$G01Q51.SQ004)] = "Jamais"
baseprimale3$artisticité = ifelse(
  baseprimale3$G01Q51.SQ004== "Jamais", 
  baseprimale3$artisticité + 0, baseprimale3$artisticité)
baseprimale3$artisticité = ifelse(
  baseprimale3$G01Q51.SQ004 == "Assez peu", 
  baseprimale3$artisticité + 1, baseprimale3$artisticité)
baseprimale3$artisticité = ifelse(
  baseprimale3$G01Q51.SQ004 == "Souvent", 
  baseprimale3$artisticité + 8, baseprimale3$artisticité)
baseprimale3$artisticité = ifelse(
  baseprimale3$G01Q51.SQ004 == "Toujours", 
  baseprimale3$artisticité + 10, baseprimale3$artisticité)


# Application d'un score si on demande l'avis des pairs sur ses travaux personnels 
## Toujours +5, Souvent +4, Assez peu +2, Jamais +0

baseprimale3$G04Q120.SQ001[is.na(baseprimale3$G04Q120.SQ001)] = "Jamais"
baseprimale3$artisticité = ifelse(
  baseprimale3$G04Q120.SQ001== "Jamais", 
  baseprimale3$artisticité + 0, baseprimale3$artisticité)
baseprimale3$artisticité = ifelse(
  baseprimale3$G04Q120.SQ001 == "Assez peu", 
  baseprimale3$artisticité + 2, baseprimale3$artisticité)
baseprimale3$artisticité = ifelse(
  baseprimale3$G04Q120.SQ001 == "Souvent", 
  baseprimale3$artisticité + 4, baseprimale3$artisticité)
baseprimale3$artisticité = ifelse(
  baseprimale3$G04Q120.SQ001 == "Toujours", 
  baseprimale3$artisticité + 5, baseprimale3$artisticité)

#Application d'un score en fonction du sentiment d'être ? au travail
## Indispensable +5, Interchangeable +3, aucun +0
baseprimale3$indis_inter[is.na(baseprimale3$indis_inter)] = "2"
baseprimale3$artisticité = ifelse(
  baseprimale3$indis_inter== "0", 
  baseprimale3$artisticité + 3, baseprimale3$artisticité)
baseprimale3$artisticité = ifelse(
  baseprimale3$indis_inter == "1", 
  baseprimale3$artisticité + 5, baseprimale3$artisticité)
baseprimale3$artisticité = ifelse(
  baseprimale3$indis_inter == "2", 
  baseprimale3$artisticité + 0, baseprimale3$artisticité)

#Application d'un score en fonction d'une opinion si le jeu-vidéo est un art
#Cette variable ne nous est pas apparu comme pertinente après la passation du questionnaire
#Et durant l'élaboration de la variable scorée 
baseprimale3$jeu_art[is.na(baseprimale3$jeu_art)] = "Oui"
baseprimale3$artisticité = ifelse(
  baseprimale3$jeu_art == "Oui", 
  baseprimale3$artisticité + 0, baseprimale3$artisticité)
baseprimale3$artisticité = ifelse(
  baseprimale3$jeu_art  == "Non", 
  baseprimale3$artisticité + 0, baseprimale3$artisticité)
baseprimale3$artisticité = ifelse(
  baseprimale3$jeu_art  == "Ne se prononce pas", 
  baseprimale3$artisticité + 0, baseprimale3$artisticité)

# Création de la table Rapport_art_crunch isolant uniquement les acteurs du domaine artistique
Rapport_art_crunch<-filter(baseprimale3, domaine_tech_art=="Domaine artistique")

# Sélection au sein de cette table des acteurs ayant pratiqué le crunch 
Rapport_art_crunch<-filter(Rapport_art_crunch, G03Q55..Avez.vous.déjà.crunché...=="Oui")

##
## Fin de création de la variable d'artisticité --------------------------------------------------



## --------------------------------------------------------------------------
### Tableau 2.1.1 - Le Crunch en fonction du degré d'artisticité ----

# Création d'une nouvelle table (tableau) prenant uniquement en compte certaines variables 
tableau<-select(Rapport_art_crunch, artisticité, temps_crunch, crunchyn, temps_crunch, nbr_crunch,statut, temps_nocrunch )

# Découpage du score d'artisticité en trois tranche (0,9) (10,19) (20,28)
tableau$artisticité<-cut(tableau$artisticité, c(0,10,19,28), include.lowest= T, right=F)

# Suppressions des lignes disposant d'un degré d'artisticité NA
tableau<-tableau[!(rowSums(is.na(tableau[1]))),]

# Nous le mettons sous format numérique pour un recodage horaire
tableau$nbr_crunch<- as.numeric(tableau$nbr_crunch)

# Réutilisation du recodage alternatif du temps de travail en période de crunch 

tableau$temps_crunch <- tableau$temps_crunch %>%
  fct_recode(
    NULL = "",
    "2000-01-02 23:00:00" = "45-49h",
    "2000-01-02 16:00:00" = "40h",
    "2000-01-03 17:30:00" = "60-69h",
    "2000-01-03 07:30:00" = "50-59h",
    "2000-01-03 23:00:00" = "70h ou plus",
  )
tableau$temps_crunch <- as_datetime(tableau$temps_crunch)
tableau$temps_crunch<-abs(difftime(time1 = "2000-01-01 00:30:00", tableau$temps_crunch, units="hours"))
tableau$temps_crunch<-as.numeric_version(tableau$temps_crunch)
tableau$nbr_crunch<-as.numeric(tableau$nbr_crunch)

# Application de ce même recodage alternatif au temps de travail en période habituelle
tableau$temps_nocrunch <- tableau$temps_nocrunch %>%
  fct_recode(
    NULL = "",
    "2000-01-02 13:00:00" = "Entre 35H et 40H",
    "2000-01-02 18:00:00" = "Entre 40H et 44H",
    "2000-01-02 23:30:00" = "Entre 45H et 50H",
    "2000-01-02 11:00:00" = "Moins de 35H",
  )

tableau$temps_nocrunch <- as_datetime(tableau$temps_nocrunch)
tableau$temps_nocrunch<-abs(difftime(time1 = "2000-01-01 00:30:00", tableau$temps_nocrunch, units="hours"))
tableau$temps_nocrunch<-as.numeric_version(tableau$temps_nocrunch)
tableau$nbr_crunch<-as.numeric(tableau$nbr_crunch)

# Création d'une table regroupant les acteurs en fonction de leur degré d'artisticité
# Et effectuant la moyenne des heures de travail hebdomadaire en temps de crunch et en période habituelle
forme_finale<-tableau %>%
  group_by(artisticité) %>%
  summarise( nombre=mean(nbr_crunch, na.rm=T), temps=median(temps_crunch, na.rm=T), temps2=median(temps_nocrunch, na.rm=T))

# Mise en forme à l'aide de Kable 

champ9<-"Lecture : en moyenne un acteur au degrés d'artisticité [10,19) fera 1 crunch par an, d'une durée moyenne de 56 heures "
#nom des colonnes
names(forme_finale)[1:4] <- c("Degré d'artistictié", "Crunch annuel moyen", "Durée moyenne hebdomadaire d'un crunch", "Durée moyenne hebdomadaire habituelle")

forme_finale %>%
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrr", digits = c(0,0,1,1),
        caption = "Le crunch en fonction du degré d'artisticité",
        label = "tableau1", position = "ht!") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%    
  footnote(general = c(champ9),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "5cm") %>% 
  column_spec(4, width = "3.5cm") 

## Fin de la création du tableau 2.1.1
## --------------------------------------------------
