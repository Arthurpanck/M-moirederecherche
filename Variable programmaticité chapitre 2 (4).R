# Création des tableaux et variables en rapport avec la culture programmatique --------------------------------------------------


# Création d'une autre base 
baseprimale3<-baseprimale 

##Création de la variable de culture programmatique--------------------------------------------------
##

#Renommer les colonnes en rapport avec la culture programmatique pour prendre moins de place
baseprimale3$télé_travail<-baseprimale3$G04Q53.SQ003...Exercez.vous.votre.activité.en.télé.travail......Fréquence.#ok
baseprimale3$prog_libr<-baseprimale3$G04Q78.SQ001...Programmez.vous.sur.votre.temps.libre......#ok
baseprimale3$dev_libr<-baseprimale3$G04Q80.SQ001...Participez.vous.....Au.développement.de.logiciel.opensource.#ok
baseprimale3$game_jam<- baseprimale3$G04Q80.SQ002...Participez.vous.....A.des.Games.Jam.. #ok
baseprimale3$eventdev<-baseprimale3$G04Q80.SQ003...Participez.vous.....A.d.autres.événements.en.rapport.de.près.ou.de.loin.avec.le.développement. #ok
baseprimale3$relecture<-baseprimale3$G04Q76.SQ001...En.période.habituelle..à.quel.rythme.relisez.vous.le.code.de.vos.pairs.et.faites.vous.relire.votre.code.....Fréquence..

# Création de la variable de culture programmatique avec un score de 0 initial 
baseprimale3$programmatique<-0

#Ajout d'un score en fonction de la méthode de travail employée pour programmer 
#Cette variable ne nous est pas apparu comme pertinente après la passation du questionnaire
#Et durant l'élaboration de la variable scorée 

baseprimale3$work_dev[is.na(baseprimale3$work_dev)] = "One programming"
baseprimale3$programmatique = ifelse( 
  baseprimale3$work_dev == "One programming", 
  baseprimale3$programmatique + 0, baseprimale3$programmatique)
baseprimale3$programmatique = ifelse( 
  baseprimale3$work_dev == "Pair programming", 
  baseprimale3$programmatique + 0, baseprimale3$programmatique)

#Ajout d'un score en fonction du fait de programmer sur son temps libre 
## Toujours +10, Souvent +8, Assez peu +2, Jamais +0

baseprimale3$prog_libr[is.na(baseprimale3$prog_libr)] = "Jamais"
baseprimale3$programmatique = ifelse(
  baseprimale3$prog_libr == "Jamais", 
  baseprimale3$programmatique + 0, baseprimale3$programmatique)
baseprimale3$programmatique = ifelse(
  baseprimale3$prog_libr == "Assez peu", 
  baseprimale3$programmatique + 2, baseprimale3$programmatique)
baseprimale3$programmatique = ifelse(
  baseprimale3$prog_libr == "Souvent", 
  baseprimale3$programmatique + 8, baseprimale3$programmatique)
baseprimale3$programmatique = ifelse(
  baseprimale3$prog_libr == "Toujours", 
  baseprimale3$programmatique + 10, baseprimale3$programmatique)

#Ajout d'un score en fonction de la fréquence de télé-travail
## Tous les jours de la semaine+2, Entre deux et trois jours+3, un jour par semaine+1

baseprimale3$télé_travail[is.na(baseprimale3$télé_travail)] = "Un jour par semaine"
baseprimale3$programmatique = ifelse(
  baseprimale3$télé_travail == "Un jour par semaine", 
  baseprimale3$programmatique + 1, baseprimale3$programmatique)
baseprimale3$programmatique = ifelse(
  baseprimale3$télé_travail == "Entre deux et trois jours par semaine", 
  baseprimale3$programmatique + 3, baseprimale3$programmatique)
baseprimale3$programmatique = ifelse(
  baseprimale3$télé_travail == "Tous les jours de la semaine", 
  baseprimale3$programmatique + 2, baseprimale3$programmatique)

#Ajout d'un score en fonction de la participation au développement logiciel libre/open source
## Souvent +5, Rarement +2, Jamais +0 
baseprimale3$dev_libr[is.na(baseprimale3$dev_libr)] = "jamais"
baseprimale3$programmatique = ifelse(
  baseprimale3$dev_libr == "Jamais", 
  baseprimale3$programmatique + 0, baseprimale3$programmatique)
baseprimale3$programmatique = ifelse(
  baseprimale3$dev_libr == "Rarement", 
  baseprimale3$programmatique + 2, baseprimale3$programmatique)
baseprimale3$programmatique = ifelse(
  baseprimale3$dev_libr== "Souvent", 
  baseprimale3$programmatique + 5, baseprimale3$programmatique)

#Ajout d'un score en fonction de la participation à des Game-Jam
## Souvent +10, Rarement +8, Jamais +0 
baseprimale3$game_jam[is.na(baseprimale3$game_jam)] = "jamais"
baseprimale3$programmatique = ifelse(
  baseprimale3$game_jam == "Jamais", 
  baseprimale3$programmatique + 0, baseprimale3$programmatique)
baseprimale3$programmatique = ifelse(
  baseprimale3$game_jam == "Rarement", 
  baseprimale3$programmatique + 8, baseprimale3$programmatique)
baseprimale3$programmatique = ifelse(
  baseprimale3$game_jam== "Souvent", 
  baseprimale3$programmatique + 10, baseprimale3$programmatique)

#Ajout d'un score à la variable participation à des événements de programmeurs
## Souvent+5, Rarement+2, Jamais+0

baseprimale3$eventdev[is.na(baseprimale3$eventdev)] = "jamais"
baseprimale3$programmatique = ifelse(
  baseprimale3$eventdev== "Jamais", 
  baseprimale3$programmatique + 0, baseprimale3$programmatique)
baseprimale3$programmatique = ifelse(
  baseprimale3$eventdev == "Rarement", 
  baseprimale3$programmatique + 2, baseprimale3$programmatique)
baseprimale3$programmatique = ifelse(
  baseprimale3$eventdev== "Souvent", 
  baseprimale3$programmatique + 5, baseprimale3$programmatique)

#Ajout d'un score en fonction de la fréquence de relecture de code 
## Toujours+10, Souvent+8, Assez peu+2, Jamais+0 

baseprimale3$relecture [is.na(baseprimale3$relecture )] = "jamais"
baseprimale3$programmatique = ifelse(
  baseprimale3$relecture == "Jamais", 
  baseprimale3$programmatique + 0, baseprimale3$programmatique)
baseprimale3$programmatique = ifelse(
  baseprimale3$relecture == "Assez peu", 
  baseprimale3$programmatique + 2, baseprimale3$programmatique)
baseprimale3$programmatique = ifelse(
  baseprimale3$relecture == "Souvent", 
  baseprimale3$programmatique + 8, baseprimale3$programmatique)
baseprimale3$programmatique = ifelse(
  baseprimale3$relecture == "Toujours", 
  baseprimale3$programmatique + 10, baseprimale3$programmatique)


# Création de la table Rapport_programmatique isolant uniquement les acteurs du domaine programmatique
Rapport_programmatique_crunch<-filter(baseprimale3, domaine_tech_art=="Domaine programmatique")

# Sélection au sein de cette table des acteurs ayant pratiqué le crunch 
Rapport_programmatique_crunch<-filter(Rapport_programmatique_crunch, G03Q55..Avez.vous.déjà.crunché...=="Oui")

##
## Fin de création de la variable culture programmatique --------------------------------------------------


## --------------------------------------------------------------------------
### Tableau 2.2.1 - Le Crunch en fonction du degré de culture programmatique ----

# Création d'une nouvelle table (programme) prenant uniquement en compte certaines variables 
programme<-select(Rapport_programmatique_crunch, programmatique, temps_crunch, crunchyn, temps_crunch, nbr_crunch, statut, temps_nocrunch )

# Découpage du score de culture programmatique en trois tranche (0,9) (10,14) (15,25)
programme$programmatique<-cut(programme$programmatique, c(0,9,14,25), include.lowest= T, right=F)

# Suppressions des lignes disposant d'un degré de culture programmatique NA
programme<-programme[!(rowSums(is.na(programme[1]))),]

# Nous le mettons sous format numérique
programme$nbr_crunch<- as.numeric(programme$nbr_crunch)

# Réutilisation du recodage alternatif du temps de travail en période de crunch 
programme$temps_crunch <- programme$temps_crunch %>%
  fct_recode(
    NULL = "",
    "2000-01-02 23:00:00" = "45-49h",
    "2000-01-02 16:00:00" = "40h",
    "2000-01-03 17:30:00" = "60-69h",
    "2000-01-03 07:30:00" = "50-59h",
    "2000-01-03 23:00:00" = "70h ou plus",
  )
programme$temps_crunch <- as_datetime(programme$temps_crunch)
programme$temps_crunch<-abs(difftime(time1 = "2000-01-01 00:30:00", programme$temps_crunch, units="hours"))
programme$temps_crunch<-as.numeric_version(programme$temps_crunch)
programme$nbr_crunch<-as.numeric(programme$nbr_crunch)

# Application de ce même recodage alternatif au temps de travail en période habituelle
programme$temps_nocrunch <- programme$temps_nocrunch %>%
  fct_recode(
    NULL = "",
    "2000-01-02 13:00:00" = "Entre 35H et 40H",
    "2000-01-02 18:00:00" = "Entre 40H et 44H",
    "2000-01-02 23:30:00" = "Entre 45H et 50H",
    "2000-01-02 11:00:00" = "Moins de 35H",
  )

programme$temps_nocrunch <- as_datetime(programme$temps_nocrunch)
programme$temps_nocrunch<-abs(difftime(time1 = "2000-01-01 00:30:00", programme$temps_nocrunch, units="hours"))
programme$temps_nocrunch<-as.numeric_version(programme$temps_nocrunch)
programme$nbr_crunch<-as.numeric(programme$nbr_crunch)


# Création d'une table regroupant les acteurs en fonction de leur degré de culture programmatique
# Et effectuant la moyenne des heures de travail hebdomadaire en temps de crunch et en période habituelle
forme_finale2<-programme %>%
  group_by(programmatique) %>%
  summarise( nombre=mean(nbr_crunch, na.rm=T), temps=median(temps_crunch, na.rm=T), temps2=median(temps_nocrunch, na.rm=T))

# Mise en forme à l'aide de Kable 

champ10<-"Lecture : en moyenne un acteur au degré de culture programmatique [14,25) fera 1 crunch par an, d'une durée moyenne hebdomadaire de 61 heures contre 40 heures en période habituelle"
#nom des colonnes
names(forme_finale2)[1:4] <- c("Degré de culture programmatique", "Crunch annuel moyen", "Durée moyenne hebdomadaire d'un crunch", "Durée moyenne hebdomadaire habituelle")

forme_finale2 %>%
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrr", digits = c(0,0,1,1),
        caption = "Le crunch en fonction du degré de culture programmatique",
        label = "tableau1", position = "ht!") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%    
  footnote(general = c(champ10),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "5cm") %>% 
  column_spec(4, width = "3.5cm") 


## Fin de la création du tableau 2.2.1
## --------------------------------------------------