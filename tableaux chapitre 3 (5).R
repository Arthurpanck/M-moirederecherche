##Création de la variable de normalité de la pratique du crunch à l'échelle de l'organisation--------------------------------------------------
##
baseprimale4<-baseprimale

#Renommer les variables de la normalité du crunch pour qu'elles prennent moins de place  

baseprimale4$pratiq_cons<- baseprimale4$G01Q12..La.pratique.du.crunch.en.entreprise.est.elle.une.pratique.normale.selon.vous..
baseprimale4$pratiq_remi <- baseprimale4$G01Q13..Dans.l.entreprise.personne.ou.peu.de.personnes.remettent.en.cause.cette.pratique..
baseprimale4$pos_diff<- baseprimale4$G02Q10..Durant.un.crunch..estimez.vous.qu.il.soit.difficile.d.adopter.une.position.en.désaccord.avec.le.groupe..
baseprimale4$qual_sit<- baseprimale4$G04Q100..La.situation.de.Crunch.est.elle.qualifiée.explicitement.comme.du.Crunch...

#Création de la variable normalité du crunch avec un score de 0 
baseprimale4$normalité<-0

### Ajout d'un score en fonction de la considération de la pratique du crunch
# elle est nécessaire+1, elle est habituelle+1, Non+0

baseprimale4$pratiq_cons[is.na(baseprimale4$pratiq_cons)] = "Non"
baseprimale4$normalité  = ifelse(
  baseprimale4$pratiq_cons== "Non", 
  baseprimale4$normalité  + 0, baseprimale4$normalité )
baseprimale4$normalité  = ifelse(
  baseprimale4$pratiq_cons== "Elle est habituelle", 
  baseprimale4$normalité  + 1, baseprimale4$normalité )
baseprimale4$normalité  = ifelse(
  baseprimale4$pratiq_cons== "Elle est nécessaire", 
  baseprimale4$normalité  + 1, baseprimale4$normalité )

### Ajout d'un socre en fonction du degré de remise en cause de la pratique
# La pratique pas remise en cause +10, la pratique remise en cause +5, ils ne la connaissent pas +0

baseprimale4$pratiq_remi[is.na(baseprimale4$pratiq_remi)] = "Ils ne la connaissent pas"
baseprimale4normalité  = ifelse(
  baseprimale4$pratiq_remi== "La pratique est remise en cause", 
  baseprimale4$normalité  + 5, baseprimale4$normalité )
baseprimale4$normalité  = ifelse(
  baseprimale4$pratiq_remi== "La pratique n'est pas remise en cause", 
  baseprimale4$normalité  + 10, baseprimale4$normalité )
baseprimale4$normalité  = ifelse(
  baseprimale4$pratiq_remi== "Ils ne la connaissent pas", 
  baseprimale4$normalité  + 0, baseprimale4$normalité )

### Ajout d'un score en fonction de la possibilité d'être en désaccord avec le groupe vis à vis du crunch 
# Oui+10, Non+5

baseprimale4$pos_diff[is.na( baseprimale4$pos_diff)] = "Non"
baseprimale4$normalité  = ifelse(
  baseprimale4$pos_diff== "Non", 
  baseprimale4$normalité  + 5, baseprimale4$normalité )
baseprimale4$normalité  = ifelse(
  baseprimale4$pos_diff== "Oui", 
  baseprimale4$normalité  + 10, baseprimale4$normalité )

### Ajout d'un score en fonction de la variable qualification de la pratique du crunch dans l'entreprise 
# Oui par les deux +5, oui par la direction+3, oui par les équipes+4, non+0

baseprimale4$qual_sit[is.na(baseprimale4$qual_sit)] = "Non"
baseprimale4$normalité  = ifelse(
  baseprimale4$qual_sit== "Non", 
  baseprimale4$normalité  + 0, baseprimale4$normalité )
baseprimale4$normalité  = ifelse(
  baseprimale4$qual_sit== "Oui par les équipes", 
  baseprimale4$normalité  + 4, baseprimale4$normalité )
baseprimale4$normalité  = ifelse(
  baseprimale4$qual_sit== "Oui par la direction", 
  baseprimale4$normalité  + 3, baseprimale4$normalité )
baseprimale4$normalité  = ifelse(
  baseprimale4$qual_sit== "Oui par les deux", 
  baseprimale4$normalité  + 5, baseprimale4$normalité )

#
## Fin de création de la variable de normalité du crunch à l'échelle de l'organisation--------------------------------------------------


## --------------------------------------------------------------------------
### Tableau 3.2.4 - Le crunch en fonction du degré de normalité du crunch ----

#Création d'une autre table nommée tablnormale 
tablnormale<-select(baseprimale4, normalité, temps_crunch, crunchyn, temps_crunch, nbr_crunch, statut, temps_nocrunch )

#découpage de la normalité en (0,9) (9,15) (15,24)
tablnormale$normalité<-cut(tablnormale$normalité, c(0,9,15,24), include.lowest= T, right=F)

#Suppression des lignes ne disposant pas d'un degré de normalité 
tablnormale<-tablnormale[!(rowSums(is.na(tablnormale[1]))),]

#Recodage temporel avec la seconde méthode, de la même manière que précédemment exécutée 
tablnormale$nbr_crunch<- as.numeric(tablnormale$nbr_crunch)
tablnormale$nbr_crunch[is.na(tablnormale$nbr_crunch)] = "0"
tablnormale$temps_crunch[is.na(temps_crunch)] = "0"

tablnormale$temps_crunch <- tablnormale$temps_crunch %>%
  fct_recode(
    NULL = "",
    "2000-01-02 23:00:00" = "45-49h",
    "2000-01-02 16:00:00" = "40h",
    "2000-01-03 17:30:00" = "60-69h",
    "2000-01-03 07:30:00" = "50-59h",
    "2000-01-03 23:00:00" = "70h ou plus",
  )
tablnormale$temps_crunch <- as_datetime(tablnormale$temps_crunch)
tablnormale$temps_crunch<-abs(difftime(time1 = "2000-01-01 00:30:00", tablnormale$temps_crunch, units="hours"))
tablnormale$temps_crunch<-as.numeric_version(tablnormale$temps_crunch)
tablnormale$nbr_crunch<-as.numeric(tablnormale$nbr_crunch)

tablnormale$temps_nocrunch <- tablnormale$temps_nocrunch %>%
  fct_recode(
    NULL = "",
    "2000-01-02 13:00:00" = "Entre 35H et 40H",
    "2000-01-02 18:00:00" = "Entre 40H et 44H",
    "2000-01-02 23:30:00" = "Entre 45H et 50H",
    "2000-01-02 11:00:00" = "Moins de 35H",
  )

tablnormale$temps_nocrunch <- as_datetime(tablnormale$temps_nocrunch)
tablnormale$temps_nocrunch<-abs(difftime(time1 = "2000-01-01 00:30:00", tablnormale$temps_nocrunch, units="hours"))
tablnormale$temps_nocrunch<-as.numeric_version(tablnormale$temps_nocrunch)
tablnormale$nbr_crunch<-as.numeric(tablnormale$nbr_crunch)

#Création d'une table avec les effectifs, le temps moyen de crunch, le temps moyen de travail en période habituelle
#Le tout regroupé en fonction des différentes tranches de degré de normalité 
tablnormale<-tablnormale %>%
  group_by(normalité) %>%
  summarise( nombre=mean(nbr_crunch, na.rm=T), temps=median(temps_crunch, na.rm=T), temps2=median(temps_nocrunch, na.rm=T))

#Changement du nom des colonnes
names(tablnormale)[1:4] <- c("Degré de normalité du crunch", "Crunch annuel moyen", "Durée moyenne hebdomadaire d'un crunch", "Durée moyenne hebdomadaire habituelle")

#Mise en forme du tableau 

champ14<-"Lecture : en moyenne un acteur perçevant le crunch comme normal à hauteur de [9,15) fera en moyenne 2 crunch par an, d'une durée moyenne hebdomadaire de 56 heures contre 37,5 heures en période habituelle"

tablnormale%>%
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrr", digits = c(0,0,1,1),
        caption = "Le crunch en fonction du degré de normalité du crunch",
        label = "tableau1", position = "ht!") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%    
  footnote(general = c(champ14),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "5cm") %>% 
  column_spec(4, width = "3.5cm") 

# Fin du tableau 3.2.4
## --------------------------------------------------



## Création de la variable institutionnelle du crunch--------------------------------------------------
##

# Renommer les colonnes en rapport avec la variable institutionnelle pour prendre moins de place

baseprimale4$svr_crunch<-baseprimale4$G02Q16..Saviez.vous.que.vous.alliez.cruncher.en.entrant.dans.cette.entreprise........
baseprimale4$deci_crunch<-baseprimale4$G02Q19..La.décision.de.cruncher.venait.elle.des.équipes..
baseprimale4$deci_crunch2<-baseprimale4$G02Q18..La.décision.de.cruncher.venait.elle.de.la.direction..
baseprimale4$augmentation_crunch<-baseprimale4$G01Q21..Lors.de.votre.dernier.Crunch..l.augmentation.du.temps.de.travail.était.elle...........

# Suppression des lignes avec des nons réponses à "de quelles manières étaient ou non payées vos heures supplémentaires" 
# De manière générale les répondants à cette question ont répondu aux autres questions constituant la variable scorée
# De manière générale les non répondants à cette question n'ont pas répondu aux autres questions constituant la variable scorée 
baseprimale4<-baseprimale4[!(rowSums(is.na(baseprimale4[134]))),] 

# Création variable institutionnelle du crunch avec un score de 0 initial 
baseprimale4$organisationnelle<-0


### Ajout d'un score en fonction du moyen de paiement des heures supplémentaires
# Prime+10, heures supplémentaires+8, payées partiellement+5, non payées+0

baseprimale4$moyen_pay[is.na(baseprimale4$moyen_pay)] = "Non payées" #Sujet à débat
baseprimale4$organisationnelle  = ifelse(
  baseprimale4$moyen_pay== "Heures supplémentaires", 
  baseprimale4$organisationnelle  + 8, baseprimale4$organisationnelle )
baseprimale4$organisationnelle  = ifelse(
  baseprimale4$moyen_pay== "Payées partiellement", 
  baseprimale4$organisationnelle  + 5, baseprimale4$organisationnelle )
baseprimale4$organisationnelle  = ifelse(
  baseprimale4$moyen_pay== "Non payées", 
  baseprimale4$organisationnelle  + 0, baseprimale4$organisationnelle )
baseprimale4$organisationnelle  = ifelse(
  baseprimale4$moyen_pay== "Prime", 
  baseprimale4$organisationnelle  + 10, baseprimale4$organisationnelle )

### Ajout d'un score en fonction du fait que le crunch soit la décision des équipes
# Oui+5, Non+0

baseprimale4$deci_crunch[is.na(baseprimale4$deci_crunch)] = "Non"
baseprimale4$organisationnelle  = ifelse(
  baseprimale4$deci_crunch== "Oui", 
  baseprimale4$organisationnelle  + 5, baseprimale4$organisationnelle )
baseprimale4$organisationnelle  = ifelse(
  baseprimale4$deci_crunch== "Non", 
  baseprimale4$organisationnelle  + 0, baseprimale4$organisationnelle )

### Ajout d'un score en fonction du fait que le crunch soit la décision de la direction 
# Oui+10, Non+0

baseprimale4$deci_crunch2[is.na(baseprimale4$deci_crunch2)] = "Non"
baseprimale4$organisationnelle  = ifelse(
  baseprimale4$deci_crunch2== "Oui", 
  baseprimale4$organisationnelle  + 10, baseprimale4$organisationnelle )
baseprimale4$organisationnelle  = ifelse(
  baseprimale4$deci_crunch2== "Non", 
  baseprimale4$organisationnelle  + 0, baseprimale4$organisationnelle )

### Ajout d'un score en fonction de la répartition des heures supplémentaires  
# Lisée+10, désordonnée+1, Autre+0 

baseprimale4$augmentation_crunch[is.na(baseprimale4$augmentation_crunch)] = "Autre"
baseprimale4$organisationnelle  = ifelse(
  baseprimale4$augmentation_crunch== "désordonnée ( tous les jours vous terminez à un horaire différent)", 
  baseprimale4$organisationnelle  + 1, baseprimale4$organisationnelle )
baseprimale4$organisationnelle  = ifelse(
  baseprimale4$augmentation_crunch== "lissée ( par exemple une augmentation de 2h de travail par jour)", 
  baseprimale4$organisationnelle  + 10, baseprimale4$organisationnelle )
baseprimale4$organisationnelle  = ifelse(
  baseprimale4$augmentation_crunch== "Autre", 
  baseprimale4$organisationnelle  + 0, baseprimale4$organisationnelle )


### Ajout d'un score en fonction du fait de savoir qu'ils allaient crunch
# Oui +2, Non +0, Autre+0, Je ne savais pas ce qu'était le crunch +0

baseprimale4$svr_crunch[is.na(baseprimale4$svr_crunch)] = "Autre"
baseprimale4$organisationnelle  = ifelse(
  baseprimale4$svr_crunch== "Non", 
  baseprimale4$organisationnelle  + 0, baseprimale4$organisationnelle )
baseprimale4$organisationnelle  = ifelse(
  baseprimale4$svr_crunch== "Oui", 
  baseprimale4$organisationnelle  + 2, baseprimale4$organisationnelle )
baseprimale4$organisationnelle  = ifelse(
  baseprimale4$svr_crunch== "Je ne savais pas ce qu'était le Crunch", 
  baseprimale4$organisationnelle  + 0, baseprimale4$organisationnelle )
baseprimale4$organisationnelle  = ifelse(
  baseprimale4$svr_crunch== "Autre", 
  baseprimale4$organisationnelle  + 0, baseprimale4$organisationnelle )

#
## Fin de création de la variable institutionnelle --------------------------------------------------

## --------------------------------------------------------------------------
### Tableau 3.2.1 - Le crunch en fonction de son degré d'institutionnalisation ----


# Création d'une table regroupant la variable institutionnelle, les horaires en temps de crunch et de non crunch 
institution<-select(baseprimale4, organisationnelle, temps_crunch, crunchyn, temps_crunch, nbr_crunch, statut, temps_nocrunch )

# On retranche l'échantillon uniquement à la population travaillant en organisation (CDD,CDI,stagiaire)
institution<-filter(institution, statut=="Salarié CDD "| statut=="Salarié CDI" | statut=="Stagiaire" )

# On découpe en deux tranches [0,9) et [9,24]
institution$organisationnelle<-cut(institution$organisationnelle, c(0,9,24), include.lowest= T, right=F)

#On supprime les lignes dont la colonne ne comprend aucun degré d'institutionnalisation
institution<-institution[!(rowSums(is.na(institution[1]))),]

# Remplacement des NA par des 0 dans le nombre de crunch annuel et le temps de crunch hebdomadaire
institution$nbr_crunch<- as.numeric(institution$nbr_crunch)
institution$nbr_crunch[is.na(institution$nbr_crunch)] = "0"
institution$temps_crunch[is.na(temps_crunch)] = "0"

#Recodage du temps avec la seconde méthode 

institution$temps_crunch <- institution$temps_crunch %>%
  fct_recode(
    NULL = "",
    "2000-01-02 23:00:00" = "45-49h",
    "2000-01-02 16:00:00" = "40h",
    "2000-01-03 17:30:00" = "60-69h",
    "2000-01-03 07:30:00" = "50-59h",
    "2000-01-03 23:00:00" = "70h ou plus",
  )
institution$temps_crunch <- as_datetime(institution$temps_crunch)
institution$temps_crunch<-abs(difftime(time1 = "2000-01-01 00:30:00", institution$temps_crunch, units="hours"))
institution$temps_crunch<-as.numeric_version(institution$temps_crunch)
institution$nbr_crunch<-as.numeric(institution$nbr_crunch)

institution$temps_nocrunch <- institution$temps_nocrunch %>%
  fct_recode(
    NULL = "",
    "2000-01-02 13:00:00" = "Entre 35H et 40H",
    "2000-01-02 18:00:00" = "Entre 40H et 44H",
    "2000-01-02 23:30:00" = "Entre 45H et 50H",
    "2000-01-02 11:00:00" = "Moins de 35H",
  )

institution$temps_nocrunch <- as_datetime(institution$temps_nocrunch)
institution$temps_nocrunch<-abs(difftime(time1 = "2000-01-01 00:30:00", institution$temps_nocrunch, units="hours"))
institution$temps_nocrunch<-as.numeric_version(institution$temps_nocrunch)
institution$nbr_crunch<-as.numeric(institution$nbr_crunch)

# Création d'une table regroupant les acteurs en fonction du degré d'institutionnalisation de la pratique
# Et effectuant la moyenne des heures de travail hebdomadaire en temps de crunch et en période habituelle
institution<-institution %>%
  group_by(organisationnelle) %>%
  summarise( nombre=mean(nbr_crunch, na.rm=T), temps=median(temps_crunch, na.rm=T), temps2=median(temps_nocrunch, na.rm=T))

# Mise en forme à l'aide de Kable 

champ11<-"Lecture : en moyenne un acteur au sein d'une organisation où le crunch est institutionnalisé à hauteur de [0,9) fera en moyenne 1 crunch par an, d'une durée moyenne hebdomadaire de 56 heures contre 37,5 heures en période habituelle, ce n'est donc pas un critère pertinent d'analyse"
names(institution)[1:4] <- c("Degré d'institutionnalisation du crunch", "Crunch annuel moyen", "Durée moyenne hebdomadaire d'un crunch", "Durée moyenne hebdomadaire habituelle")

institution %>%
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrr", digits = c(0,0,1,1),
        caption = "Le crunch en fonction de son degré d'institutionnalisation",
        label = "tableau1", position = "ht!") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%    
  footnote(general = c(champ11),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "5cm") %>% 
  column_spec(4, width = "3.5cm") 


# Fin Tableau 3.2.1 ---------------------------------------
## --------------------------------------------------

## --------------------------------------------------------------------------
### Tableau 3.2.2 - Le crunch en fonction de son degré d'institutionnalisation ----

# Création d'une table regroupant la variable institutionnelle, les horaires en temps de crunch et de non crunch 
institution2<-select(baseprimale4, organisationnelle, temps_crunch, crunchyn, temps_crunch, nbr_crunch, statut, temps_nocrunch )

# On retranche l'échantillon uniquement à la population travaillant en organisation (CDD,CDI,stagiaire)
institution2<-filter(institution2, statut=="Salarié CDD "| statut=="Salarié CDI" | statut=="Stagiaire" )

# On découpe en deux tranches [0,9) et [9,24]
institution2$organisationnelle<-cut(institution2$organisationnelle, c(0,9,24), include.lowest= T, right=F)

#On supprime les lignes dont la colonne ne comprend aucun degré d'institutionnalisation
institution2<-institution2[!(rowSums(is.na(institution2[1]))),]

# Remplacement des NA par des 0 dans le nombre de crunch annuel et le temps de crunch hebdomadaire
institution2$nbr_crunch<- as.numeric(institution2$nbr_crunch)
institution2$nbr_crunch[is.na(institution2$nbr_crunch)] = "0"
institution2$temps_crunch[is.na(temps_crunch)] = "0"

#Recodage du temps avec la seconde méthode 

institution2$temps_crunch <- institution2$temps_crunch %>%
  fct_recode(
    NULL = "",
    "2000-01-02 23:00:00" = "45-49h",
    "2000-01-02 16:00:00" = "40h",
    "2000-01-03 17:30:00" = "60-69h",
    "2000-01-03 07:30:00" = "50-59h",
    "2000-01-03 23:00:00" = "70h ou plus",
  )
institution2$temps_crunch <- as_datetime(institution2$temps_crunch)
institution2$temps_crunch<-abs(difftime(time1 = "2000-01-01 00:30:00", institution2$temps_crunch, units="hours"))
institution2$temps_crunch<-as.numeric_version(institution2$temps_crunch)
institution2$nbr_crunch<-as.numeric(institution2$nbr_crunch)

institution2$temps_nocrunch <- institution2$temps_nocrunch %>%
  fct_recode(
    NULL = "",
    "2000-01-02 13:00:00" = "Entre 35H et 40H",
    "2000-01-02 18:00:00" = "Entre 40H et 44H",
    "2000-01-02 23:30:00" = "Entre 45H et 50H",
    "2000-01-02 11:00:00" = "Moins de 35H",
  )

institution2$temps_nocrunch <- as_datetime(institution2$temps_nocrunch)
institution2$temps_nocrunch<-abs(difftime(time1 = "2000-01-01 00:30:00", institution2$temps_nocrunch, units="hours"))
institution2$temps_nocrunch<-as.numeric_version(institution2$temps_nocrunch)
institution2$nbr_crunch<-as.numeric(institution2$nbr_crunch)

# Création d'une table regroupant les acteurs en fonction du degré d'institutionnalisation de la pratique
# Et effectuant la moyenne des heures de travail hebdomadaire en temps de crunch et en période habituelle
institution2<-institution2 %>%
  group_by(organisationnelle) %>%
  summarise( nombre=mean(nbr_crunch, na.rm=T), temps=sd(temps_crunch, na.rm=T), temps2=median(temps_nocrunch, na.rm=T))

# Mise en forme à l'aide de Kable 

champ12<-"Lecture : l'écart type de la durée moyenne hebdomadaire d'un crunch au sein d'une organisation où le crunch est institutionnalisé à hauteur de [0,9) est de 10,9"
names(institution2)[1:4] <- c("Degré d'institutionnalisation du crunch", "Crunch annuel moyen", "Ecart type de la durée moyenne hebdomadaire d'un crunch", "Durée moyenne hebdomadaire habituelle")

institution2 %>%
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrr", digits = c(0,0,1,1),
        caption = "Le crunch en fonction de son degré d'institutionnalisation",
        label = "tableau1", position = "ht!") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%    
  footnote(general = c(champ12),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "5cm") %>% 
  column_spec(4, width = "3.5cm") 


## Fin tableau 3.2.2 ---------------------------------------
## --------------------------------------------------


##Création de la variable de soutien affectif du crunch--------------------------------------------------
##

#Renommer les colonnes en rapport avec la variable soutien affectif pour prendre moins de place
baseprimale4$ami<-baseprimale4$G02Q08..Durant.un.crunch..certains.de.vos.collègues.sont.ils.devenus.vos.amis..
baseprimale4$nature_coll<-baseprimale4$G02Q07..Durant.un.crunch..avez.vous.eu.l.impression.de.découvrir.la.vraie.nature.de.vos.collègues..

#Création de la variable soutien affectif à un score de 0 
baseprimale4$émotionnelle<-0 


### Ajout d'un score en fonction de la création d'un lien d'amitié
# Oui+10, Non+0

baseprimale4$ami[is.na(baseprimale4$ami)] = "Non"
baseprimale4$émotionnelle  = ifelse(
  baseprimale4$ami== "Oui", 
  baseprimale4$émotionnelle  + 10, baseprimale4$émotionnelle )
baseprimale4$émotionnelle  = ifelse(
  baseprimale4$ami== "Non", 
  baseprimale4$émotionnelle  + 0, baseprimale4$émotionnelle )

### Ajout d'un score en fonction du fait d'avoir découvert la nature de ses collègues
# Oui+5, Non+0

baseprimale4$nature_coll[is.na(baseprimale4$nature_coll)] = "Non"
baseprimale4$émotionnelle  = ifelse(
  baseprimale4$nature_coll== "Oui", 
  baseprimale4$émotionnelle  + 5, baseprimale4$émotionnelle )
baseprimale4$émotionnelle  = ifelse(
  baseprimale4$nature_coll== "Non", 
  baseprimale4$émotionnelle  + 0, baseprimale4$émotionnelle )

### Ajoute d'un score en fonction de la formation de nouvelles relations au cours du crunch
#Supérieur hiéarchique+10, professionnel forums +10, Salarié profession +5, salarié même rang+5, salarié profession différente +5

baseprimale4$new_rel[is.na(baseprimale4$new_rel)] = "Non"
baseprimale4$émotionnelle  = ifelse(
  baseprimale4$new_rel== "Salarié de la profession", 
  baseprimale4$émotionnelle  + 5, baseprimale4$émotionnelle )
baseprimale4$émotionnelle  = ifelse(
  baseprimale4$new_rel== "Salarié de même rang", 
  baseprimale4$émotionnelle  + 5, baseprimale4$émotionnelle )
baseprimale4$émotionnelle  = ifelse(
  baseprimale4$new_rel== "Salarié profession différente", 
  baseprimale4$émotionnelle  + 5, baseprimale4$émotionnelle )
baseprimale4$émotionnelle  = ifelse(
  baseprimale4$new_rel== "Supérieur hiéarchique", 
  baseprimale4$émotionnelle  + 10, baseprimale4$émotionnelle )
baseprimale4$émotionnelle  = ifelse(
  baseprimale4$new_rel== "Professionnels-forums", 
  baseprimale4$émotionnelle  + 10, baseprimale4$émotionnelle )
baseprimale4$émotionnelle  = ifelse(
  baseprimale4$new_rel== "Non", 
  baseprimale4$émotionnelle  + 0, baseprimale4$émotionnelle )

###Ajout d'un score en fonction de fête réalisées durant un crunch 
#Restaurant+5, Chez collègue+3, Le soir au bureau+2, pause+2, Non+0

baseprimale4$fest_crunch[is.na(baseprimale4$fest_crunch)] = "Non"
baseprimale4$émotionnelle  = ifelse(
  baseprimale4$fest_crunch== "Non", 
  baseprimale4$émotionnelle  + 0, baseprimale4$émotionnelle )
baseprimale4$émotionnelle  = ifelse(
  baseprimale4$fest_crunch== "Restaurant", 
  baseprimale4$émotionnelle  + 5, baseprimale4$émotionnelle )
baseprimale4$émotionnelle  = ifelse(
  baseprimale4$fest_crunch== "Chez collègue", 
  baseprimale4$émotionnelle  + 3, baseprimale4$émotionnelle )
baseprimale4$émotionnelle  = ifelse(
  baseprimale4$fest_crunch== "Le soir au bureau", 
  baseprimale4$émotionnelle  + 2, baseprimale4$émotionnelle )
baseprimale4$émotionnelle  = ifelse(
  baseprimale4$fest_crunch== "Pause", 
  baseprimale4$émotionnelle  + 2, baseprimale4$émotionnelle )


##
## Fin de création de la variable de soutien affectif--------------------------------------------------


## --------------------------------------------------------------------------
### Tableau 3.2.3 - Relation entre la dimension émotionnelle et institutionnelle ----

# Création d'une nouvelle table (institemo) prenant uniquement en compte variable institutionnelle et émotionnelle, ainsi que les horaires de crunch et de non crunch
instit_emo<-select(baseprimale4, organisationnelle, émotionnelle, temps_crunch, crunchyn, temps_crunch, nbr_crunch, statut, temps_nocrunch )

#On retranche aux individus travaillant dans des organisations (CDD, CDI)
instit_emo<-filter(instit_emo, statut=="Salarié CDD "| statut=="Salarié CDI" | statut=="Stagiaire" )

#On découpe en le soutien affectif en [0,7] [7,22] et l'institutionnalisation en [0,9] [9,24]
instit_emo$organisationnelle<-cut(instit_emo$organisationnelle, c(0,9,24), include.lowest= T, right=F)
instit_emo$émotionnelle<-cut(instit_emo$émotionnelle, c(0,7,22), include.lowest= T, right=F)

#On supprime les colonnes qui n'ont pas de temps de crunch
instit_emo<-instit_emo[!(rowSums(is.na(instit_emo[3]))),]
instit_emo$nbr_crunch<- as.numeric(instit_emo$nbr_crunch)
instit_emo$nbr_crunch[is.na(instit_emo$nbr_crunch)] = "0"
instit_emo$temps_crunch[is.na(temps_crunch)] = "0"

#On effectue le recodage habituel en terme horaire 

instit_emo$temps_crunch <- instit_emo$temps_crunch %>%
  fct_recode(
    NULL = "",
    "2000-01-02 23:00:00" = "45-49h",
    "2000-01-02 16:00:00" = "40h",
    "2000-01-03 17:30:00" = "60-69h",
    "2000-01-03 07:30:00" = "50-59h",
    "2000-01-03 23:00:00" = "70h ou plus",
  )

instit_emo$temps_crunch <- as_datetime(instit_emo$temps_crunch)
instit_emo$temps_crunch<-abs(difftime(time1 = "2000-01-01 00:30:00", instit_emo$temps_crunch, units="hours"))
instit_emo$temps_crunch<-as.numeric_version(instit_emo$temps_crunch)
instit_emo$nbr_crunch<-as.numeric(instit_emo$nbr_crunch)

instit_emo$temps_nocrunch <- instit_emo$temps_nocrunch %>%
  fct_recode(
    NULL = "",
    "2000-01-02 13:00:00" = "Entre 35H et 40H",
    "2000-01-02 18:00:00" = "Entre 40H et 44H",
    "2000-01-02 23:30:00" = "Entre 45H et 50H",
    "2000-01-02 11:00:00" = "Moins de 35H",
  )

instit_emo$temps_nocrunch <- as_datetime(instit_emo$temps_nocrunch)
instit_emo$temps_nocrunch<-abs(difftime(time1 = "2000-01-01 00:30:00", instit_emo$temps_nocrunch, units="hours"))
instit_emo$temps_nocrunch<-as.numeric_version(instit_emo$temps_nocrunch)
instit_emo$nbr_crunch<-as.numeric(instit_emo$nbr_crunch)

#Mise en page à l'aide de Kable (nous avons complété avec paint ici)

champ13<-"Lecture : 83% des acteurs disposant de peu de soutien émotionnel [0,7] se trouvent dans des organisations où le crunch est très institutionnalisé [9,24]"
rprop(table(instit_emo$émotionnelle, instit_emo$organisationnelle)) %>%
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrr", digits = c(0,0,1,1),
        caption = "Relation entre la dimension émotionnelle et institutionnelle",
        label = "tableau1", position = "ht!") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%    
  footnote(general = c(champ13),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "5cm") %>% 
  column_spec(4, width = "3.5cm") 


##Fin du tableau 3.2.3 --------------------------------------------------
##-----------------------------------------------------------------------


## --------------------------------------------------------------------------
### Tableau 3.2.5 - Relation entre le degré de normalité et la dimension institutionnelle ----

#Création d'une nouvelle table normal_instit
normal_instit<-select(baseprimale4, organisationnelle, normalité, temps_crunch, crunchyn, temps_crunch, nbr_crunch, statut, temps_nocrunch, G04Q100..La.situation.de.Crunch.est.elle.qualifiée.explicitement.comme.du.Crunch... )

#Restriction de l'échantillon aux personnes travaillant en organisation 
normal_instit<-filter(normal_instit, statut=="Salarié CDD "| statut=="Salarié CDI" | statut=="Stagiaire" )

#Création des tranches pour la variable d'institutionnalisation et de normalité 
normal_instit$organisationnelle<-cut(normal_instit$organisationnelle, c(0,9,24), include.lowest= T, right=F)
normal_instit<-normal_instit[!(rowSums(is.na(normal_instit[1]))),]
normal_instit$normalité<-cut(normal_instit$normalité, c(0,9,15,24), include.lowest= T, right=F)

#Mise en forme du tableau 

champ15<-"Lecture : 67% des acteurs considérant la pratique du crunch comme relativement normale [9,15) travaillent au sein d'organisation où la pratique est fortement institutionnalisée [9,24]"

rprop(table(normal_instit$normalité, normal_instit$organisationnelle))%>%
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrr", digits = c(0,0,1,1),
        caption = "Relation entre le degré de normalité et la dimension institutionnelle ",
        label = "tableau1", position = "ht!") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%    
  footnote(general = c(champ15),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "5cm") %>% 
  column_spec(4, width = "3.5cm") 

## Fin du tableau 3.2.5
###--------------------------------------------------