# Recodage quantitatif des horaires du crunch -------------------------------------

# Encodage du fichier : UTF-8

## Aide au lecteur ------------------------------------- 
# Nous allons recoder le temps de travail horaire effectif de chaque répondant
# Cette horaire nous a été communiquée de manière journalière sur une plage de une semaine
# Nous disposons de l'heure de début de travail, de fin de travail et de pause 
# Nous allons donc calculer le temps horaire de travail journalier puis hebdomadaire
# Le recodage est exactement le même pour tous les jours de la semaine
# -------------------------------------

baseprimale2<-baseprimale

# Renommer les variables correspondant à l'horaire de début et de fin de travail du lundi 
baseprimale2$temps_crunchlm <- baseprimale2$G00Q83.SQ001_SQ001...Lors.de.votre.dernière.période.de.Crunch..sur.une.semaine.quelles.ont.été.vos.modalités.de.travail.....Lundi..Horaire.de.début.de.travail..
baseprimale2$temps_crunchls<- baseprimale2$G00Q83.SQ001_SQ002...Lors.de.votre.dernière.période.de.Crunch..sur.une.semaine.quelles.ont.été.vos.modalités.de.travail.....Lundi..Horaire.de.fin.de.travail..

# Recodage des différentes horaires du lundi matin en caractères numériques sous un format UTC universel
## Ce format permettra ensuite de calculer des différences d'horaires
## Le choix de la date 01/01/2000 n'a aucune influence sur le calcul

baseprimale2$temps_crunchlm <- baseprimale2$temps_crunchlm %>%
  fct_recode(
    NULL = "",
    "2000-01-01 08:30:00" = "08:30",
    "2000-01-01 08:30:00" = "08h30",
    "2000-01-01 10:00:00" = "10",
    "2000-01-01 10:00:00" = "10h",
    "2000-01-01 02:00:00" = "2h",
    "2000-01-01 07:00:00" = "7h",
    "2000-01-01 08:00:00" = "8h",
    "2000-01-01 08:40:00" = "8h40",
    "2000-01-01 09:00:00" = "9",
    "2000-01-01 09:00:00" = "9h",
    "2000-01-01 09:00:00" = "9h00",
    "2000-01-01 00:00:00" = "0"
  )

# Recodage des différentes horaires du lundi soir en caractères numériques sous un format UTC universel

baseprimale2$temps_crunchls <- baseprimale2$temps_crunchls %>%
  fct_recode(
    NULL = "",
    "2000-01-01 17:00:00" = "17h",
    "2000-01-01 18:00:00" = "18h",
    "2000-01-01 19:00:00" = "19",
    "2000-01-01 19:00:00" = "19h00",
    "2000-01-01 20:00:00" = "20",
    "2000-01-01 20:00:00" = "20h",
    "2000-01-01 22:00:00" = "22",
    "2000-01-01 22:00:00" = "22h",
    "2000-01-01 23:30:00" = "22h30",
    "2000-01-01 23:00:00" = "23h",
    "2000-01-02 02:00:00" = "2h",
    "2000-01-01 00:00:00" = "0"
  )

# Changement de format arrondissant à l'heure près et permettant calculs de différence horaire  
baseprimale2$temps_crunchlm <- as_datetime(baseprimale2$temps_crunchlm)
baseprimale2$temps_crunchls <- as_datetime(baseprimale2$temps_crunchls)

# Calcul de la différence horaire et création d'une variable associée
baseprimale2$lundi_crunch<-abs(difftime(baseprimale2$temps_crunchlm, baseprimale2$temps_crunchls, units="hour"))


### Nous répétons la manipulation de la même manière pour les autres jours de la semaine
### -------------------------------------



#Recodage du Mardi
baseprimale2$temps_crunchmm <- baseprimale2$G00Q83.SQ002_SQ001...Lors.de.votre.dernière.période.de.Crunch..sur.une.semaine.quelles.ont.été.vos.modalités.de.travail.....Mardi..Horaire.de.début.de.travail..
baseprimale2$temps_crunchms<- baseprimale2$G00Q83.SQ002_SQ002...Lors.de.votre.dernière.période.de.Crunch..sur.une.semaine.quelles.ont.été.vos.modalités.de.travail.....Mardi..Horaire.de.fin.de.travail.. 

# Recodage en horaire plus classique
baseprimale2$temps_crunchmm <- baseprimale2$temps_crunchmm %>%
  fct_recode(
    NULL = "",
    "2000-01-01 08:30:00" = "08:30",
    "2000-01-01 08:30:00" = "08h30",
    "2000-01-01 10:00:00" = "10",
    "2000-01-01 10:00:00" = "10h",
    "2000-01-01 02:00:00" = "2h",
    "2000-01-01 07:00:00" = "7h",
    "2000-01-01 08:00:00" = "8h",
    "2000-01-01 08:40:00" = "8h40",
    "2000-01-01 09:00:00" = "9",
    "2000-01-01 09:00:00" = "9h",
    "2000-01-01 09:00:00" = "9h00"
  )

baseprimale2$temps_crunchms <- baseprimale2$temps_crunchms %>%
  fct_recode(
    NULL = "",
    "2000-01-01 17:00:00" = "17h",
    "2000-01-01 18:00:00" = "18h",
    "2000-01-01 19:00:00" = "19",
    "2000-01-01 19:00:00" = "19h00",
    "2000-01-01 20:00:00" = "20",
    "2000-01-01 20:00:00" = "20h",
    "2000-01-01 22:00:00" = "22",
    "2000-01-01 22:00:00" = "22h",
    "2000-01-01 23:30:00" = "22h30",
    "2000-01-01 23:00:00" = "23h",
    "2000-01-02 02:00:00" = "2h"
  )

baseprimale2$temps_crunchmm <- as_datetime(baseprimale2$temps_crunchmm)
baseprimale2$temps_crunchms <- as_datetime(baseprimale2$temps_crunchms)
baseprimale2$mardi_crunch<-abs(difftime(baseprimale2$temps_crunchmm, baseprimale2$temps_crunchms))



#Recodage du Mercredi
baseprimale2$temps_crunchm2m <- baseprimale2$G00Q83.SQ003_SQ001...Lors.de.votre.dernière.période.de.Crunch..sur.une.semaine.quelles.ont.été.vos.modalités.de.travail.....Mercredi..Horaire.de.début.de.travail.. 
baseprimale2$temps_crunchm2s<- baseprimale2$G00Q83.SQ003_SQ002...Lors.de.votre.dernière.période.de.Crunch..sur.une.semaine.quelles.ont.été.vos.modalités.de.travail.....Mercredi..Horaire.de.fin.de.travail.. 

# Recodage en horaire plus classique
baseprimale2$temps_crunchm2m <- baseprimale2$temps_crunchm2m %>%
  fct_recode(
    NULL = "",
    "2000-01-01 08:30:00" = "08:30",
    "2000-01-01 08:30:00" = "08h30",
    "2000-01-01 10:00:00" = "10",
    "2000-01-01 10:00:00" = "10h",
    "2000-01-01 02:00:00" = "2h",
    "2000-01-01 07:00:00" = "7h",
    "2000-01-01 08:00:00" = "8h",
    "2000-01-01 08:40:00" = "8h40",
    "2000-01-01 09:00:00" = "9",
    "2000-01-01 09:00:00" = "9h",
    "2000-01-01 09:00:00" = "9h00"
  )

baseprimale2$temps_crunchm2s <- baseprimale2$temps_crunchm2s %>%
  fct_recode(
    NULL = "",
    "2000-01-01 17:00:00" = "17h",
    "2000-01-01 18:00:00" = "18h",
    "2000-01-01 19:00:00" = "19",
    "2000-01-01 19:00:00" = "19h00",
    "2000-01-01 20:00:00" = "20",
    "2000-01-01 20:00:00" = "20h",
    "2000-01-01 22:00:00" = "22",
    "2000-01-01 22:00:00" = "22h",
    "2000-01-01 23:30:00" = "22h30",
    "2000-01-01 23:00:00" = "23h",
    "2000-01-02 02:00:00" = "2h"
  )

baseprimale2$temps_crunchm2m <- as_datetime(baseprimale2$temps_crunchm2m)
baseprimale2$temps_crunchm2s <- as_datetime(baseprimale2$temps_crunchm2s)
baseprimale2$mercredi_crunch<-abs(difftime(baseprimale2$temps_crunchm2m, baseprimale2$temps_crunchm2s))



#Recodage du Jeudi
baseprimale2$temps_crunchjm <- baseprimale2$G00Q83.SQ004_SQ001...Lors.de.votre.dernière.période.de.Crunch..sur.une.semaine.quelles.ont.été.vos.modalités.de.travail.....Jeudi..Horaire.de.début.de.travail.. 
baseprimale2$temps_crunchjs<- baseprimale2$G00Q83.SQ004_SQ002...Lors.de.votre.dernière.période.de.Crunch..sur.une.semaine.quelles.ont.été.vos.modalités.de.travail.....Jeudi..Horaire.de.fin.de.travail.. 

# Recodage en horaire plus classique
baseprimale2$temps_crunchjm <- baseprimale2$temps_crunchjm %>%
  fct_recode(
    NULL = "",
    "2000-01-01 08:30:00" = "08:30",
    "2000-01-01 08:30:00" = "08h30",
    "2000-01-01 10:00:00" = "10",
    "2000-01-01 10:00:00" = "10h",
    "2000-01-01 02:00:00" = "2h",
    "2000-01-01 07:00:00" = "7h",
    "2000-01-01 08:00:00" = "8h",
    "2000-01-01 08:40:00" = "8h40",
    "2000-01-01 09:00:00" = "9",
    "2000-01-01 09:00:00" = "9h",
    "2000-01-01 09:00:00" = "9h00"
  )

baseprimale2$temps_crunchjs <- baseprimale2$temps_crunchjs %>%
  fct_recode(
    NULL = "",
    "2000-01-01 17:00:00" = "17h",
    "2000-01-01 18:00:00" = "18h",
    "2000-01-01 19:00:00" = "19",
    "2000-01-01 19:00:00" = "19h00",
    "2000-01-01 20:00:00" = "20",
    "2000-01-01 20:00:00" = "20h",
    "2000-01-01 22:00:00" = "22",
    "2000-01-01 22:00:00" = "22h",
    "2000-01-01 23:30:00" = "22h30",
    "2000-01-01 23:00:00" = "23h",
    "2000-01-02 02:00:00" = "2h",
    "2000-01-01 19:30:00" = "19:30",
    "2000-01-01 19:00:00" = "19h",
    "2000-01-01 16:00:00" = "16"
  )

baseprimale2$temps_crunchjm <- as_datetime(baseprimale2$temps_crunchjm)
baseprimale2$temps_crunchjs <- as_datetime(baseprimale2$temps_crunchjs)
baseprimale2$jeudi_crunch<-abs(difftime(baseprimale2$temps_crunchjm, baseprimale2$temps_crunchjs))



#Recodage du Vendredi
baseprimale2$temps_crunchvm <- baseprimale2$G00Q83.SQ005_SQ001...Lors.de.votre.dernière.période.de.Crunch..sur.une.semaine.quelles.ont.été.vos.modalités.de.travail.....Vendredi..Horaire.de.début.de.travail.. 
baseprimale2$temps_crunchvs<- baseprimale2$G00Q83.SQ005_SQ002...Lors.de.votre.dernière.période.de.Crunch..sur.une.semaine.quelles.ont.été.vos.modalités.de.travail.....Vendredi..Horaire.de.fin.de.travail.. 

# Recodage en horaire plus classique
baseprimale2$temps_crunchvm <- baseprimale2$temps_crunchvm %>%
  fct_recode(
    NULL = "",
    "2000-01-01 08:30:00" = "08:30",
    "2000-01-01 08:30:00" = "08h30",
    "2000-01-01 10:00:00" = "10",
    "2000-01-01 10:00:00" = "10h",
    "2000-01-01 02:00:00" = "2h",
    "2000-01-01 07:00:00" = "7h",
    "2000-01-01 08:00:00" = "8h",
    "2000-01-01 08:40:00" = "8h40",
    "2000-01-01 09:00:00" = "9",
    "2000-01-01 09:00:00" = "9h",
    "2000-01-01 09:00:00" = "9h00"
  )

baseprimale2$temps_crunchvs <- baseprimale2$temps_crunchvs %>%
  fct_recode(
    NULL = "",
    "2000-01-01 18:00:00" = "18h",
    "2000-01-01 19:00:00" = "19",
    "2000-01-01 20:00:00" = "20",
    "2000-01-01 20:00:00" = "20h",
    "2000-01-01 22:00:00" = "22h",
    "2000-01-01 23:00:00" = "23h",
    "2000-01-02 02:00:00" = "2h",
    "2000-01-01 18:30:00" = "18:30"
  )

baseprimale2$temps_crunchvm <- as_datetime(baseprimale2$temps_crunchvm)
baseprimale2$temps_crunchvs<- as.character(baseprimale2$temps_crunchvs)
baseprimale2$temps_crunchvs <- as_datetime(baseprimale2$temps_crunchvs, tz="UTC")
baseprimale2$vendredi_crunch<-abs(difftime(baseprimale2$temps_crunchvm, baseprimale2$temps_crunchvs))



#Recodage du Samedi
baseprimale2$temps_crunchsm <- baseprimale2$G00Q83.SQ006_SQ001...Lors.de.votre.dernière.période.de.Crunch..sur.une.semaine.quelles.ont.été.vos.modalités.de.travail.....Samedi..Horaire.de.début.de.travail.. 
baseprimale2$temps_crunchss<- baseprimale2$G00Q83.SQ006_SQ002...Lors.de.votre.dernière.période.de.Crunch..sur.une.semaine.quelles.ont.été.vos.modalités.de.travail.....Samedi..Horaire.de.fin.de.travail.. 

# Recodage en horaire plus classique
baseprimale2$temps_crunchsm <- baseprimale2$temps_crunchsm %>%
  fct_recode(
    NULL = "",
    "2000-01-01 08:30:00" = "08:30",
    "2000-01-01 08:30:00" = "08h30",
    "2000-01-01 10:00:00" = "10",
    "2000-01-01 10:00:00" = "10h",
    "2000-01-01 02:00:00" = "2h",
    "2000-01-01 07:00:00" = "7h",
    "2000-01-01 08:00:00" = "8h",
    "2000-01-01 08:40:00" = "8h40",
    "2000-01-01 09:00:00" = "9",
    "2000-01-01 09:00:00" = "9h",
    "2000-01-01 09:00:00" = "9h00",
    "2000-01-01 00:00:00" = "0",
    "2000-01-01 00:00:00" = "0h"
    
  )

baseprimale2$temps_crunchss <- baseprimale2$temps_crunchss %>%
  fct_recode(
    NULL = "",
    "2000-01-01 17:00:00" = "17h",
    "2000-01-01 18:00:00" = "18h",
    "2000-01-01 19:00:00" = "19",
    "2000-01-01 19:00:00" = "19h00",
    "2000-01-01 20:00:00" = "20",
    "2000-01-01 20:00:00" = "20h",
    "2000-01-01 22:00:00" = "22",
    "2000-01-01 22:00:00" = "22h",
    "2000-01-01 23:30:00" = "22h30",
    "2000-01-01 23:00:00" = "23h",
    "2000-01-02 02:00:00" = "2h",
    "2000-01-01 00:00:00" = "0",
    "2000-01-01 00:00:00" = "0h",
    "2000-01-01 12:00:00" = "12h",
    "2000-01-01 13:00:00" = "13h",
    "2000-01-01 16:00:00" = "16h",
    "2000-01-01 17:30:00" = "17-18h"
  )


baseprimale2$temps_crunchsm <- as_datetime(baseprimale2$temps_crunchsm) 
baseprimale2$temps_crunchss <- as_datetime(baseprimale2$temps_crunchss)
baseprimale2$samedi_crunch<-abs(difftime(baseprimale2$temps_crunchsm, baseprimale2$temps_crunchss, units="hours"))



#Recodage du Dimanche
baseprimale2$temps_crunchdm <- baseprimale2$G00Q83.SQ007_SQ001...Lors.de.votre.dernière.période.de.Crunch..sur.une.semaine.quelles.ont.été.vos.modalités.de.travail.....Dimanche..Horaire.de.début.de.travail.. 
baseprimale2$temps_crunchds<- baseprimale2$G00Q83.SQ007_SQ002...Lors.de.votre.dernière.période.de.Crunch..sur.une.semaine.quelles.ont.été.vos.modalités.de.travail.....Dimanche..Horaire.de.fin.de.travail.. 

# Recodage en horaire plus classique
baseprimale2$temps_crunchdm <- baseprimale2$temps_crunchdm %>%
  fct_recode(
    NULL = "",
    "2000-01-01 08:30:00" = "08:30",
    "2000-01-01 08:30:00" = "08h30",
    "2000-01-01 10:00:00" = "10",
    "2000-01-01 10:00:00" = "10h",
    "2000-01-01 02:00:00" = "2h",
    "2000-01-01 07:00:00" = "7h",
    "2000-01-01 08:00:00" = "8h",
    "2000-01-01 08:40:00" = "8h40",
    "2000-01-01 09:00:00" = "9",
    "2000-01-01 09:00:00" = "9h",
    "2000-01-01 09:00:00" = "9h00",
    "2000-01-01 00:00:00" = "0",
    "2000-01-01 00:00:00" = "0h"
  )

baseprimale2$temps_crunchds <- baseprimale2$temps_crunchds %>%
  fct_recode(
    NULL = "",
    "2000-01-01 17:00:00" = "17h",
    "2000-01-01 18:00:00" = "18h",
    "2000-01-01 19:00:00" = "19",
    "2000-01-01 19:00:00" = "19h00",
    "2000-01-01 20:00:00" = "20",
    "2000-01-01 20:00:00" = "20h",
    "2000-01-01 22:00:00" = "22",
    "2000-01-01 22:00:00" = "22h",
    "2000-01-01 23:30:00" = "22h30",
    "2000-01-01 23:00:00" = "23h",
    "2000-01-02 02:00:00" = "2h",
    "2000-01-02 02:00:00" = "2h",
    "2000-01-01 00:00:00" = "0",
    "2000-01-01 00:00:00" = "0h",
    "2000-01-01 12:00:00" = "12h",
    "2000-01-01 13:00:00" = "13h",
    "2000-01-01 16:00:00" = "16h",
    "2000-01-01 17:30:00" = "17-18h",
    "2000-01-01 21:00:00" = "21h"
  )

baseprimale2$temps_crunchdm <- as_datetime(baseprimale2$temps_crunchdm)
baseprimale2$temps_crunchds <- as_datetime(baseprimale2$temps_crunchds)
baseprimale2$dimanche_crunch<-abs(difftime(baseprimale2$temps_crunchdm, baseprimale2$temps_crunchds, units="hours"))

### Fin du recodage horaire  
### -------------------------------------




# Création des variables Pause de tous les jours de la semaine
baseprimale2$pause_lundi<-baseprimale2$G00Q83.SQ001_SQ003...Lors.de.votre.dernière.période.de.Crunch..sur.une.semaine.quelles.ont.été.vos.modalités.de.travail.....Lundi..Temps.de.pause.total..
baseprimale2$pause_mardi<-baseprimale2$G00Q83.SQ002_SQ003...Lors.de.votre.dernière.période.de.Crunch..sur.une.semaine.quelles.ont.été.vos.modalités.de.travail.....Mardi..Temps.de.pause.total..
baseprimale2$pause_mercredi<-baseprimale2$G00Q83.SQ003_SQ003...Lors.de.votre.dernière.période.de.Crunch..sur.une.semaine.quelles.ont.été.vos.modalités.de.travail.....Mercredi..Temps.de.pause.total..
baseprimale2$pause_jeudi<-baseprimale2$G00Q83.SQ004_SQ003...Lors.de.votre.dernière.période.de.Crunch..sur.une.semaine.quelles.ont.été.vos.modalités.de.travail.....Jeudi..Temps.de.pause.total..
baseprimale2$pause_vendredi<-baseprimale2$G00Q83.SQ005_SQ003...Lors.de.votre.dernière.période.de.Crunch..sur.une.semaine.quelles.ont.été.vos.modalités.de.travail.....Vendredi..Temps.de.pause.total..
baseprimale2$pause_samedi<-baseprimale2$G00Q83.SQ006_SQ003...Lors.de.votre.dernière.période.de.Crunch..sur.une.semaine.quelles.ont.été.vos.modalités.de.travail.....Samedi..Temps.de.pause.total..
baseprimale2$pause_dimanche<-baseprimale2$G00Q83.SQ007_SQ003...Lors.de.votre.dernière.période.de.Crunch..sur.une.semaine.quelles.ont.été.vos.modalités.de.travail.....Dimanche..Temps.de.pause.total..

#Pause Lundi 
## Recodage des différentes horaires de la pause du lundi en caractères numériques sous un format UTC universel
## Ce format permettra ensuite de calculer des différences d'horaires
## Le choix de la date 01/01/2000 n'a aucune influence sur le calcul

baseprimale2$pause_lundi <- baseprimale2$pause_lundi %>%
  fct_recode(
    NULL = "",
    "2000-01-01 01:00:00" = "1",
    "2000-01-01 01:00:00" = "1h",
    "2000-01-01 01:30:00" = "1-2h",
    "2000-01-01 01:30:00" = "1h30",
    "2000-01-01 02:00:00" = "2",
    "2000-01-01 02:00:00" = "2h",
    "2000-01-01 02:30:00" = "2h30",
    "2000-01-01 00:30:00" = "30min",
    "2000-01-01 04:00:00" = "4",
    "2000-01-01 04:00:00" = "4h",
    "2000-01-01 00:00:00" = "-",
    "2000-01-01 00:00:00" = "0",
    "2000-01-01 00:00:00" = "0h",
  )

# Changement de format arrondissant à l'heure près et permettant calculs de différence horaire  
baseprimale2$pause_lundi <- as_datetime(baseprimale2$pause_lundi)

# Calcul de la différence horaire avec le 01/01/2000 à 00:00:00 
# Permet d'obtenir l'heure non en format UTC mais sous format numérique en heure 
baseprimale2$pause_lundi<-abs(difftime(time1 = "2000-01-01 00:00:00", baseprimale2$pause_lundi, units="hours"))

### Nous répétons la manipulation de la même manière pour les autres pauses des jours de la semaine
### -------------------------------------


# Pause mardi 
baseprimale2$pause_mardi <- baseprimale2$pause_mardi %>%
  fct_recode(
    NULL = "",
    "2000-01-01 01:00:00" = "1",
    "2000-01-01 01:00:00" = "1h",
    "2000-01-01 01:30:00" = "1-2h",
    "2000-01-01 01:30:00" = "1h30",
    "2000-01-01 02:00:00" = "2",
    "2000-01-01 02:00:00" = "2h",
    "2000-01-01 02:30:00" = "2h30",
    "2000-01-01 00:30:00" = "30min",
    "2000-01-01 04:00:00" = "4",
    "2000-01-01 04:00:00" = "4h",
    "2000-01-01 00:00:00" = "-",
    "2000-01-01 00:00:00" = "0",
    "2000-01-01 00:00:00" = "0h",
  )

baseprimale2$pause_mardi <- as_datetime(baseprimale2$pause_mardi)
baseprimale2$pause_mardi<-abs(difftime(time1 = "2000-01-01 00:00:00", baseprimale2$pause_mardi, units="hours"))


# Pause mercredi
baseprimale2$pause_mercredi <- baseprimale2$pause_mercredi %>%
  fct_recode(
    NULL = "",
    "2000-01-01 01:00:00" = "1",
    "2000-01-01 01:00:00" = "1h",
    "2000-01-01 01:30:00" = "1-2h",
    "2000-01-01 01:30:00" = "1h30",
    "2000-01-01 02:00:00" = "2",
    "2000-01-01 02:00:00" = "2h",
    "2000-01-01 02:30:00" = "2h30",
    "2000-01-01 00:30:00" = "30min",
    "2000-01-01 04:00:00" = "4",
    "2000-01-01 04:00:00" = "4h",
    "2000-01-01 00:00:00" = "-",
    "2000-01-01 00:00:00" = "0",
    "2000-01-01 00:00:00" = "0h",
  )

baseprimale2$pause_mercredi <- as_datetime(baseprimale2$pause_mercredi)
baseprimale2$pause_mercredi<-abs(difftime(time1 = "2000-01-01 00:00:00", baseprimale2$pause_mercredi, units="hours"))


# Pause Jeudi
baseprimale2$pause_jeudi <- baseprimale2$pause_jeudi %>%
  fct_recode(
    NULL = "",
    "2000-01-01 01:00:00" = "1",
    "2000-01-01 01:00:00" = "1h",
    "2000-01-01 01:30:00" = "1-2h",
    "2000-01-01 01:30:00" = "1h30",
    "2000-01-01 02:00:00" = "2",
    "2000-01-01 02:00:00" = "2h",
    "2000-01-01 02:30:00" = "2h30",
    "2000-01-01 00:30:00" = "30min",
    "2000-01-01 04:00:00" = "4",
    "2000-01-01 04:00:00" = "4h",
    "2000-01-01 00:00:00" = "-",
    "2000-01-01 00:00:00" = "0",
    "2000-01-01 00:00:00" = "0h",
  )

baseprimale2$pause_jeudi <- as_datetime(baseprimale2$pause_jeudi)
baseprimale2$pause_jeudi<-abs(difftime(time1 = "2000-01-01 00:00:00", baseprimale2$pause_jeudi, units="hours"))


# Pause Vendredi
baseprimale2$pause_vendredi <- baseprimale2$pause_vendredi %>%
  fct_recode(
    NULL = "",
    "2000-01-01 01:00:00" = "1",
    "2000-01-01 01:00:00" = "1h",
    "2000-01-01 01:30:00" = "1-2h",
    "2000-01-01 01:30:00" = "1h30",
    "2000-01-01 02:00:00" = "2",
    "2000-01-01 02:00:00" = "2h",
    "2000-01-01 02:30:00" = "2h30",
    "2000-01-01 00:30:00" = "30min",
    "2000-01-01 04:00:00" = "4",
    "2000-01-01 04:00:00" = "4h",
    "2000-01-01 00:00:00" = "-",
    "2000-01-01 00:00:00" = "0",
    "2000-01-01 00:00:00" = "0h",
  )

baseprimale2$pause_vendredi <- as_datetime(baseprimale2$pause_vendredi)
baseprimale2$pause_vendredi<-abs(difftime(time1 = "2000-01-01 00:00:00", baseprimale2$pause_vendredi, units="hours"))


# Pause Samedi
baseprimale2$pause_samedi <- baseprimale2$pause_samedi %>%
  fct_recode(
    NULL = "",
    "2000-01-01 01:00:00" = "1",
    "2000-01-01 01:00:00" = "1h",
    "2000-01-01 01:30:00" = "1-2h",
    "2000-01-01 01:30:00" = "1h30",
    "2000-01-01 02:00:00" = "2",
    "2000-01-01 02:00:00" = "2h",
    "2000-01-01 02:30:00" = "2h30",
    "2000-01-01 00:30:00" = "30min",
    "2000-01-01 04:00:00" = "4",
    "2000-01-01 04:00:00" = "4h",
    "2000-01-01 00:00:00" = "-",
    "2000-01-01 00:00:00" = "0",
    "2000-01-01 00:00:00" = "0h",
  )

baseprimale2$pause_samedi <- as_datetime(baseprimale2$pause_samedi)
baseprimale2$pause_samedi<-abs(difftime(time1 = "2000-01-01 00:00:00", baseprimale2$pause_samedi, units="hours"))


# Pause Dimanche
baseprimale2$pause_dimanche <- baseprimale2$pause_dimanche %>%
  fct_recode(
    NULL = "",
    "2000-01-01 01:00:00" = "1",
    "2000-01-01 01:00:00" = "1h",
    "2000-01-01 01:30:00" = "1-2h",
    "2000-01-01 01:30:00" = "1h30",
    "2000-01-01 02:00:00" = "2",
    "2000-01-01 02:00:00" = "2h",
    "2000-01-01 02:30:00" = "2h30",
    "2000-01-01 00:30:00" = "30min",
    "2000-01-01 04:00:00" = "4",
    "2000-01-01 04:00:00" = "4h",
    "2000-01-01 00:00:00" = "-",
    "2000-01-01 00:00:00" = "0",
    "2000-01-01 00:00:00" = "0h",
  )

baseprimale2$pause_dimanche <- as_datetime(baseprimale2$pause_dimanche)
baseprimale2$pause_dimanche<-abs(difftime(time1 = "2000-01-01 00:00:00", baseprimale2$pause_dimanche, units="hours"))


### Fin du recodage horaire des pauses  
### -------------------------------------


## Recodage des horaires journaliers de travail effectif en retranchant les heures de travail
## Au temps de pause journalier 
baseprimale2$lundi_crunch <-baseprimale2$lundi_crunch - baseprimale2$pause_lundi
baseprimale2$mardi_crunch <-baseprimale2$mardi_crunch - baseprimale2$pause_mardi
baseprimale2$mercredi_crunch <-baseprimale2$mercredi_crunch - baseprimale2$pause_mercredi
baseprimale2$jeudi_crunch <-baseprimale2$jeudi_crunch - baseprimale2$pause_jeudi
baseprimale2$vendredi_crunch  <- baseprimale2$vendredi_crunch - baseprimale2$pause_vendredi
baseprimale2$samedi_crunch <- baseprimale2$samedi_crunch - baseprimale2$pause_samedi
baseprimale2$dimanche_crunch <- baseprimale2$dimanche_crunch - baseprimale2$pause_dimanche


## Création de la variable travail hebdomadaire effectif 
baseprimale2$semaine<- baseprimale2$lundi_crunch + baseprimale2$mardi_crunch
+ baseprimale2$mercredi_crunch + baseprimale2$jeudi_crunch + baseprimale2$vendredi_crunch+
  baseprimale2$samedi_crunch + baseprimale2$dimanche_crunch


### Recodage alternatif des horaires de crunch -------------------------------------
# Recodage des différentes horaires de crunch à partir de plages horaires 
# On fait le choix de faire une moyenne du minimum et maximum horaire
# On traduit les caractères numériques sous un format UTC universel
# Le choix de la date 01/01/2000 n'a aucune influence sur le calcul

baseprimale2$temps_crunch <- baseprimale2$temps_crunch %>%
  fct_recode(
    NULL = "",
    "2000-01-02 23:00:00" = "45-49h",
    "2000-01-02 16:00:00" = "40h",
    "2000-01-03 17:30:00" = "60-69h",
    "2000-01-03 07:30:00" = "50-59h",
    "2000-01-03 23:00:00" = "70h ou plus",
  )

#Recodage permettant d'obtenir une variable expérimant les heures sous format numérique
baseprimale2$temps_crunch <- as_datetime(baseprimale2$temps_crunch)
baseprimale2$temps_crunch<-abs(difftime(time1 = "2000-01-01 00:30:00", baseprimale2$temps_crunch, units="hours"))

#Fin du recodage numérique du crunch------------------- 
#Nous utiliserons ces deux outils pour calculer les durées moyennes hebdomadaires du crunch




### Figure 1.3.1 - Horaire journalier au cours d'une semaine de crunch ----

# Création d'une nouvelle variable (semainecrunch) à partir des temps journalier de crunch 
semainecrunch<-select(baseprimale2, lundi_crunch,mardi_crunch, mercredi_crunch,jeudi_crunch,vendredi_crunch,samedi_crunch,dimanche_crunch,statut, temps_crunch)

# Suppression des lignes avec des NA (en se basant sur les non réponses des horaires du lundi matin)
semainecrunch<-semainecrunch[!(rowSums(is.na(semainecrunch[1]))),] 

# Changement de noms des variables pour les rendre plus lisible 
semainecrunch$statut<-fct_collapse(semainecrunch$statut, "Indépendant"="Auto-Entrepreneur (Personne Publiant son propre jeu)", "CDD"="Salarié CDD", "CDI"="Salarié CDI")

# Création de figure_1_3_1 en réunissant les acteurs en fonction de leur statut et en faisant la moyenne des horaires journaliers de crunch
figure_1_3_1<-semainecrunch %>%
  group_by(statut) %>%
  summarise( horaire=mean(lundi_crunch, na.rm=T), nombre2=mean(mardi_crunch, na.rm=T)
             ,nombre3=mean(mercredi_crunch, na.rm=T),nombre4=mean(jeudi_crunch, na.rm=T),
             nombre5=mean(vendredi_crunch, na.rm=T), nombre6=mean(samedi_crunch, na.rm=T), nombre7=mean(dimanche_crunch, na.rm=T))

# Création d'un graphique lisible sous forme de geom_bar

figure_1_3_1<-as.data.frame(figure_1_3_1)

ggplot(figure_1_3_1, aes(x=statut, y = horaire))+
  geom_point(aes(x=statut,y=horaire),  col="pink")+   #Lundi point rose
  geom_point(aes(x=statut,y=nombre2), col="blue")+   #Mardi point bleu
  geom_point(aes(x=statut,y=nombre3), col="red")+    #Mercredi point rouge
  geom_point(aes(x=statut,y=nombre4), col="green")+  #Jeudi point vert
  geom_point(aes(x=statut,y=nombre5), col="black")+  #Vendredi point noir
  geom_point(aes(x=statut,y=nombre6), col="purple")+ #Samedi point violet
  geom_point(aes(x=statut,y=nombre7), col="yellow")+ #Dimanche point jaune
  labs(title="Horaire journalier au cours d'une semaine de crunch", x="Statut de l'acteur",y="Horaire journalier")

### Fin Figure 1.3.1 ---------------------------------------------

##---------------------------------------------------
### Tableau 1.3.2 - Horaire moyen hebdomadaire de travail en période de crunch ----

# Création d'une nouvelle variable prenant en compte les horaires journalier en période de crunch
semainecrunch2<-select(baseprimale2, lundi_crunch,mardi_crunch, mercredi_crunch,jeudi_crunch,vendredi_crunch,samedi_crunch,dimanche_crunch,statut, temps_crunch)

# Suppression des lignes possédant des NA sur les horaires du Lundi 
semainecrunch2<-semainecrunch2[!(rowSums(is.na(semainecrunch2[1]))),]

# Changement de nom des modalités pour une meilleure visualisation
semainecrunch2$statut<-fct_collapse(semainecrunch2$statut, "Indépendant"="Auto-Entrepreneur (Personne Publiant son propre jeu)", "Salarié CDD" = "Stagiaire")
semainecrunch2$statut<-fct_collapse(semainecrunch2$statut, "Salarié au statut précaire"= "Salarié CDD")

# On crée la table qui Regroupe par statut de travailleur en faisant la moyenne journalière travaillée
tableau1_3_2<-semainecrunch2 %>%
  group_by(statut) %>%
  summarise( horaire=mean(lundi_crunch, na.rm=T), nombre2=mean(mardi_crunch, na.rm=T)
             ,nombre3=mean(mercredi_crunch, na.rm=T),nombre4=mean(jeudi_crunch, na.rm=T),
             nombre5=mean(vendredi_crunch, na.rm=T), nombre6=mean(samedi_crunch, na.rm=T), nombre7=mean(dimanche_crunch, na.rm=T))

# On crée une nouvelle variable correspond à l'horaire hebdomadaire moyen en période de crunch
tableau1_3_2$total<-tableau1_3_2$horaire+tableau1_3_2$nombre2+
  tableau1_3_2$nombre3+tableau1_3_2$nombre4+tableau1_3_2$nombre5+
  tableau1_3_2$nombre6+tableau1_3_2$nombre7

#On supprime les variables inutiles 
tableau1_3_2<- select(tableau1_3_2, -nombre)
tableau1_3_2<- select(tableau1_3_2, -nombre1)
tableau1_3_2<- select(tableau1_3_2, -nombre2)
tableau1_3_2<- select(tableau1_3_2, -nombre3)
tableau1_3_2<- select(tableau1_3_2, -nombre4)
tableau1_3_2<- select(tableau1_3_2, -nombre5)
tableau1_3_2<- select(tableau1_3_2, -nombre6)
tableau1_3_2<- select(tableau1_3_2, -nombre7)
tableau1_3_2<- select(tableau1_3_2, -horaire)

# On effectue quelques modifications pour que ce soit plus lisible 
tableau1_3_2$total<-round(tableau1_3_2$total)
names(tableau1_3_2)[1:2] <- c("Statut du travailleur", "Horaire moyen hebdomadaire")

# Mise en page du tableau 

champ6<- "Lecture : Un indépendant travaille en moyenne 78 heures par semaine en période de crunch"

head(tableau1_3_2) %>%
  kable(format = "html", linesep = "", booktable=TRUE, caption = "Horaire moyen hebdomadaire de travail en période de Crunch", label="pages")%>%
  kable_styling(latex_options = c("striped", "HOLD_position", "repeat header"), repeat_header_text = "\\textit{(suite)})")%>%
  footnote(general = c(champ6),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "")

### Fin Tableau 1.3.2 ---------------------------------------------

##----------------------
### Tableau 1.3.3 - Horaire moyen hebdomadaire de travail en période de crunch en fonction du métier exercé ----

# Création d'une nouvelle variable prenant en compte les horaires journalier en période de crunch
semainecrunch3<-select(baseprimale2, lundi_crunch,mardi_crunch, mercredi_crunch,jeudi_crunch,vendredi_crunch,samedi_crunch,dimanche_crunch,statut, temps_crunch, métier, ipaddr..Adresse.IP)

#Suppression des lignes comportant des NA aux horaires du lundi matin 
semainecrunch3<-semainecrunch3[!(rowSums(is.na(semainecrunch3[1]))),] 

# Changement de nom des modalités pour une meilleure visualisation
semainecrunch3$statut<-fct_collapse(semainecrunch3$statut, "Indépendant"="Auto-Entrepreneur (Personne Publiant son propre jeu)", "CDD"="Salarié CDD", "CDI"="Salarié CDI")

# Création de la table faisant la moyenne de travail journalier en fonction de la profession
tableau1_3_3<-semainecrunch3 %>%
  group_by(métier) %>%
  summarise( horaire=mean(lundi_crunch, na.rm=T), nombre2=mean(mardi_crunch, na.rm=T)
             ,nombre3=mean(mercredi_crunch, na.rm=T),nombre4=mean(jeudi_crunch, na.rm=T),
             nombre5=mean(vendredi_crunch, na.rm=T), nombre6=mean(samedi_crunch, na.rm=T), nombre7=mean(dimanche_crunch, na.rm=T))

# On crée une nouvelle variable correspond à l'horaire hebdomadaire moyen en période de crunch
tableau1_3_3$total<-tableau1_3_3$horaire+tableau1_3_3$nombre2+
  tableau1_3_3$nombre3+tableau1_3_3$nombre4+tableau1_3_3$nombre5+
  tableau1_3_3$nombre6+tableau1_3_3$nombre7

# Suppression des colonnes correspondant aux horaires de chaque jour de la semaine (maintenant inutiles)
tableau1_3_3<- select(tableau1_3_3, -nombre)
tableau1_3_3<- select(tableau1_3_3, -nombre1)
tableau1_3_3<- select(tableau1_3_3, -nombre2)
tableau1_3_3<- select(tableau1_3_3, -nombre3)
tableau1_3_3<- select(tableau1_3_3, -nombre4)
tableau1_3_3<- select(tableau1_3_3, -nombre5)
tableau1_3_3<- select(tableau1_3_3, -nombre6)
tableau1_3_3<- select(tableau1_3_3, -nombre7) 
tableau1_3_3<- select(tableau1_3_3, -horaire)

# Suppression des métiers inexistants et des professions ne disposant pas d'horaires hebdomadaires
tableau1_3_3<-tableau1_3_3[!(rowSums(is.na(tableau1_3_3[1]))),] 
tableau1_3_3<-tableau1_3_3[!(rowSums(is.na(tableau1_3_3[2]))),] 

# On arrondit les heures de travail hebdomadaire 
tableau1_3_3$total<-round(tableau1_3_3$total)

# Changement de nom des modalités pour une meilleure visualisation
tableau1_3_3$métier<-fct_collapse(tableau1_3_3$métier, "Animation 2D-3D" = "Animation 2D 3D" 
                                      , "Game-designer" = "Game-Design", "Graphiste 2D-3D"= "Graphisme 2D 3D", "Manager" = "Production", "Programmeur" = "Programmation" , "Sound-designer" ="Sound-Design")

# Changement du nom des colonnes pour une meilleur visualisation
names(tableau1_3_3)[1:2] <- c("Métier exercé", "Horaire moyen hebdomadaire")

#Mise en page du tableau 

champ7<- "Lecture : Un manager travaille en moyenne 35 heures par semaine en période de crunch"

head(tableau1_3_3) %>%
  kable(format = "html", linesep = "", booktable=TRUE, caption = "Horaire moyen hebdomadaire de travail en période de Crunch en fonction du métier exercé", label="pages")%>%
  kable_styling(latex_options = c("striped", "HOLD_position", "repeat header"), repeat_header_text = "\\textit{(suite)})")%>%
  footnote(general = c(champ7),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "")


### Fin Tableau 1.3.3 ---------------------------------------------


### Tableau 1.3.4 -Le crunch durant les périodes de production en fonction du métier exercé----

#Premier tri de l'échantillon en sélectionnant uniquement les répondant ayant crunch
semainecrunch3<-filter(baseprimale2, crunchyn=="Oui")

#Suppression des lignes n'indiquant pas d'horaire hebdomadaire de crunch
semainecrunch3<-semainecrunch3[!(rowSums(is.na(semainecrunch3[124]))),] 


# Changement de nom des modalités pour une meilleure visualisation
semainecrunch3$période_crunch<-fct_collapse(semainecrunch3$période_crunch, "Au début de production" = "En Pré-production")
semainecrunch3$métier<-fct_collapse(semainecrunch3$métier, "Animation 2D-3D" = "Animation 2D 3D" 
                                   , "Game-designer" = "Game-Design", "Graphiste 2D-3D"= "Graphisme 2D 3D", "Manager" = "Production", "Programmeur" = "Programmation" , "Sound-designer" ="Sound-Design",
                                   "UI-UX Designer"="UI-UX Design", "créateur VFX"="VFX", "Testeur"="Test/QA")

# Mise en page du tableau 

champ8 <- "Lecture : 60% des crunch d'un Game-designer ont lieu à la fin de la production"

rprop(table(semainecrunch3$métier, semainecrunch3$période_crunch)) %>%
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrr", digits = c(0,0,1,1),
        caption = "Le crunch durant les périodes de production en fonction du métier exercé",
        label = "tableau1", position = "ht!") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%    
  footnote(general = c(champ8),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "5cm") %>% 
  column_spec(4, width = "3.5cm") 

### Fin Tableau 1.3.4 ---------------------------------------------

