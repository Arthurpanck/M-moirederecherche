# Nettoyage des réponses au questionnaire et recodage des questions imposé par le format de Lime-Survey-------------------------------------

# Encodage du fichier : UTF-8

## Importation des packages -----
library(tidyverse)
library(questionr)
library(dplyr)
library(stringr) 
library(lubridate)
library(kableExtra)
library(knitr)
library(FactoInvestigate)
library(GGally)
library(gtsummary)
library(tinytex)


# On importe la base de donnée -----
baseprimale<- read.csv2("C:/Users/Arthur PANCKOUCKE/Desktop/Mémoire/Analyse quantitative/Base de donnée sans code.csv", na = c("","NA","N/A"))

# Nettoyage----
## Suppression des répondants n'ayant pas répondu à la première question obligatoire
baseprimale<-baseprimale[!(rowSums(is.na(baseprimale[3]))),] 

##Suppression de colonne inexploitable 
baseprimale<- select(baseprimale, -refurl..URL.référente)
baseprimale<-select(baseprimale, -G03Q35.other...Quel.type.d.établissement.d.enseignement.supérieur.avez.vous.fréquenté............Autre.)

# Création de variables équivalentes a des variables intéressantes mais avec un nom plus court
baseprimale$statut<- baseprimale$G03Q38..Quel.est.votre.statut..
baseprimale$nbr_crunch<-baseprimale$G04Q87..Au.cours.de.2021..combien.de.fois.vous.êtes.vous.retrouvé.en.situation.de.Crunch...
baseprimale$crunchyn<-baseprimale$G03Q55..Avez.vous.déjà.crunché...
baseprimale$temps_nocrunch<-baseprimale$G02Q15..En.période.habituelle..par.semaine.il.est.normal.de.travailler

# Recodage---- 

## Recodage partiel de la durée du crunch 
baseprimale$temps_crunch<-baseprimale$G02Q81..Lors.de.votre.dernier.Crunch..par.semaine.il.était.normal.de.travailler...
baseprimale$temps_crunch<-fct_collapse(baseprimale$temps_crunch, "40h"="Moins de 40 heures")                                    

## Recodage de la fonction métier
baseprimale$métier<-baseprimale$G03Q31.SQ001...Quel.est.votre.métier......Animation..2D.3D..

baseprimale$métier = case_when(baseprimale$G03Q31.SQ001...Quel.est.votre.métier......Animation..2D.3D.. == "Oui"  ~ "Animation 2D 3D",
                               baseprimale$G03Q31.SQ003...Quel.est.votre.métier......Graphisme..2D.3D.. == "Oui"  ~ "Graphisme 2D 3D",
                               baseprimale$G03Q31.SQ002...Quel.est.votre.métier......Programmation. == "Oui"  ~ "Programmation ",
                               baseprimale$G03Q31.SQ004...Quel.est.votre.métier......VFX. == "Oui"  ~ "VFX",
                               baseprimale$G03Q31.SQ005...Quel.est.votre.métier......Production. == "Oui"  ~ "Production",
                               baseprimale$G03Q31.SQ006...Quel.est.votre.métier......Game.Design. == "Oui"  ~ "Game-Design",
                               baseprimale$G03Q31.SQ008...Quel.est.votre.métier......UI.UX.Design. == "Oui"  ~ "UI-UX-Design",
                               baseprimale$G03Q31.SQ010...Quel.est.votre.métier......Test..QA.. == "Oui"  ~ "Test/QA",
                               baseprimale$G03Q31.SQ009...Quel.est.votre.métier......Sound.Design. == "Oui"  ~ "Sound-Design",
                               baseprimale$G03Q31.SQ007...Quel.est.votre.métier......Marketing. == "Oui" ~ "Marketing" ,
                               baseprimale$G03Q31.other...Quel.est.votre.métier......Autre. == "Oui" ~ "Autre")

### Suppression des colonnes devenues inutiles 
baseprimale<- select(baseprimale, -G03Q31.SQ001...Quel.est.votre.métier......Animation..2D.3D..) 
baseprimale<- select(baseprimale, -G03Q31.SQ003...Quel.est.votre.métier......Graphisme..2D.3D..) 
baseprimale<- select(baseprimale, -G03Q31.SQ002...Quel.est.votre.métier......Programmation. )
baseprimale<- select(baseprimale, -G03Q31.SQ004...Quel.est.votre.métier......VFX. )
baseprimale<- select(baseprimale, -G03Q31.SQ005...Quel.est.votre.métier......Production.) 
baseprimale<- select(baseprimale, -G03Q31.SQ006...Quel.est.votre.métier......Game.Design.) 
baseprimale<- select(baseprimale, -G03Q31.SQ008...Quel.est.votre.métier......UI.UX.Design.) 
baseprimale<- select(baseprimale, -G03Q31.SQ010...Quel.est.votre.métier......Test..QA.. )
baseprimale<- select(baseprimale, -G03Q31.SQ009...Quel.est.votre.métier......Sound.Design.) 
baseprimale<- select(baseprimale, -G03Q31.SQ007...Quel.est.votre.métier......Marketing. )
baseprimale<- select(baseprimale, -G03Q31.other...Quel.est.votre.métier......Autre. )


## Recodage de la taille de l'organisation (nous recodons les freelances comme acteur travaillant seul) 

baseprimale$G03Q43<-baseprimale$G03Q43..Combien.de.salariés.environ.travaillent.dans.votre.entreprise............
baseprimale$G03Q43<-case_when(baseprimale$G03Q38..Quel.est.votre.statut..== "Freelance" ~ "0" , T ~ as.character(baseprimale$G03Q43))
baseprimale$G03Q43<- as.integer(baseprimale$G03Q43)

baseprimale$tailleorga<-case_when(baseprimale$G03Q43==-0 ~ "Acteur unique", 
                                  baseprimale$G03Q43<=10 ~"Organisation de moins de 10 acteurs",
                                  baseprimale$G03Q43>10 &
                                    baseprimale$G03Q43<=100 ~ "Organisation de moins de 100 acteurs", 
                                  baseprimale$G03Q43>100 & 
                                    baseprimale$G03Q43<=500 ~ "Organisation de moins de 500 acteurs",
                                  baseprimale$G03Q43>500 &
                                    baseprimale$G03Q43<100000 ~ "Organisation de 500 acteurs et plus",
                                  T ~ "")

## Recodage de la variable période du crunch 

baseprimale$période_crunch<-baseprimale$G04Q91.SQ001...A.quel.moment.du.développement.d.un.jeu.êtes.vous.le.plus.susceptible.de.rentrer.en.Crunch....En.Pré.Production.

baseprimale$période_crunch = case_when(baseprimale$G04Q91.SQ001...A.quel.moment.du.développement.d.un.jeu.êtes.vous.le.plus.susceptible.de.rentrer.en.Crunch....En.Pré.Production.== "Oui"  ~ "En Pré-production",
                                       baseprimale$G04Q91.SQ002...A.quel.moment.du.développement.d.un.jeu.êtes.vous.le.plus.susceptible.de.rentrer.en.Crunch....Au.début.de.la.production.== "Oui"  ~ "Au début de production",
                                       baseprimale$G04Q91.SQ003...A.quel.moment.du.développement.d.un.jeu.êtes.vous.le.plus.susceptible.de.rentrer.en.Crunch....Au.milieu.de.la.production.== "Oui"  ~ "Au milieu de la production",
                                       baseprimale$G04Q91.SQ004...A.quel.moment.du.développement.d.un.jeu.êtes.vous.le.plus.susceptible.de.rentrer.en.Crunch....A.la.fin.de.la.production.== "Oui"  ~ "A la fin de la production")


###Suppression des colonnes devenues inutiles 
baseprimale<- select(baseprimale, -G04Q91.SQ001...A.quel.moment.du.développement.d.un.jeu.êtes.vous.le.plus.susceptible.de.rentrer.en.Crunch....En.Pré.Production.)
baseprimale<- select(baseprimale, -G04Q91.SQ002...A.quel.moment.du.développement.d.un.jeu.êtes.vous.le.plus.susceptible.de.rentrer.en.Crunch....Au.début.de.la.production.)
baseprimale<- select(baseprimale, -G04Q91.SQ003...A.quel.moment.du.développement.d.un.jeu.êtes.vous.le.plus.susceptible.de.rentrer.en.Crunch....Au.milieu.de.la.production.)
baseprimale<- select(baseprimale, -G04Q91.SQ004...A.quel.moment.du.développement.d.un.jeu.êtes.vous.le.plus.susceptible.de.rentrer.en.Crunch....A.la.fin.de.la.production.)


## Recodage de la manière de travailler des programmeurs  
baseprimale$work_dev <-baseprimale$G04Q54.SQ001...Dans.vos.bureaux.travaillez.vous........Seul.devant.votre.poste.de.travail.

baseprimale$work_dev = case_when( baseprimale$G04Q54.SQ001...Dans.vos.bureaux.travaillez.vous........Seul.devant.votre.poste.de.travail.== "Oui"  ~ "One programming",
                                  baseprimale$G04Q54.SQ002...Dans.vos.bureaux.travaillez.vous........A.deux.devant.votre.poste.de.travail.== "Oui"  ~ "Pair programming",
                                  baseprimale$G04Q54.other...Dans.vos.bureaux.travaillez.vous........Autre.== "Oui"  ~ "autre")

### Suppression des colonnes devenues inutiles 
baseprimale<- select(baseprimale, -G04Q54.SQ001...Dans.vos.bureaux.travaillez.vous........Seul.devant.votre.poste.de.travail.)
baseprimale<- select(baseprimale, -G04Q54.SQ002...Dans.vos.bureaux.travaillez.vous........A.deux.devant.votre.poste.de.travail.)
baseprimale<- select(baseprimale, -G04Q54.other...Dans.vos.bureaux.travaillez.vous........Autre.)


# Recodage de ce qui importe le plus dans le travail pour les professionnels du milieu artistique
baseprimale$travail_art<-baseprimale$G04Q49.SQ001...Qu.est.ce.qui.vous.préoccupe.le.plus.dans.votre.travail.........la.portée.esthétique.de.votre.travail.

baseprimale$travail_art = case_when( baseprimale$G04Q49.SQ001...Qu.est.ce.qui.vous.préoccupe.le.plus.dans.votre.travail.........la.portée.esthétique.de.votre.travail.== "Oui"  ~ "Portée esthétique",
                                     baseprimale$G04Q49.SQ002...Qu.est.ce.qui.vous.préoccupe.le.plus.dans.votre.travail.........le.fait.d.avoir.créé.quelque.chose.de.fonctionnel. == "Oui"  ~ "Portée fonctionnelle",
                                     baseprimale$G04Q49.SQ003...Qu.est.ce.qui.vous.préoccupe.le.plus.dans.votre.travail.........la.manière.dont.il.impacte.votre.carrière.  == "Oui"  ~ "Impact sur la carrière",
                                     baseprimale$G04Q49.SQ004...Qu.est.ce.qui.vous.préoccupe.le.plus.dans.votre.travail.........la.démonstration.de.son.savoir.faire.  == "Oui"  ~ "Démonstration savoir faire",
                                     baseprimale$G04Q49.other...Qu.est.ce.qui.vous.préoccupe.le.plus.dans.votre.travail.........Autre. =="oui" ~ "Autre")

### Suppression des colonnes devenues inutiles 
baseprimale<- select(baseprimale, -G04Q49.SQ001...Qu.est.ce.qui.vous.préoccupe.le.plus.dans.votre.travail.........la.portée.esthétique.de.votre.travail.)
baseprimale<- select(baseprimale, -G04Q49.SQ002...Qu.est.ce.qui.vous.préoccupe.le.plus.dans.votre.travail.........le.fait.d.avoir.créé.quelque.chose.de.fonctionnel.)
baseprimale<- select(baseprimale, -G04Q49.SQ003...Qu.est.ce.qui.vous.préoccupe.le.plus.dans.votre.travail.........la.manière.dont.il.impacte.votre.carrière.)
baseprimale<- select(baseprimale, -G04Q49.SQ004...Qu.est.ce.qui.vous.préoccupe.le.plus.dans.votre.travail.........la.démonstration.de.son.savoir.faire.)
baseprimale<- select(baseprimale, -G04Q49.other...Qu.est.ce.qui.vous.préoccupe.le.plus.dans.votre.travail.........Autre.)


## Recodage Variable se sentir artisan ou artiste, pour les professionnels du milieu artistique
baseprimale$artiste_artisan<-baseprimale$G04Q47.SQ001...Vous.vous.considérez.comme......un.artiste.

baseprimale$artiste_artisan = case_when( baseprimale$G04Q47.SQ001...Vous.vous.considérez.comme......un.artiste.== "Oui"  ~ "Artiste",
                                         baseprimale$G04Q47.SQ002...Vous.vous.considérez.comme......un.artisan..== "Oui"  ~ "Artisan",
                                         baseprimale$G04Q47.SQ003...Vous.vous.considérez.comme......aucun.des.deux.== "Oui"  ~ "Aucun des deux")

### Suppression des colonnes devenues inutiles 
baseprimale<- select(baseprimale, -G04Q47.SQ001...Vous.vous.considérez.comme......un.artiste.)
baseprimale<- select(baseprimale, -G04Q47.SQ002...Vous.vous.considérez.comme......un.artisan..)
baseprimale<- select(baseprimale, -G04Q47.SQ003...Vous.vous.considérez.comme......aucun.des.deux.)


## Création de la variable âge 
baseprimale$G03Q29..Quelle.est.votre.année.de.naissance.. <- as.numeric(baseprimale$G03Q29..Quelle.est.votre.année.de.naissance..)
baseprimale$age <- 2022-baseprimale$G03Q29..Quelle.est.votre.année.de.naissance.. 


### Suppression de la colonne devenue inutile 
baseprimale<- select(baseprimale, -G03Q29..Quelle.est.votre.année.de.naissance..) 


## Création de la variable distinguant domaine programmatique et domaine artistique 

baseprimale$domaine_tech_art<-baseprimale$G04Q45.SQ001...Votre.métier.relève.t.il.du.domaine.....de.l.art..graphisme..son..etc...

baseprimale$domaine_tech_art = case_when( baseprimale$G04Q45.SQ001...Votre.métier.relève.t.il.du.domaine.....de.l.art..graphisme..son..etc...== "Oui"  ~ "Domaine artistique",
                                          baseprimale$G04Q45.SQ002...Votre.métier.relève.t.il.du.domaine.....du.développement..== "Oui"  ~ "Domaine programmatique",
                                          baseprimale$G04Q46.SQ001...Si...les.deux....quel.est.celui.qui.prédomine.....l.art.. == "Oui" ~ "Domaine artistique",
                                          baseprimale$G04Q46.SQ002...Si...les.deux....quel.est.celui.qui.prédomine.....le.développement.. == "Oui" ~ "Domaine programmatique", 
                                          baseprimale$G04Q46.SQ003...Si...les.deux....quel.est.celui.qui.prédomine.....les.deux.à.part.égales. == "Oui" ~ "Domaine équilibré")

### Suppression des colonnes devenues inutiles 
baseprimale<- select(baseprimale, -G04Q45.SQ004...Votre.métier.relève.t.il.du.domaine.....les.deux.) 
baseprimale<- select(baseprimale, -G04Q45.SQ001...Votre.métier.relève.t.il.du.domaine.....de.l.art..graphisme..son..etc...)
baseprimale<- select(baseprimale, -G04Q45.SQ002...Votre.métier.relève.t.il.du.domaine.....du.développement..)
baseprimale<- select(baseprimale, -G04Q46.SQ001...Si...les.deux....quel.est.celui.qui.prédomine.....l.art..)
baseprimale<- select(baseprimale, -G04Q46.SQ002...Si...les.deux....quel.est.celui.qui.prédomine.....le.développement..)
baseprimale<- select(baseprimale, -G04Q46.SQ003...Si...les.deux....quel.est.celui.qui.prédomine.....les.deux.à.part.égales.)


## Recodage des moyens de paiement en cas de crunch 

baseprimale$moyen_pay<-baseprimale$G01Q22.SQ001...Lors.de.votre.dernier.Crunch..vos.heures.de.travail.supplémentaires.étaient.elles.payées.....oui..en.heures.supplémentaires.

baseprimale$moyen_pay = case_when( baseprimale$G01Q22.SQ001...Lors.de.votre.dernier.Crunch..vos.heures.de.travail.supplémentaires.étaient.elles.payées.....oui..en.heures.supplémentaires.== "Oui"  ~ "Heures supplémentaires",
                                   baseprimale$G01Q22.SQ002...Lors.de.votre.dernier.Crunch..vos.heures.de.travail.supplémentaires.étaient.elles.payées.....oui..en.prime.  == "Oui"  ~ "Prime",
                                   baseprimale$G01Q22.SQ003...Lors.de.votre.dernier.Crunch..vos.heures.de.travail.supplémentaires.étaient.elles.payées.....oui..mais.de.manière.partielle.== "Oui"  ~ "Payées partiellement ",
                                   baseprimale$G01Q22.SQ004...Lors.de.votre.dernier.Crunch..vos.heures.de.travail.supplémentaires.étaient.elles.payées.....non.== "Oui"  ~ "Non payées")


### Suppression des colonnes devenues inutiles 
baseprimale<- select(baseprimale, -G01Q22.SQ001...Lors.de.votre.dernier.Crunch..vos.heures.de.travail.supplémentaires.étaient.elles.payées.....oui..en.heures.supplémentaires.)
baseprimale<- select(baseprimale, -G01Q22.SQ002...Lors.de.votre.dernier.Crunch..vos.heures.de.travail.supplémentaires.étaient.elles.payées.....oui..en.prime.)
baseprimale<- select(baseprimale, -G01Q22.SQ003...Lors.de.votre.dernier.Crunch..vos.heures.de.travail.supplémentaires.étaient.elles.payées.....oui..mais.de.manière.partielle.)
baseprimale<- select(baseprimale, -G01Q22.SQ004...Lors.de.votre.dernier.Crunch..vos.heures.de.travail.supplémentaires.étaient.elles.payées.....non.)


## Création de la variable nouvelles relations durant crunch 

baseprimale$new_rel<-baseprimale$G02Q06.SQ002...Durant.un.crunch..avez.vous.forgé.des.liens.avec.de.nouvelles.personnes.....Salarié.d.une.profession.différente.

baseprimale$new_rel = case_when( baseprimale$G02Q06.SQ001...Durant.un.crunch..avez.vous.forgé.des.liens.avec.de.nouvelles.personnes.....Salarié.de.la.profession.== "oui"  ~ "Salarié de la profession",
                                 baseprimale$G02Q06.SQ002...Durant.un.crunch..avez.vous.forgé.des.liens.avec.de.nouvelles.personnes.....Salarié.d.une.profession.différente.  == "oui"  ~ "Salarié profession différente",
                                 baseprimale$G02Q06.SQ003...Durant.un.crunch..avez.vous.forgé.des.liens.avec.de.nouvelles.personnes.....Salarié.de.même.rang.  == "oui"  ~ "Salarié de même rang ",
                                 baseprimale$G02Q06.SQ004...Durant.un.crunch..avez.vous.forgé.des.liens.avec.de.nouvelles.personnes.....Supérieur.hiéarchique..  == "oui"  ~ "Supérieur hiéarchique",
                                 baseprimale$G02Q06.SQ005...Durant.un.crunch..avez.vous.forgé.des.liens.avec.de.nouvelles.personnes.....Professionnels.sur.des.forums. == "oui"  ~ "Professionnels-forums")

### Suppression des colonnes devenues inutiles 
baseprimale<- select(baseprimale, -G02Q06.SQ001...Durant.un.crunch..avez.vous.forgé.des.liens.avec.de.nouvelles.personnes.....Salarié.de.la.profession.)
baseprimale<- select(baseprimale, -G02Q06.SQ002...Durant.un.crunch..avez.vous.forgé.des.liens.avec.de.nouvelles.personnes.....Salarié.d.une.profession.différente.)
baseprimale<- select(baseprimale, -G02Q06.SQ003...Durant.un.crunch..avez.vous.forgé.des.liens.avec.de.nouvelles.personnes.....Salarié.de.même.rang.)
baseprimale<- select(baseprimale, -G02Q06.SQ004...Durant.un.crunch..avez.vous.forgé.des.liens.avec.de.nouvelles.personnes.....Supérieur.hiéarchique..)
baseprimale<- select(baseprimale, -G02Q06.SQ005...Durant.un.crunch..avez.vous.forgé.des.liens.avec.de.nouvelles.personnes.....Professionnels.sur.des.forums.)


## Création de la variable lieu de fête en situation de crunch 
baseprimale$fest_crunch<-baseprimale$G02Q09.SQ001...Durant.un.crunch..avez.vous.partagé.des.instants.festifs.avec.vos.collègues.....Pendant.les.pauses.

baseprimale$fest_crunch = case_when( baseprimale$G02Q09.SQ001...Durant.un.crunch..avez.vous.partagé.des.instants.festifs.avec.vos.collègues.....Pendant.les.pauses. == "oui"  ~ "Pause",
                                     baseprimale$G02Q09.SQ002...Durant.un.crunch..avez.vous.partagé.des.instants.festifs.avec.vos.collègues.....Le.soir.au.bureau.. == "oui"  ~ "Le soir au bureau",
                                     baseprimale$G02Q09.SQ003...Durant.un.crunch..avez.vous.partagé.des.instants.festifs.avec.vos.collègues.....Dans.un.restaurant.. == "oui"  ~ "Restaurant",
                                     baseprimale$G02Q09.SQ004...Durant.un.crunch..avez.vous.partagé.des.instants.festifs.avec.vos.collègues.....Chez.un.collègue.. == "oui"  ~ "Chez collègue")

### Suppressions des colonnes devenues inutiles 
baseprimale<- select(baseprimale, -G02Q09.SQ001...Durant.un.crunch..avez.vous.partagé.des.instants.festifs.avec.vos.collègues.....Pendant.les.pauses.)
baseprimale<- select(baseprimale, -G02Q09.SQ002...Durant.un.crunch..avez.vous.partagé.des.instants.festifs.avec.vos.collègues.....Le.soir.au.bureau..)
baseprimale<- select(baseprimale, -G02Q09.SQ003...Durant.un.crunch..avez.vous.partagé.des.instants.festifs.avec.vos.collègues.....Dans.un.restaurant..)
baseprimale<- select(baseprimale, -G02Q09.SQ004...Durant.un.crunch..avez.vous.partagé.des.instants.festifs.avec.vos.collègues.....Chez.un.collègue..)

#Fin du recodage des questions, ce dernier fut relativement rapide étant donné que 
#nous avons téléchargé les réponses à nos questions sous format de chaîne de caractère
#et non sous format numérique 
#nous évitant ainsi, de nombreuses lignes supplémentaires de recodage 