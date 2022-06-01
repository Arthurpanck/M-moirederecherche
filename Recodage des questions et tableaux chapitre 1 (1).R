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

## Recodage partiel de la durée du crunch (changement de nom)
baseprimale$temps_crunch<-baseprimale$G02Q81..Lors.de.votre.dernier.Crunch..par.semaine.il.était.normal.de.travailler...
baseprimale$temps_crunch<-fct_collapse(baseprimale$temps_crunch, "40h"="Moins de 40 heures")                                    

## Recodage de la fonction métier (recoder toutes les professions dans une seule colonne au lieu de 10)
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
## En fonction des réponses chiffrées ouvertes nous créons des catégories de taille d'organisation

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

## Recodage de la variable période du crunch(les périodes de crunch recodage en une seule colonne au lieu de 4) 

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


## Recodage de la manière de travailler des programmeurs (recodage en une seule colonne au lieu de 3)  
baseprimale$work_dev <-baseprimale$G04Q54.SQ001...Dans.vos.bureaux.travaillez.vous........Seul.devant.votre.poste.de.travail.

baseprimale$work_dev = case_when( baseprimale$G04Q54.SQ001...Dans.vos.bureaux.travaillez.vous........Seul.devant.votre.poste.de.travail.== "Oui"  ~ "One programming",
                                  baseprimale$G04Q54.SQ002...Dans.vos.bureaux.travaillez.vous........A.deux.devant.votre.poste.de.travail.== "Oui"  ~ "Pair programming",
                                  baseprimale$G04Q54.other...Dans.vos.bureaux.travaillez.vous........Autre.== "Oui"  ~ "autre")

### Suppression des colonnes devenues inutiles 
baseprimale<- select(baseprimale, -G04Q54.SQ001...Dans.vos.bureaux.travaillez.vous........Seul.devant.votre.poste.de.travail.)
baseprimale<- select(baseprimale, -G04Q54.SQ002...Dans.vos.bureaux.travaillez.vous........A.deux.devant.votre.poste.de.travail.)
baseprimale<- select(baseprimale, -G04Q54.other...Dans.vos.bureaux.travaillez.vous........Autre.)


# Recodage de ce qui importe le plus dans le travail pour les professionnels du milieu artistique (recodage dans une seule colonne au lieu de 5)
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


## Recodage Variable se sentir artisan ou artiste, pour les professionnels du milieu artistique (recodage en une seule colonne au lieu de 3)
baseprimale$artiste_artisan<-baseprimale$G04Q47.SQ001...Vous.vous.considérez.comme......un.artiste.

baseprimale$artiste_artisan = case_when( baseprimale$G04Q47.SQ001...Vous.vous.considérez.comme......un.artiste.== "Oui"  ~ "Artiste",
                                         baseprimale$G04Q47.SQ002...Vous.vous.considérez.comme......un.artisan..== "Oui"  ~ "Artisan",
                                         baseprimale$G04Q47.SQ003...Vous.vous.considérez.comme......aucun.des.deux.== "Oui"  ~ "Aucun des deux")

### Suppression des colonnes devenues inutiles 
baseprimale<- select(baseprimale, -G04Q47.SQ001...Vous.vous.considérez.comme......un.artiste.)
baseprimale<- select(baseprimale, -G04Q47.SQ002...Vous.vous.considérez.comme......un.artisan..)
baseprimale<- select(baseprimale, -G04Q47.SQ003...Vous.vous.considérez.comme......aucun.des.deux.)


## Création de la variable âge (on fait 2022- l'année de naissance)
baseprimale$G03Q29..Quelle.est.votre.année.de.naissance.. <- as.numeric(baseprimale$G03Q29..Quelle.est.votre.année.de.naissance..)
baseprimale$age <- 2022-baseprimale$G03Q29..Quelle.est.votre.année.de.naissance.. 


### Suppression de la colonne devenue inutile 
baseprimale<- select(baseprimale, -G03Q29..Quelle.est.votre.année.de.naissance..) 


## Création de la variable distinguant domaine programmatique et domaine artistique (dans une seule colonne au lieu de 5)
## On attribue un domaine artistique et programmatique en fonction du domaine prédominant lorsque les deux sont présents dans un seul métier 
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


## Recodage des moyens de paiement en cas de crunch (recodage dans une seule colonne)

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


## Création de la variable nouvelles relations durant crunch (recodage dans une seule colonne)

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


## Création de la variable lieu de fête en situation de crunch (recodage dans une seule colonne)
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

#Fin du recodage des questions, cela fut relativement rapide étant donné que 
#nous avons téléchargé les réponses à nos questions sous format de chaîne de caractère
#et non sous format numérique 
#nous évitant ainsi, de nombreuses lignes supplémentaires de recodage 




## --------------------------------------------------------------------------
### Tableau 1.2.1 - Répartition de l'échantillon par rapport au statut du travailleur----

# Création tableau à trois colonnes : fréquence en effectif, fréquence en %, fréquence cumulée en % 

échantillon <- baseprimale$G03Q38..Quel.est.votre.statut.. %>% # dernière page ouverte par l'enquêté
  table(useNA = "ifany") %>% # tri à plat
  as.data.frame %>% 
  select(Freq) %>% 
  mutate(`Fréquence (%)` = Freq / sum(Freq) * 100, # proportion
         `Fréquence cumulée (%)` = cumsum(Freq) / sum(Freq) * 100) # proportion cumulée

# Ajout d'une ligne total 
échantillon[6,] <- c(sum(échantillon$Freq), sum(échantillon$`Fréquence (%)`), 100) # ajout de la somme

## Changement du noms des lignes 
rownames(échantillon) <- c("Auto-Entrepreneur", " Freelance ", " Salarié CDD ","Salarié CDI ", "Stagiaire ", "Total")

échantillon %<>%
  rownames_to_column %>% 
  tibble

## Changement du nom des colonnes
names(échantillon)[1:2] <- c("Statut du travailleur ", "Effectif")

# mise en forme du tableau 

champ <- "Lecture : l'échantillon représente la variété de l'industrie vidéoludique, les entrepreneurs représentent 18,9% des répondants, les freelances 21,1%"

échantillon %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrr", digits = c(0,0,1,1),
        caption = "Répartition de l'échantillon par rapport au statut du travailleur",
        label = "pages", position = "ht!") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "5cm") %>% 
  column_spec(4, width = "3.5cm")

### Fin tableau 1.2.1----------------------------------------------


## --------------------------------------------------------------------------
### Tableau 1.2.2 - Répartition de l'échantillon par rapport au statut du travailleur en entreprise----

# Restriction de l'échantillon à ceux travaillant en entreprise (CDD, CDI, stagiaire)

échantillon2<-filter(baseprimale, statut=="Salarié CDD" | statut=="Salarié CDI"  | statut=="Stagiaire")

# Création tableau à trois colonnes : fréquence en effectif, fréquence en %, fréquence cumulée en % 

échantillon2 <- échantillon2$statut %>% # Les statuts effectifs
  table(useNA = "ifany") %>% # tri à plat
  as.data.frame %>% 
  select(Freq) %>% 
  mutate(`Fréquence (%)` = Freq / sum(Freq) * 100, # proportion
         `Fréquence cumulée (%)` = cumsum(Freq) / sum(Freq) * 100) # proportion cumulée

# Ajout d'une ligne de total 

échantillon2[4,] <- c(sum(échantillon2$Freq), sum(échantillon2$`Fréquence (%)`), 100) # ajout de la somme

### Changement du noms des lignes 
rownames(échantillon2) <- c(" Salarié CDD ","Salarié CDI ", "Stagiaire ", "Total")

###Le nom des lignes devient une colonne "rowname"
échantillon2 %<>%
  rownames_to_column %>% 
  tibble

### Changement du nom des colonnes
names(échantillon2)[1:2] <- c("Statut salarial", "Effectif")


#Mise en forme du tableau 

champ2<-"Lecture : Les salariés en CDI représentent 75,4% des répondants, et les salariés au statut plus précaire 24,6%"

échantillon2 %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrr", digits = c(0,0,1,1),
        caption = "Répartition de l'échantillon par rapport au statut du travailleur en entreprise",
        label = "pages", position = "ht!") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ2),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "5cm") %>% 
  column_spec(4, width = "3.5cm")

### Fin tableau 1.2.2----------------------------------------------


## --------------------------------------------------------------------------
### Tableau 1.2.3 - Représentativité de l'échantillon par rapport à la variable genrée----

#Suppression dans l'échantillon des répondants n'ayant pas répondu à la question déterminant le genre 
échantillon3<-baseprimale[!(rowSums(is.na(baseprimale[5]))),]

# Création tableau à trois colonnes : fréquence en effectif, fréquence en %, fréquence cumulée en % 

échantillon3 <- échantillon3$G03Q30..Quel.est.votre.genre.................. %>% # dernière page ouverte par l'enquêté
  table(useNA = "ifany") %>% # tri à plat
  as.data.frame %>% 
  select(Freq) %>% 
  mutate(`Fréquence (%)` = Freq / sum(Freq) * 100, # proportion
         `Fréquence cumulée (%)` = cumsum(Freq) / sum(Freq) * 100) # proportion cumulée

# Ajout d'une ligne total

échantillon3[4,] <- c(sum(échantillon3$Freq), sum(échantillon3$`Fréquence (%)`), 100) # ajout de la somme

### Changement du noms des lignes 
rownames(échantillon3) <- c("Autre", " Féminin", "Masculin", "Total") #On remet à sa place les éléments

### transformation du nom des lignes en colonne "rowname"
échantillon3 %<>%
  rownames_to_column %>% 
  tibble

### Changement du nom des colonnes
names(échantillon3)[1:2] <- c("Genre", "Effectif")

#Mise en forme du tableau 

champ3 <- "Lecture : 74,4% des répondants à ce questionnaire sont du genre masculin"

échantillon3 %>% 
  slice(c(3,2,1,4)) %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrr", digits = c(0,0,1,1),
        caption = "Représentativité de l'échantillon par rapport à la variable genrée",
        label = "pages", position = "ht!") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ3),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "5cm") %>% 
  column_spec(4, width = "3.5cm")

### Fin tableau 1.2.3----------------------------------------------


## --------------------------------------------------------------------------
### Tableau 1.2.4 - Répartition des réponses au questionnaire selon la dernière page validée----

#Création de la table échantillon 4
échantillon4<-baseprimale

#Suppression de ceux ayant comme lastpage 0 (résultat incohérent dû à limesurvey)
échantillon4$lastpage..Dernière.page<- droplevels.factor(échantillon4$lastpage..Dernière.page, 0)

#Transformation des non réponses sur la colonne en lastpage de valeur 1
échantillon4$lastpage..Dernière.page[is.na(échantillon4$lastpage..Dernière.page)] = "1"

# Création tableau à trois colonnes : fréquence en effectif, fréquence en %, fréquence cumulée en % 
échantillon4<- échantillon4$lastpage..Dernière.page %>% # dernière page ouverte par l'enquêté (encore un souci avec 0, mais devrait disp avec les NA)
  table(useNA = "ifany") %>% # On fait un tri à plat
  as.data.frame %>% 
  select(Freq) %>% 
  mutate(`Fréquence (%)` = Freq / sum(Freq) * 100, # proportion des effectifs 
         `Fréquence cumulée (%)` = cumsum(Freq) / sum(Freq) * 100) # proportion cumulée des effectifs 

# Ajout d'une ligne de total 

échantillon4[6,] <- c(sum(échantillon4$Freq), sum(échantillon4$`Fréquence (%)`), 100) # On ajoute la somme

### Changement du noms des lignes 
rownames(échantillon4) <- c( "Demande de consentement","Socio-démographique", "Artistes et développeurs", "Le crunch en entreprise", "Management et rapport au travail", "Total")

### Nom des lignes dans une colonne nommée "rowname"
échantillon4 %<>%
  rownames_to_column %>% 
  tibble

### Changement du noms des colonnes
names(échantillon4)[1:2] <- c("Dernière page validée", "Effectif")

# Mise en forme du tableau 

champ4 <- "Lecture : 95 répondants ont au moins répondu à une question, 64 répondants ont complété l'entiéreté du questionnaire"

échantillon4 %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrr", digits = c(0,0,1,1),
        caption = "Répartition des réponses au questionnaire selon la dernière page validée",
        label = "tableau1", position = "ht!") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ4),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "5cm") %>% 
  column_spec(4, width = "3.5cm")

### Fin tableau 1.2.4----------------------------------------------


## --------------------------------------------------------------------------
### Tableau 1.2.5 - Part de questionnaires complets en fonction de la pratique du Crunch----

# Création de la table échantillong 5 
échantillon5<-baseprimale

# même manipulation qu'au dessus Suppression des lignes à la valeur aberrante 0, transformation des NA en section 1 du questionnaire 

échantillon5$lastpage..Dernière.page<- droplevels.factor(échantillon5$lastpage..Dernière.page, 0)
échantillon5$lastpage..Dernière.page[is.na(échantillon5$lastpage..Dernière.page)] = "1"

# Création tableau à trois colonnes : fréquence en effectif, fréquence en %, fréquence cumulée en % 

échantillon5<- échantillon5$lastpage..Dernière.page %>% # dernière page ouverte par l'enquêté (encore un souci avec 0, mais devrait disp avec les NA)
  table(useNA = "ifany") %>% # On fait un tri à plat
  as.data.frame %>% 
  select(Freq) %>% 
  mutate(`Fréquence (%)` = Freq / sum(Freq) * 100, # proportion des effectifs 
         `Fréquence cumulée (%)` = cumsum(Freq) / sum(Freq) * 100) # proportion cumulée des effectifs 

# Ajout d'une ligne faisant le total 

échantillon5[6,] <- c(sum(échantillon5$Freq), sum(échantillon5$`Fréquence (%)`), 100) # On ajoute la somme

### Changement du noms des lignes 
rownames(échantillon5) <- c( "Demande de consentement","Socio-démographique", "Artistes et développeurs", "Le crunch en entreprise", "Management et rapport au travail", "Total")

### Nom des lignes dans une colonne appelée "rowname"
échantillon5 %<>%
  rownames_to_column %>% 
  tibble

### Changement du nom des colonnes
names(échantillon5)[1:2] <- c("Dernière page validée", "Effectif")

# Mise en forme du tableau 

champ5 <- "Lecture : 95 répondants ont au moins répondu à une question, 64 répondants ont complété l'entiéreté du questionnaire"

échantillon5 %>% 
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrr", digits = c(0,0,1,1),
        caption = "Répartition des réponses au questionnaire selon la dernière page validée",
        label = "tableau1", position = "ht!") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%
  footnote(general = c(champ5),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "5cm") %>% 
  column_spec(4, width = "3.5cm")

### Fin tableau 1.2.5----------------------------------------------