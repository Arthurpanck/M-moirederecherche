##Création de la variable politique du crunch--------------------------------------------------
##
baseprimale5<-baseprimale

# Suppression des lignes avec des nons réponses à "vous êtes vous renseigné régulièrement sur le crunch" 
# De manière générale les répondants à cette question ont répondu aux autres questions constituant la variable scorée
# De manière générale les non répondants à cette question n'ont pas répondu aux autres questions constituant la variable scorée 
baseprimale5<-baseprimale5[!(rowSums(is.na(baseprimale5[32]))),]

# Renommer les colonnes en rapport avec la variable politique pour prendre moins de place

baseprimale5$renseign<-baseprimale5$G02Q02..En.dehors.de.vos.heures.de.travail..vous.êtes.vous.renseigné.régulièrement.sur.le.Crunch..
baseprimale5$discuss<- baseprimale5$G02Q03..En.période.habituelle.de.travail..le.Crunch.est.il.un.sujet.régulier.de.conversation.dans.mon.entreprise...
baseprimale5$portraitpresse <- baseprimale5$G02Q04..Est.ce.que.vous.reconnaissez.votre.situation.dans.le.portrait.du.Crunch.dépeint.par.les.différents.médias..heures.non.payées..burn.out..surcharge.forcé..direction.toxique..etc...

#Création variable politique du crunch avec un score de 0 initial 
baseprimale5$politique<-0

### Ajout d'un score en fonction du renseignement sur le crunch dans les médias
# Oui+1, Non+0

baseprimale5$renseign[is.na(baseprimale5$renseign)] = "Non"
baseprimale5$politique  = ifelse(
  baseprimale5$renseign== "Oui", 
  baseprimale5$politique  + 1, baseprimale5$politique )
baseprimale5$politique  = ifelse(
  baseprimale5$renseign == "Non", 
  baseprimale5$politique  + 0, baseprimale5$politique )

### Ajout d'un score en fonction du fait d'avoir des discussions à propos du crunch en entreprise 
# Oui +1, Non+0

baseprimale5$discuss[is.na(baseprimale5$discuss)] = "Non"
baseprimale5$politique  = ifelse(
  baseprimale5$discuss== "Oui", 
  baseprimale5$politique  + 1, baseprimale5$politique )
baseprimale5$politique  = ifelse(
  baseprimale5$discuss== "Non", 
  baseprimale5$politique  + 0, baseprimale5$politique )

### Addition d'un score en fonction d'une concordance entre le portait du crunch dépeint par la presse et le sien
# Oui +1, Non+0

baseprimale5$portraitpresse[is.na(baseprimale5$portraitpresse)] = "Non"
baseprimale5$politique  = ifelse(
  baseprimale5$portraitpresse== "Oui", 
  baseprimale5$politique  + 1, baseprimale5$politique )
baseprimale5$politique  = ifelse(
  baseprimale5$portraitpresse== "Non", 
  baseprimale5$politique  + 0, baseprimale5$politique )

##
## Fin de création de la variable politique --------------------------------------------------

## --------------------------------------------------------------------------
### Tableau 3.2.6 - Le crunch comme objet politique en fonction du statut des acteurs ----

#Création d'une table 
basepolitique<-baseprimale5

#Changement du nom des modalités pour faciliter la lecture 
basepolitique$statut<-fct_collapse(basepolitique$statut, "Indépendant"="Auto-Entrepreneur (Personne Publiant son propre jeu)", "CDD"="Salarié CDD", "CDI"="Salarié CDI")

#Mise en page du tableau 

champ16<-"Lecture : 50% des salariés en CDD ont un degré de politisation vis à vis du crunch de 1"

cprop(table (basepolitique$politique,basepolitique$statut ))%>%
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrr", digits = c(0,0,1,1),
        caption = "Le crunch comme objet politique en fonction du statut des acteurs",
        label = "tableau1", position = "ht!") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%    
  footnote(general = c(champ16),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "5cm") %>% 
  column_spec(4, width = "3.5cm") 

## Fin du tableau 3.2.6 --------------------------------------------------
###------------------------------------------


## --------------------------------------------------------------------------
### Tableau 3.2.7 - Réponse à la question : La situation de crunch est elle qualifiée explicitement comme du crunch ? ----

#Mise en page du tableau (simple croisement de deux variables, modifié avec paint)

champ17<-"Lecture : Sur un total de 33 acteurs, 11 déclarent qu'une situation de surcharge de travail prolongée n'est qualifiée comme crunch ni par les équipes ni par la direction"
table(baseprimale$G04Q100..La.situation.de.Crunch.est.elle.qualifiée.explicitement.comme.du.Crunch...)%>%
  kable(format = "html", linesep = "", booktable=TRUE, caption = "Réponse à la question : La situation de crunch est elle qualifiée explicitement comme du crunch ? ", label="pages")%>%
  kable_styling(latex_options = c("striped", "HOLD_position", "repeat header"), repeat_header_text = "\\textit{(suite)})")%>%
  footnote(general = c(champ17),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "")


## Fin du tableau 3.2.7
###---------------------------------------------------


##Création de la variable de normalité du crunch --------------------------------------------------
#(pour pouvoir l'appliquer au politique)
##

#Renommer les variables de la normalité du crunch pour qu'elles prennent moins de place  

baseprimale5$pratiq_cons<- baseprimale5$G01Q12..La.pratique.du.crunch.en.entreprise.est.elle.une.pratique.normale.selon.vous..
baseprimale5$pratiq_remi <- baseprimale5$G01Q13..Dans.l.entreprise.personne.ou.peu.de.personnes.remettent.en.cause.cette.pratique..
baseprimale5$pos_diff<- baseprimale5$G02Q10..Durant.un.crunch..estimez.vous.qu.il.soit.difficile.d.adopter.une.position.en.désaccord.avec.le.groupe..
baseprimale5$qual_sit<- baseprimale5$G04Q100..La.situation.de.Crunch.est.elle.qualifiée.explicitement.comme.du.Crunch...

#Création de la variable normalité du crunch avec un score de 0 
baseprimale5$normalité<-0

### Ajout d'un score en fonction de la considération de la pratique du crunch
# elle est nécessaire+1, elle est habituelle+1, Non+0

baseprimale5$pratiq_cons[is.na(baseprimale5$pratiq_cons)] = "Non"
baseprimale5$normalité  = ifelse(
  baseprimale5$pratiq_cons== "Non", 
  baseprimale5$normalité  + 0, baseprimale5$normalité )
baseprimale5$normalité  = ifelse(
  baseprimale5$pratiq_cons== "Elle est habituelle", 
  baseprimale5$normalité  + 1, baseprimale5$normalité )
baseprimale5$normalité  = ifelse(
  baseprimale5$pratiq_cons== "Elle est nécessaire", 
  baseprimale5$normalité  + 1, baseprimale5$normalité )

### Ajout d'un socre en fonction du degré de remise en cause de la pratique
# La pratique pas remise en cause +10, la pratique remise en cause +5, ils ne la connaissent pas +0

baseprimale5$pratiq_remi[is.na(baseprimale5$pratiq_remi)] = "Ils ne la connaissent pas"
baseprimale5normalité  = ifelse(
  baseprimale5$pratiq_remi== "La pratique est remise en cause", 
  baseprimale5$normalité  + 5, baseprimale5$normalité )
baseprimale5$normalité  = ifelse(
  baseprimale5$pratiq_remi== "La pratique n'est pas remise en cause", 
  baseprimale5$normalité  + 10, baseprimale5$normalité )
baseprimale5$normalité  = ifelse(
  baseprimale5$pratiq_remi== "Ils ne la connaissent pas", 
  baseprimale5$normalité  + 0, baseprimale5$normalité )

### Ajout d'un score en fonction de la possibilité d'être en désaccord avec le groupe vis à vis du crunch 
# Oui+10, Non+5

baseprimale5$pos_diff[is.na( baseprimale5$pos_diff)] = "Non"
baseprimale5$normalité  = ifelse(
  baseprimale5$pos_diff== "Non", 
  baseprimale5$normalité  + 5, baseprimale5$normalité )
baseprimale5$normalité  = ifelse(
  baseprimale5$pos_diff== "Oui", 
  baseprimale5$normalité  + 10, baseprimale5$normalité )

### Ajout d'un score en fonction de la variable qualification de la pratique du crunch dans l'entreprise 
# Oui par les deux +5, oui par la direction+3, oui par les équipes+4, non+0

baseprimale5$qual_sit[is.na(baseprimale5$qual_sit)] = "Non"
baseprimale5$normalité  = ifelse(
  baseprimale5$qual_sit== "Non", 
  baseprimale5$normalité  + 0, baseprimale5$normalité )
baseprimale5$normalité  = ifelse(
  baseprimale5$qual_sit== "Oui par les équipes", 
  baseprimale5$normalité  + 4, baseprimale5$normalité )
baseprimale5$normalité  = ifelse(
  baseprimale5$qual_sit== "Oui par la direction", 
  baseprimale5$normalité  + 3, baseprimale5$normalité )
baseprimale5$normalité  = ifelse(
  baseprimale5$qual_sit== "Oui par les deux", 
  baseprimale5$normalité  + 5, baseprimale5$normalité )

##
## Fin de création de la variable normalité --------------------------------------------------


##--------------------------------------------------------------------------
#Tableau 3.2.8 -La qualification de la situation de crunch par les acteurs en fonction de son degré de normalité----------------

# Création de la table normal_rep 
normal_rep<-select(baseprimale5, normalité, temps_crunch, crunchyn, temps_crunch, nbr_crunch, statut, temps_nocrunch, G04Q100..La.situation.de.Crunch.est.elle.qualifiée.explicitement.comme.du.Crunch... )

# Restriction de l'échantillon aux répondants travaillant en entreprise 
normal_rep<-filter(normal_rep, statut=="Salarié CDD "| statut=="Salarié CDI" | statut=="Stagiaire" )

# Création des tranches de normalité 
normal_rep$normalité<-cut(normal_rep$normalité, c(0,9,15,24), include.lowest= T, right=F)

# Suppression des lignes sans degré de normalité 
normal_rep<-normal_rep[!(rowSums(is.na(normal_rep[1]))),]

# Modification du nom des modalités pour une meilleure lecture 
normal_rep$G04Q100..La.situation.de.Crunch.est.elle.qualifiée.explicitement.comme.du.Crunch...<-fct_collapse(normal_rep$G04Q100..La.situation.de.Crunch.est.elle.qualifiée.explicitement.comme.du.Crunch..., "Oui par les équipes et la direction"="Oui par les deux")

# Mise en forme du tableau 

champ18<-"Lecture : Lorsque la pratique du crunch est considérée comme très normale [15,24], la surcharge de travail prolongée est reconnue comme crunch par les équipes et la direction dans 50% des cas "
rprop(table(normal_rep$normalité, normal_rep$G04Q100..La.situation.de.Crunch.est.elle.qualifiée.explicitement.comme.du.Crunch...))%>%
  kable(booktabs = TRUE, linesep = "", longtable = TRUE, align = "lrrr", digits = c(0,0,1,1),
        caption = "La qualification de la situation de crunch par les acteurs en fonction de son degré de normalité",
        label = "tableau1", position = "ht!") %>% 
  kable_styling(full_width = T, latex_options = c("repeat_header"), repeat_header_text = "\\textit{(suite)}", position = "center", font_size = 9) %>%    
  footnote(general = c(champ18),
           general_title = "",
           footnote_as_chunk = T, escape = F, threeparttable = T, symbol_manual = "") %>% 
  column_spec(1, width = "5cm") %>% 
  column_spec(4, width = "3.5cm") 

## Fin tableau 3.2.8
##-------------------------------
