###########Devoir manipulation###########
###########Preparer par Belande D, John  Pascal Be172850


######chargement des packages

library(readxl)
library(httr)
library(tidyverse)
library(lubridate)

########importation des données

lienurl<-"https://www.brh.ht/wp-content/uploads/agregatsmon.xls"
GET(lienurl,
    write_disk(tf<-tempfile(fileext="agregatsmon.xls")))
    agregat<-read_excel(tf,1L)
    agregat

########Manipulation du data frame    
agregat<-data.frame(agregat)

colnames(agregat)<-agregat[2,]  #####utilisation d'une ligne comme nom des autres lignes
objet90<-agregat[147:530,]      ####objbet ayant toutes les lignes a partir d'octobre 1990

obj<-objet90[,-c(5,9,11,15,20,23,25,30,35,40,42,47,56,79,84)]   ######supresion des colones supperflus


#######renomons les collones du data frame
obj<-obj%>%rename(M3d='M3...8',
                  tdc='TAUX DE CHANGE BRH',
                  reserve='RESERVE NETTES DE CHANGE BRH AVEC  DEPOTS AVEC BCMS(MILLIONS DE $)')

##########Estimation du taux de change (regression lineaire) en fonction de M3 et des reserves de change
tdc_estime<-lm(tdc~M3+reserve,data=obj)


##########afficher les resultats de l'estimation et de la statistique de student
summary(tdc_estime)$coef
