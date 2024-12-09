install.packages("TSstudio")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("dygraphs")
install.packages("forecast")
install.packages("dplyr")
install.packages("xts")
library(TSstudio)
library(dplyr)
library(dygraphs)
library(xts)
dataset <- read.csv("deaths-and-new-cases-of-hiv.csv")
dataset
head(dataset)
summary(dataset)
str(dataset)

#selectionnons uniquement les donnees du Cameroun
dataset <- dataset %>%filter(Entity =="Cameroon")
dataset<- data.frame(dataset)
dataset
taux_mortalite <- dataset$Deaths...HIV.AIDS...Sex..Both...Age..All.Ages..Number.

dataset <- dataset %>% rename(
  annee = Year,
  taux_mortalite = Deaths...HIV.AIDS...Sex..Both...Age..All.Ages..Number.
)

# selection des elements important de mon dataset

VIH <- dataset %>%
  select(annee,taux_mortalite)%>%
  arrange(annee)
VIH

str(VIH)

#convertissons maintenant en une serie temporelle
point_depart <- c(min(dataset$annee))
frequence <- 1

VIHts <- ts(
  data = VIH$taux_mortalite ,
  start = point_depart,
  frequency = frequence
)

VIHts

ts_plot(VIHts)
dygraph( VIHts,
         main= "taux de mortalite en fonction des annees",
         xlab= "annee",
         ylab="taux_mortalite"

)



library(forecast)

# Applique un modèle de lissage exponentiel
VIHts_smooth <- forecast::ses(VIHts, h = 10) # Prévision sur 5 périodes supplémentaires
plot(VIHts_smooth, main = "Prevision a l'aide du lissage exponentiel", xlab = "annee", ylab = "personne décedé")


#Maintenant utilisons un graphique dynamique pour pouvoir mieux interpreter nos donnees
VIHts_prev <- VIHts_smooth$mean
VIHts_combiné <- cbind(Donnees_Actuelle = as.xts(VIHts), Prevision = as.xts(VIHts_prev))


dygraph(VIHts_combiné,
        main = "Prevision à l'aide du lissage exponentiel",
        xlab = "annee",
        ylab = "personne décedé")
