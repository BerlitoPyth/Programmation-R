#1 Import
df = read.csv("fao.csv", header = TRUE, sep = ";", dec = ",")
#2Combien de pays
nrow(df)

#3 Affiché le jeu de données
head(df)
#résumé des données
summary(df)

#4Dispo alim moyenne
mean(df[, 2])
#5 Nombre habitants dans le monde

sum(df$Population, na.rm=TRUE)

#6 Ecart type import / export viande

sd(df$Import_viande)
sd(df$Export_viande, na.rm=TRUE)

#7Mediane du volume de productio
median(df$Prod_viande, na.rm=TRUE)
#quartile dispo alim
quantile(df$Dispo_alim)

#centiles du volume d'importation (début, fin,, pas)
quantile(df$Import_viande, seq(0,1,0.01))


#Exercice 3 
#1Construire une requête pour extraire les lignes du dataset avec les 5 pays les moins peuplés.
head(df)
rang = order(df$Population)
resultat = head(df[rang,], n=5)
View(resultat)
#2Construire une requête pour extraire les lignes du dataset avec les 5 pays les plus peuplés.
head(df)
rang = order(df$Population)
resultats = tail(df[rang,], n=5)
View(resultats)

#3 Construire une requête pour extraire les lignes du dataset avec les 5 pays qui produisent le plus de viande.
plus_viande = order(df$Prod_viande, decreasing = TRUE)
result = head(df[plus_viande,], n=5)
View(result)

#4 Construire une requête pour extraire les lignes du dataset avec les 5 pays qui importent le plus de viande.
imp = order(df$Import_viande, decreasing = TRUE)
affiche = head(df[imp,], n=5)
View(affiche)


#5En moyenne, le besoin énergétique moyen d’une adulte est de 2300 kcal par jour. Construire une requête pour extraire les lignes du dataset avec les pays qui ont une disponibilité alimentaire supérieure ou égale à 2300 kcal. Combien de pays sont concernés ?
extract = subset(df, Dispo_alim >= 2300)
View(extract)

#6Construire une requête pour extraire les lignes du dataset avec les pays qui ont une disponibilité alimentaire strictement supérieure à 3500 kcal et qui importe un volume de viande supérieure ou égale à 1 000 000 tonnes par an. Combien de pays sont concernés ?

alim = subset(df,Dispo_alim > 3500 & Import_viande > 1000 )
View(alim)

#7Construire une requête pour extraire les lignes du dataset avec la France et la Belgique.
pays = subset(df, Nom == "France" | Nom =="Belgique")
View(pays)
#Même chose autre
pays = subset(df, Nom %in% c("France", "Belgique"))
View(pays)

#Exercice 4 - Modifier le dataframe

#1 Ajouter une colonne nommée part_export qui correspond à la part des exportations de viande par rapport à la production de viande.

df$part_export = df$Export_viande / df$Prod_viande
head(df)

#2La colonne Dispo_alim présente la disponibilité alimentaire par personne. Ajouter une colonne nommée dispo_alim_pays qui correspond à la disponibilité total du pays en Kcal/jour.
df$dispo_alim_pays = df$Dispo_alim * df$Population
head(df)

#3 Exporter le nouveau dataframe dans un fichier csv nommé ExportTp2.csv avec la fonction write.table().

write.table(df, file = "ExportTp2.csv")

#4
dispo_alim_mondiale = sum(df$Dispo_alim_pays, na.rm=TRUE)
dispo_alim_mondiale


#5
dispo_alim_mondiale/2300

#Exercice 5 - Corrélation

#1Représenter graphiquement dans un nuage de points le lien entre Prod_viande et Export_viande. Commenter le lien entre ces deux variables ?

plot(x = df$Prod_viande,
     y = df$Export_viande, 
     main = "Pays : Prod_viande / Export_viande")

#2 Calculer le coefficient de corrélation de cette relation avec la fonction cor().
cor(x = df$Prod_viande,
    y = df$Export_viande)

#3Construire la matrice des corrélations des variables quantitatives avec la fonction cor(). Afficher cette matrice dans une vue et arrondisser les valeurs avec deux décimales uniquements. Commenter la relation la plus forte, la plus faible.
matriceCor = cor(df[ , - 1] , use = "complete.obs")
matriceCor = round(matriceCor , 2)
View(matriceCor)

#4Pour mieux visualiser ces corrélations, nous allons utiliser un package qui ne fait pas parti des packages par défaut. Installer le package corrplot avec la fonction install.packages() sauf s'il est déjà installé.

install.packages("corrplot")

#5Construire une Corrélogramme avec la fonction corrplot()
library(corrplot) 
corrplot(matriceCor, method="circle")
