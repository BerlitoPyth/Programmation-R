# IUT SD1 - TP1 : Manipulation de Dataframes en R

# EXERCICE 1 - Utilisation d'un dataframe existant (iris)

# 1. Afficher le jeu de données iris et sa classe
iris
class(iris)

# 2. Afficher le jeu de données dans une vue
View(iris)

# 3. Afficher le nombre de lignes
nrow(iris)

# 4. Afficher le nombre de colonnes
ncol(iris)

# 5. Afficher le nom des colonnes
colnames(iris)

# 6. Afficher un résumé du dataframe
summary(iris)

# 7. Afficher uniquement les colonnes Sepal.Length et Species
iris[ , c("Sepal.Length", "Species")]

# 8. Afficher uniquement les lignes 100, 103 et 105
iris[c(100, 103, 105), ]

# 9. Afficher uniquement les lignes 50 à 100
iris[50:100, ]

# 10. Calculer la moyenne de la variable Sepal.Length
mean(iris$Sepal.Length)

# 11. Calculer la médiane de la variable Sepal.Width
median(iris$Sepal.Width)

# 12. Calculer l'écart-type de la variable Petal.Length
sd(iris$Petal.Length)

# 13. Calculer les déciles de la variable Petal.Width
quantile(iris$Petal.Width, probs = seq(from = 0.1, to = 0.9, by = 0.1))

# EXERCICE 2 - Import/Export d'un dataframe

# La fonction read.csv()

# 1. Importer les jeux de données manga.csv et anime.csv 
dfManga = read.csv("C:/dev/R/TP1/manga.csv", header = TRUE, dec= ".")
dfAnime = read.csv("C:/dev/R/TP1/anime.csv", header = TRUE, dec= ".")
class(dfManga)
class(dfAnime)

# 2. Afficher les jeux de données pour les visualiser
View(dfManga)


# 3.Afficher le nombre de lignes et colonnes avec la fonction dim()
dim(dfManga)
dim(dfAnime)


# 4. Calculer la moyenne de la variable Score pour les deux dataframe. Quelle est la moyenne la plus élevée ?

mean(dfManga$Score)
mean(dfAnime$Score)

# 5.Calculer le nombre total de votes de la variable Vote pour les deux dataframe. Ou y a t-il le plus de votes ?

sum(dfManga$Vote)
sum(dfAnime$Vote)

# 6.Calculer l'écart-type des notes de la variable Score pour les deux dataframe. Ou est l'échantillon le plus homogènes au niveau des Score ?
sd(dfManga$Score)
sd(dfAnime$Score)

# 7.Calculer les déciles des notes de la variable Score pour les deux dataframe. Quelle dataframe a le décile 1 le plus petit ?

quantile(dfManga$Score, probs = seq(from = 0.1, to = 0.9, by = 0.1 ) )
quantile(dfAnime$Score, probs = seq(from = 0.1, to = 0.9, by = 0.1 ) )


#Les fonctions subset(), table() et prop.table()
#Pour chaque extraction, créer un nouvel objet et calculer le nombre de ligne filtrées.

# 1.Filtre sur les Mangas 
#Combien de Manga ont une note strictement supérieure à 9/10 ?

extraction1 = subset(dfManga, Score > 9)
nrow(extraction1)

# 2.Combien de Manga ont 200000 votes ou plus ?
extraction2 = subset(dfManga, Vote >= 200000)
nrow(extraction2)
# 3.Combien de Manga ont strictement plus de 200000 votes et plus de 8/10 ?

extraction3 = subset(dfManga, Vote >= 200000 & Score > 8)
nrow(extraction3)

# 4.Combien de Manga ont une note comprise entre 7/10 et 8/10 ?
extraction4 <- subset(dfManga, Score >= 7 & Score <= 8)
nrow(extraction4)

# 1. Calculer les effectifs de la variable Rating
effectifRating <- table(dfAnime$Rating)
print(effectifRating)
length(effectifRating)
prop.table(effectifRating)

# 2. Combien d'Anime sont concernés par le Rating : R - 17+ (violence & profanity) ?
extraction2 <- subset(dfAnime, Rating == "R - 17+ (violence & profanity)")
nrow(extraction2)

# 3. Combien d'Anime sont concernés par ce Rating et ont une note supérieure à 8/10 ?
extraction3 <- subset(dfAnime, Rating == "R - 17+ (violence & profanity)" & Score >= 8)
nrow(extraction3)

# 4. Combien d'Anime ne correspondent pas au Rating : R - 17+ (violence & profanity) ?
extraction4 <- subset(dfAnime, Rating != "R - 17+ (violence & profanity)")
nrow(extraction4)

# 5. Combien d'Anime correspondent au Rating : PG - Children et G - All Ages ?
extraction5 <- subset(dfAnime, Rating %in% c("PG - Children", "G - All Ages"))
nrow(extraction5)

# 6. Combien d'Anime ne correspondent pas au Rating : PG - Children et G - All Ages ?
extraction6 <- subset(dfAnime, !Rating %in% c("PG - Children", "G - All Ages"))
nrow(extraction6)

# 7. Combien d'Anime ont une note supérieure à 9/10 ou ont plus de 400000 votes ?
extraction7 <- subset(dfAnime, Score >= 9 | Vote > 400000)
nrow(extraction7)


 Les fonctions rbind() et write.table()

# 1. Modifier les deux dataframe en ne conservant que certaines variables
dfAnime <- dfAnime[ , c("Title", "Score", "Vote", "Ranked")]
dfManga <- dfManga[ , c("Title", "Score", "Vote", "Ranked")]

# 2. Créer une colonne Type avec la valeur Anime ou Manga
dfAnime$Type <- "Anime"
dfManga$Type <- "Manga"

# 3. Compiler les deux dataframe avec rbind()
dfConcat <- rbind(dfManga, dfAnime)
View(dfConcat)

# 4. Exporter le dataframe dans un fichier CSV
write.table(x = dfConcat, file = "C:/dev/R/TP1/ExportTp1.csv", sep = ";", row.names = FALSE)


