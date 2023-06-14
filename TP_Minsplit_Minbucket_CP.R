### Création de l'arbre en utilisant un paramètre nsplit=1
library(rpart)
jouer_base <- read.table("C:/Users/ibrah/OneDrive/Bureau/M2GL/Semestre02/IA/Data/arbre.txt", header=TRUE)
fit <- rpart(Jouer ~ Temps + Temperature + Humidite + Venteux,
             method="class", data=jouer_base,
             control=rpart.control(minsplit=14))
post(fit,file="")
printcp(fit)

### Création de l'abre en utilisant comme paramétre nsplit = 5
library(rpart)
jouer_base <- read.table("C:/Users/ibrah/OneDrive/Bureau/M2GL/Semestre02/IA/Data/arbre.txt", header=TRUE)
fit <- rpart(Jouer ~ Temps + Temperature + Humidite + Venteux,
              method = "class", data = jouer_base,
             control=rpart.control(minsplit = 5))
post(fit, file="")
printcp(fit)

#----------------------------------------------------------------
# Le paramètre minsplit est défini à 5, ce qui signifie que le partitionnement
# ne sera entrepris que si un nœud contient au moins 5 observations. Dans notre arbre,
# il y a 14 observations au total, ce qui satisfait la condition minsplit.
# Le paramètre minbucket est calculé automatiquement comme un tiers de minsplit. 
# Dans notre cas, minbucket sera approximativement égal à 5/3, soit environ 1.67. 
# Cela signifie que chaque nœud terminal (feuille) devrait avoir au moins 2 observations, ce qui est vérifié dans notre arbre.
# l'arbre obtenu avec minsplit = 5 respecte les contraintes minsplit et minbucket.

### Création de l'abre en utilisant comme paramétre nsplit = 10
library(rpart)
jouer_base <- read.table("C:/Users/ibrah/OneDrive/Bureau/M2GL/Semestre02/IA/Data/arbre.txt", header=TRUE)
fit <- rpart(Jouer ~ Temps + Temperature + Humidite + Venteux,
             method = "class", data = jouer_base,
             control=rpart.control(minsplit = 10))
#post(fit, file="")
printcp(fit)

### Création de l'abre en utilisant comme paramétre nsplit = 13
library(rpart)
jouer_base <- read.table("C:/Users/ibrah/OneDrive/Bureau/M2GL/Semestre02/IA/Data/arbre.txt", header=TRUE)
fit <- rpart(Jouer ~ Temps + Temperature + Humidite + Venteux,
             method = "class", data = jouer_base,
             control=rpart.control(minsplit = 13))
#post(fit, file="")
printcp(fit)


### Création de l'abre en utilisant comme paramétre nsplit = 14
library(rpart)
jouer_base <- read.table("C:/Users/ibrah/OneDrive/Bureau/M2GL/Semestre02/IA/Data/arbre.txt", header=TRUE)
fit <- rpart(Jouer ~ Temps + Temperature + Humidite + Venteux,
             method = "class", data = jouer_base,
             control=rpart.control(minsplit = 14))
post(fit, file="")
printcp(fit)

# Expliquer les impacts directs et indirects des changements de minsplit, minbucket et/ou cp.
############# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ################
#                       Impact direct~minsplit
# L'augmentation de la valeur de minsplit réduit le nombre de divisions de l'arbre, 
# ce qui conduit à des arbres plus petits et moins complexes. En augmentant minsplit, 
# on favorise la généralisation plutôt que l'adaptation aux données d'entraînement spécifiques.
#                       Impact indirect~minsplit
# Une valeur plus élevée de minsplit peut entraîner une baisse de la précision 
# de prédiction, car des divisions supplémentaires qui pourraient être informatives
# sont évitées. Cela peut entraîner une perte d'informations et de détails 
# importants contenus dans les données.
                    
############# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ################
############# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ################
#                       Impact direct~minbucket
# En augmentant la valeur de minbucket, on spécifie un nombre minimum plus
# élevé d'observations requises dans chaque feuille de l'arbre. 
# Cela conduit à des feuilles contenant un plus grand nombre d'observations.
#                       Impact indirect~minbucket
# Une valeur plus élevée de minbucket peut simplifier l'arbre en réduisant
# le nombre de feuilles, ce qui peut entraîner une perte de précision de prédiction.
# Des feuilles avec moins d'observations peuvent également conduire à
# une généralisation excessive et à une perte d'informations spécifiques
# aux sous-groupes de données.

############# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ################
############# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ################
#                       Impact direct~cp
# En augmentant la valeur de cp, on spécifie un seuil de complexité plus 
# élevé, ce qui limite la taille de l'arbre. Cela permet de favoriser 
# la simplicité et d'éviter le surajustement aux données d'entraînement.
#                       Impact indirect~cp
# Une valeur plus élevée de cp peut entraîner une perte de précision de
# prédiction, car des subdivisions supplémentaires qui pourraient améliorer
# la performance prédictive sont ignorées.

############# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ################
  