### Création de l'arbre en utilisant un paramètre nsplit=1
library(rpart)
library(rpart.plot)
jouer_base <- read.table("C:/Users/ibrah/OneDrive/Bureau/M2GL/Semestre02/IA/Data/arbre.txt", header=TRUE)
fit <- rpart(Jouer ~ Temps + Temperature + Humidite + Venteux,
             method="class", data=jouer_base,
             control=rpart.control(minsplit=1))
post(fit,file="")
printcp(fit)

pruned_tree <- prune(fit, cp = 0.2)
print(pruned_tree)
rpart.plot(pruned_tree)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Détermination de l'arbre qui minimise la complexité de coût Rα(T) pour α = 0,20
# utilisation de la fonction prune() en spécifiant le paramètre de complexité cp égal à la valeur qui correspond à α = 0,20
alpha <- 0.20
pruned_tree <- prune(fit, cp = fit$cptable[which.min(fit$cptable[, "xerror"]), "CP"], k = alpha)

# Affichage de l'arbre pruné
print(pruned_tree)
rpart.plot(pruned_tree)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
                          # Deuxième méthode
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(rpart)
# Chargement des données
jouer_base <- read.table("C:/Users/ibrah/OneDrive/Bureau/M2GL/Semestre02/IA/Data/arbre.txt", header=TRUE)

# Création de l'arbre initial
fit <- rpart(Jouer ~ Temps + Temperature + Humidite + Venteux,
             method="class",
             data=jouer_base,
             control=rpart.control(minsplit=1))

# Recherche du seuil de complexité pour α = 0,20
alpha <- 0.20
se_values <- fit$cptable[, "xstd"]
min_se <- min(se_values)
max_alpha_se <- min_se + (1 - alpha) * (max(se_values) - min_se)
cp_value <- fit$cptable[fit$cptable[, "xstd"] <= max_alpha_se, "CP"]
min_cp <- min(cp_value)

# Sélection de l'arbre qui minimise la complexité de coût Rα(T) pour α = 0,20
pruned_tree <- prune(fit, cp = min_cp)

# Affichage de l'arbre élagué
rpart.plot(pruned_tree)

# Dans cet exemple, nous utilisons les valeurs d'écart-type (xstd) 
# de la table des coûts (cptable) au lieu de l'erreur de validation croisée 
# pour déterminer le seuil de complexité approprié. 
# Nous calculons la valeur maximale acceptable d'écart-type (max_alpha_se) 
# en utilisant la méthode 1-SE, où (1 - alpha) est le facteur de réduction 
# et max(se_values) - min(se_values) est l'intervalle total d'écart-type. 
# Ensuite, nous sélectionnons les valeurs de seuil de complexité qui ont un écart-type 
# inférieur ou égal à max_alpha_se, puis nous choisissons le plus petit seuil 
# de complexité (min_cp) parmi ces valeurs.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Utilisons la méthode la plus simple pour déterminer l'arbre qui minimise 
# le cout de complexité Rα(T) pour α = 0,05.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### Création de l'arbre en utilisant un paramètre nsplit=1
library(rpart)
library(rpart.plot)
jouer_base <- read.table("C:/Users/ibrah/OneDrive/Bureau/M2GL/Semestre02/IA/Data/arbre.txt", header=TRUE)
fit <- rpart(Jouer ~ Temps + Temperature + Humidite + Venteux,
             method="class", data=jouer_base,
             control=rpart.control(minsplit=1))
post(fit,file="")
printcp(fit)

pruned_tree <- prune(fit, cp = 0.05)
print(pruned_tree)
rpart.plot(pruned_tree)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Détermination de l'arbre qui minimise la complexité de coût Rα(T) pour α = 0,20
# utilisation de la fonction prune() en spécifiant le paramètre de complexité cp égal à la valeur qui correspond à α = 0,20
alpha <- 0.05
pruned_tree <- prune(fit, cp = fit$cptable[which.min(fit$cptable[, "xerror"]), "CP"], k = alpha)

# Affichage de l'arbre pruné
print(pruned_tree)
rpart.plot(pruned_tree)
