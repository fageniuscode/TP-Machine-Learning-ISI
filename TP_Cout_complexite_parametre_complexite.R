### Création de l'arbre en utilisant un paramètre nsplit=2
library(rpart)
jouer_base <- read.table("C:/Users/ibrah/OneDrive/Bureau/M2GL/Semestre02/IA/Data/arbre.txt", header=TRUE)
fit <- rpart(Jouer ~ Temps + Temperature + Humidite + Venteux,
             method="class", data=jouer_base,
             control=rpart.control(minsplit=1))
post(fit,file="")

### calculons le coefficient de corrélation Rα(T) de l'arbre de décision avec nsplit=2
# Charger les données
jouer_base <- read.table("C:/Users/ibrah/OneDrive/Bureau/M2GL/Semestre02/IA/Data/arbre.txt", header=TRUE)
# Construire l'arbre avec nsplit=2
fit <- rpart(Jouer ~ Temps + Temperature + Humidite + Venteux,
             method="class", data=jouer_base,
             control=rpart.control(minsplit=2))
# Effectuer les prédictions sur les données d'entraînement
predictions <- predict(fit, jouer_base, type="class")
# Calculer l'erreur de resubstitution Rα(T)
resubstitution_error <- sum(predictions != jouer_base$Jouer) / nrow(jouer_base)
# Afficher l'erreur de resubstitution
print(resubstitution_error)

### calculons le coefficient de corrélation Rα(T) de l'arbre de décision avec nsplit=4
# Charger les données
jouer_base <- read.table("C:/Users/ibrah/OneDrive/Bureau/M2GL/Semestre02/IA/Data/arbre.txt", header=TRUE)
# Construire l'arbre avec nsplit=2
fit <- rpart(Jouer ~ Temps + Temperature + Humidite + Venteux,
             method="class", data=jouer_base,
             control=rpart.control(minsplit=4))
# Effectuer les prédictions sur les données d'entraînement
predictions <- predict(fit, jouer_base, type="class")
# Calculer l'erreur de resubstitution Rα(T)
resubstitution_error <- sum(predictions != jouer_base$Jouer) / nrow(jouer_base)
# Afficher l'erreur de resubstitution
print(resubstitution_error)
