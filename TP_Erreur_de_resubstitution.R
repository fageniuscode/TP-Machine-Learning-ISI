### Création de l'arbre en utilisant un paramètre nsplit=2
library(rpart)
jouer_base <- read.table("C:/Users/ibrah/OneDrive/Bureau/M2GL/Semestre02/IA/Data/arbre.txt", header=TRUE)
fit <- rpart(Jouer ~ Temps + Temperature + Humidite + Venteux,
             method="class", data=jouer_base,
             control=rpart.control(minsplit=2))
post(fit,file="")

### Identification des noeuds sur lesquels la fonction de perte doit être appliquée

printcp(fit)

# Le nœud racine (node 0) a une erreur de 0.35714, ce qui signifie qu'il y a 5 erreurs de classification sur un total de 14 observations.
# Le nœud 0 est le point de départ avec une erreur relative de 1.0 et une erreur de 
# validation croisée (xerror) de 1.0. Cela signifie que l'arbre initial a une erreur de 35.714% 
# sur les données d'entraînement et de 35.714% sur les données de validation croisée.
# Au nœud 2, la variable "Venteux" est utilisée comme critère de division supplémentaire. 
# Cela réduit l'erreur relative à 0.4 et l'erreur de validation croisée à 1.2.
# Au nœud 6, la variable "Temps" est utilisée comme critère de division supplémentaire. 
# Cela réduit l'erreur relative à 0.0 et augmente légèrement l'erreur de validation croisée à 1.4.
# les nœuds 0, 2 et 6 comme des candidats où la fonction de perte peut être appliquée pour améliorer la précision du modèle.

# Combien de fois doit on appliquer la fonction de perte
# La première application de la fonction de perte se fait au nœud 0, 
# qui est le nœud racine de l'arbre. La valeur d'erreur relative au nœud 0 est de 1.0, 
# et la fonction de perte est calculée pour évaluer la qualité de la division à partir du nœud racine.
# La dernière application de la fonction de perte est effectuée au nœud 6, 
# où la variable "Temps" est utilisée comme critère de division. 
# La fonction de perte est appliquée pour évaluer si cette division améliore 
# les performances du modèle par rapport à l'état précédent (nœud 2).


# Calculer l'erreur en resubstitution de cet arbre.
# Prédictions de l'arbre sur les données d'entraînement
predictions <- predict(fit, jouer_base, type="class")

# Calcul de l'erreur en resubstitution
erreur_resubstitution <- sum(predictions != jouer_base$Jouer) / nrow(jouer_base)

# Affichage de l'erreur en resubstitution
cat("Erreur en resubstitution:", erreur_resubstitution, "\n")

### Comment obtenir l'erreur en resubstitution sans d'etailler l'application de la fonction de perte.

### Le realerror de cet l'arbre qu'on a généré est de 0.4, dire comment elle est obtenue ?
# Définition du nombre de folds pour la validation croisée
k <- 5

# Création des indices des folds
set.seed(42)  # Fixer la graine aléatoire pour la reproductibilité
folds <- cut(seq(1, nrow(jouer_base)), breaks=k, labels=FALSE)

# Vecteur pour stocker les erreurs de chaque fold
errors <- numeric(k)

# Boucle de validation croisée
for (i in 1:k) {
  # Sélection des données de test et d'entraînement pour ce fold
  test_indices <- which(folds == i)
  train_data <- jouer_base[-test_indices, ]
  test_data <- jouer_base[test_indices, ]
  
  # Construction de l'arbre sur les données d'entraînement
  fit <- rpart(Jouer ~ Temps + Temperature + Humidite + Venteux,
               method="class", data=train_data,
               control=rpart.control(minsplit=2))
  
  # Prédictions de l'arbre sur les données de test
  predictions <- predict(fit, test_data, type="class")
  
  # Calcul de l'erreur de ce fold
  errors[i] <- sum(predictions != test_data$Jouer) / nrow(test_data)
}

# Calcul de l'erreur réelle moyenne
real_error <- mean(errors)

# Affichage de l'erreur réelle moyenne
cat("Erreur réelle moyenne:", real_error, "\n")

