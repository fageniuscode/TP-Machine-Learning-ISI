sample1.df <- read.table("C:/Users/ibrah/OneDrive/Bureau/M2GL/Semestre02/IA/Data/cv_sample1.df.txt",
                         header=TRUE, sep=",", na.strings = "NA", dec = ".", strip.white = TRUE)
sample1.df

# Chargement de la bibliothèque DAAG
library(DAAG)

# Validation croisée à l'aide de la fonction CVlm
# Les données utilisées sont sample1.df
# m = 10 indique que la validation croisée est effectuée avec 10 plis
# form.lm définit le modèle de régression avec la formule y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7
val1.daag <- CVlm(data = sample1.df, m = 10, form.lm = formula (y ~x1 + x2 + x3 + x4 + x5 + x6 + x7))

#------------------------------------------------------------------------------------

# Extraction de la 3ème colonne de TSample8.df et création de la matrice y8
y8<-as.matrix(TSample8.df[,3])

# Création d'un vecteur vc rempli de 1, ayant une longueur de 90
vc<-rep(1,90)

# Création d'une séquence allant de 1 au nombre de lignes de TSample8.df
# et assignation à la variable l
l<-seq(1,nrow(TSample8.df))

# Affichage de la séquence l
l

# Création d'une séquence allant de 4 au nombre de colonnes
# de TSample8.df et assignation à la variable c
c<-seq(4,ncol(TSample8.df))

# Affichage de la séquence c
c

# Extraction des lignes de TSample8.df correspondant aux indices de la séquence l 
# et des colonnes correspondant aux indices de la séquence c
TSample8.df[l,c]

# Création d'une nouvelle matrice x.df en combinant
# le vecteur vc et les valeurs extraites de TSample8.df
x.df<-cbind(vc, TSample8.df[l,c])

# Conversion de la matrice x.df en une matrice x
x<-as.matrix(x.df)

# Affichage de la matrice x
x

# Calcul de l'inverse de la matrice (x^T * x)
stxx<-solve(t(x)%*%x)

# Calcul du produit transposé de x multiplié par y8
txy<-t(x)%*%y8

# Calcul du produit de stxx par txy pour obtenir le vecteur B8
B8<-stxx%*%txy

B8
#A comparer avec M8.lm

# -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
#   On peut voir que les résultats des deux méthodes sont très similaires. 
#   Les coefficients estimés pour les variables x1, x2, x3, x4, x5, x6 et x7 
#   sont presque identiques. De plus, le coefficient d'interception (Intercept) 
#   est également le même (-2.788877). Cela indique que les deux approches 
#   de modélisation linéaire ont produit des résultats cohérents.

