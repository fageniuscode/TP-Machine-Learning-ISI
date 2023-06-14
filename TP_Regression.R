sample1.df <- read.table("C:/Users/ibrah/OneDrive/Bureau/M2GL/Semestre02/IA/Data/cv_sample1.df.txt",
                         header=TRUE, sep=",", na.strings = "NA", dec = ".", strip.white = TRUE)
sample1.df

library(DAAG)
val1.daag <- CVlm(data = sample1.df, m = 10,
                  form.lm = formula (y ~x1 + x2 + x3 + x4 + x5 + x6, + x7))

IndicesT8 <- c(7, 25, 26, 43, 67, 68, 76, 84, 85, 97)
IndicesT8

TSample8.df <- sample1.df[-IndicesT8, ]
TSample8.df

M8.lm <- lm(y ~x1 + x2 + x3 +x4 + x5 + x6 + x7,
            data = TSample8.df)
# A comparer avec B8
M8.lm

y8<-as.matrix(TSample8.df[,3])
vc<-rep(1,90)
l<-seq(1,nrow(TSample8.df))
l
c<-seq(4,ncol(TSample8.df))
c
TSample8.df[l,c]
x.df<-cbind(vc, TSample8.df[l,c])
x<-as.matrix(x.df)
x
stxx<-solve(t(x)%*%x)
txy<-t(x)%*%y8
B8<-stxx%*%txy
#A comparer avec M8.lm

#--- Les deux résultats sont les mêmes:
#---- Pour le premier ------ #
# Il crée le vecteur IndicesT8 contenant les indices des observations à exclure de sample1.df.
# Il crée un nouvel objet de données TSample8.df en excluant les observations ayant
# les indices spécifiés dans IndicesT8 à partir de sample1.df.
# Il affiche le modèle linéaire multiple M8.lm.

#---- Pour le premier ------ #
# Il crée une matrice y8 en extrayant la troisième colonne de TSample8.df.
# Il crée un vecteur vc de 90 éléments, où chaque élément est égal à 1.
# Il crée un vecteur l contenant une séquence de nombres allant de 1 à nrow(TSample8.df).
# Il crée un vecteur c contenant une séquence de nombres allant de 4 à ncol(TSample8.df).
# Il extrait une sous-matrice de TSample8.df en utilisant les indices spécifiés dans les vecteurs l et c.
# Il combine le vecteur vc et la sous-matrice extraite pour former une nouvelle matrice x.df.
# Il convertit la matrice x.df en une matrice x.
# Il calcule l'inverse de la matrice transposée de x multipliée par x et stocke le résultat dans stxx.
# Il calcule le produit de la transposée de x par y8 et stocke le résultat dans txy.
# Il calcule le produit de stxx par txy et stocke le résultat dans B8.

