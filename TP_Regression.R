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
# A compiler a B8
M8.lm

Y8<-as.matrix(TSample8.df[,3])
vc<-rep(1,90)
l-seq(l,nrow(TSample8.df))
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





