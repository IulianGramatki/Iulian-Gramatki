rm(list=ls())
setwd("C:/Users/Iulian/Documents/GitHub/Iulian-Gramatki/coursework/Titanic")
library(dplyr)
library(nnet)
library(ade4)
library(stringr)

dummies <- function (df) #change factor variables into dummies
#works on data frames with factor variables only (can be one or more variables)
{
  acm.util.df <- function(i) {
    cl <- df[, i]
    cha <- names(df)[i]
    n <- length(cl)
    cl <- as.factor(cl)
    x <- matrix(0, n, length(levels(cl)))
    x[(1:n) + n * (unclass(cl) - 1)] <- 1
    x <- as.matrix(x[, -1])
    dimnames(x) <- list(row.names(df), paste(cha, levels(cl)[-1], 
                                             sep = "."))
    return(x)
  }
  G <- lapply(1:ncol(df), acm.util.df)
  G <- data.frame(G, check.names = FALSE)
  return(G)
}


A<-read.csv('train.csv')
Y<-A$Survived

featuretransform <- function(A) {
  B <<- A %>% mutate(Cabin1=substr(Cabin,1,1),Ticket1=word(Ticket,-2),
                    Title=word(substr(Name,str_locate(Name,',')[,1]+2,999999),1),
                    Agen=(Age-mean(Age,na.rm=TRUE))/sd(Age,na.rm=TRUE), Agen2=Agen^2,
                    Faren=(Fare-mean(Fare,na.rm=TRUE))/sd(Fare,na.rm=TRUE), Faren2=Faren^2,
                    Constant=1,Ticket1=gsub('[./]','',Ticket1))
  numerics <- B %>% select(Constant,Agen,Agen2,Faren,Faren2) 
  factors <- B %>% select(Pclass,Sex,SibSp,Parch,Embarked,Cabin1,Ticket1,Title) 
  factors <- dummies(factors)
  X <<- as.matrix(cbind(numerics,factors))
  k <<- ncol(X)
  N <<- nrow(X)
}
featuretransform(A)

#impute missing values
OLS_beta <- function(X,Y) { #generate Yhat based on OLS of Y on X
  return(solve(t(X) %*% X) %*% t(X) %*% Y)
}

imputeage <-function() {
  ageYorig <- as.matrix(B %>% select(Agen))
  ageY <- as.matrix(B %>% filter(!is.na(Agen)) %>% select(Agen))
  ageXorig <- as.matrix(as.data.frame(X) %>% select(-Agen,-Agen2))
  ageX <- as.matrix(as.data.frame(X) %>% filter(!is.na(Agen)) %>% select(-Agen,-Agen2))
  #some variables do not vary anymore after we remove observations with missing ages.
  #Drop these variables
  varstokeep <- colSums(ageX) > 1
  ageX <- ageX[,varstokeep]
  ageXorig <- ageXorig[,varstokeep]
  agepred <- ageXorig %*% OLS_beta(ageX,ageY)
  ageYorig[is.na(ageYorig)] <- agepred[is.na(ageYorig)]
  X[,"Agen"] <<- ageYorig
  X[,"Agen2"] <<- X[,"Agen"] ^ 2
#end impute
}
imputeage()

beta <- t(t(rep(0.01,k)))

logit_ll <- function(b) { #negated logistic log-likelihood of a sample. Uses global X and Y
    z <- X %*% b
    L1 <- 1 / (1 + exp(-z)) #likelihood to have y=1
    L0 <- 1 - L1 #likelihood to have y=0
    L <- L1 * Y + L0 * (1-Y) #likelihood of observed y
    l <- -sum(log(L), na.rm=TRUE) #log-likelihood of total sample
#    if (exists('betahat')) browser() #debugging command
    return(l)
}

logit_ll_gr <- function(b) { #gradient of logistic log-likelihood
  z <- X %*% b
  L1 <- exp(-z) / (1 + exp(-z)) #likelihood to have y=1
  L0 <- 1 - L1 #likelihood to have y=0
  L1M <- matrix(L1,N,k) * X
  L0M <- matrix(L0,N,k) * (-X)
  YM <- matrix(Y,N,k)
  dL <- L1M * YM + L0M * (1-YM) #likelihood of observed y
  g <- -colSums(dL, na.rm=TRUE) #log-likelihood of total sample
  #    if (exists('betahat')) browser() #debugging command
  return(g)
}

MLE <- optim(par=beta,fn=logit_ll,gr=logit_ll_gr,method='BFGS')
betahat <- MLE$par
rownames(betahat) <- colnames(X)

predict <- function(X,b) { #predict outcomes based on estimated coefficients
  zhat <<- X %*% b
  Lhat <<- 1 / (1 + exp(-zhat))
  Yhat <<- as.numeric(Lhat >= 0.5)
  accuracy <<- sum(Y==Yhat, na.rm=TRUE) / length(Y)
#  return(Yhat)
}
predict(X,betahat)

#make predictions on test set
T<-read.csv('test.csv')
featuretransform(T)
imputeage()
betahat1 <- as.matrix(betahat[rownames(betahat) %in% colnames(X)])
X1 <- X[,colnames(X) %in% rownames(betahat)]
predict(X1,betahat1)
write.csv(Yhat,file='result.csv')