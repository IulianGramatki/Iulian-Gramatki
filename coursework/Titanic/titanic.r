rm(list=ls())
setwd("C:/Users/Iulian/Documents/GitHub/Iulian-Gramatki/coursework/Titanic")
library(dplyr)

A<-read.csv('train.csv')
Y<-A["Survived"]

X <- A
X <- X %>% mutate(Male = recode(Sex, 'male' = 1, 'female' = 0))
X <- X %>% rename(Male_label = Sex)

X <- as.matrix(X[c("Pclass","Age","SibSp","Parch","Fare","Male")])

beta <- t(t(rep(0.01,6)))

logit_ll <- function(b) { #negated logistic log-likelihood of a sample. Uses global X and Y

    z <- X %*% b
    L1 <- 1 / (1 + exp(-z)) #likelihood to have y=1
    L0 <- 1 - L1 #likelihood to have y=0
    L <- L1 * Y + L0 * (1-Y) #likelihood of observed y
    l <- -sum(log(L), na.rm=TRUE) #log-likelihood of total sample
    if (exists('betahat')) browser() #debugging command
    return(l)
    
}

MLE <- optim(par=beta,fn=logit_ll)
betahat <- MLE$par

predict <- function(X,b) { #predict outcomes based on estimated coefficients
  
  zhat <<- X %*% b
  Lhat <<- 1 / (1 + exp(-zhat))
  Yhat <<- Lhat >= 0.5
  accuracy <<- sum(Y==Yhat, na.rm=TRUE) / nrow(Y)
#  return(Yhat)
  
}

predict(X,betahat)
