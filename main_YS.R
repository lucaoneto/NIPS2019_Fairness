# Application of the Method (when the sensitive feature is known at test time) 
# to the Aduld Dataset

# Preliminary Definitions
setwd(".")
rm(list=ls())
library(randomForest)
library(pracma)
cat("\014")
set.seed(13)

# The daaset has been inserted into a single file 
# Training set is from row 1 to row 32561
# Test set is from row 32562 to the end
nl = 32561
D = read.csv("adult.tot", header=FALSE, sep=",")
ntot = nrow(D)
L = D[c(1:nl),]
T = D[c((nl+1):ntot),]

# Train a standard RF (using as imput also the sensitive features) 
# and get both the classification on the test set with label and probability
M = randomForest(x=L[,-c(15)], y=L[,15], ntree=500, do.trace = FALSE)
YT = T[,15]
YF_R = predict(M,T[,-c(15)])
YF_P = predict(M,T[,-c(15)],type="prob")

# Retrive Group Memebership and Test Binary Labels
l1 = levels(T[,10])
g = (T[,10] == l1[1])
l2 = levels(T[,15])
y = (T[,15] == l2[2])

# Print Results of Classical RF
e = as.numeric(YF_R != YT)
print(""); print("RF")
acc = 1 - mean(e);     print(sprintf("ACC = %f",acc))
err = mean(e[g]);      print(sprintf("ERR %s = %f",l1[1],err))
err = mean(e[!g]);     print(sprintf("ERR %s = %f",l1[2],err))
err = mean(e[g & y]);  print(sprintf("ERR %s %s = %f",l1[1],l2[2],err))
err = mean(e[!g & y]); print(sprintf("ERR %s %s = %f",l1[2],l2[2],err))
print(sprintf("DEO = %f",abs(mean(e[g & y]) - mean(e[!g & y]))))

# Apply The proposed Method
eta = as.numeric(YF_P[,2])
EX1s1etaX1 = mean(eta[g])
EX1s0etaX0 = mean(eta[!g])
PY1S1 = mean(eta[g])*mean(g)
PY1S0 = mean(eta[g])*(1-mean(g))
obj = Inf
ttmp = c()
for(theta in c(-logspace(-2,2,10000), logspace(-2,2,10000))) {
  tmp = abs(mean(eta[g] *as.numeric(1<=eta[g] *(2.0-theta/PY1S1)))/EX1s1etaX1 -
            mean(eta[!g]*as.numeric(1<=eta[!g]*(2.0+theta/PY1S0)))/EX1s0etaX0)
  ttmp = c(ttmp, tmp)
  if (obj > tmp){
    obj = tmp
    thetahat = theta
  }
}
yp = c()
for (i in c(1:length(y))){
  if (g[i]){
    yp = c(yp,as.numeric(1<=eta[i]*(2.0-thetahat/PY1S1)))
  }
  else {
    yp = c(yp,as.numeric(1<=eta[i]*(2.0+thetahat/PY1S0)))
  }
}

# Print Results of Fair RF
e = as.numeric(y != yp);
print(""); print("Fair RF")
acc = 1 - mean(e);     print(sprintf("ACC = %f",acc))
err = mean(e[g]);      print(sprintf("ERR %s = %f",l1[1],err))
err = mean(e[!g]);     print(sprintf("ERR %s = %f",l1[2],err))
err = mean(e[g & y]);  print(sprintf("ERR %s %s = %f",l1[1],l2[2],err))
err = mean(e[!g & y]); print(sprintf("ERR %s %s = %f",l1[2],l2[2],err))
print(sprintf("DEO = %f",abs(mean(e[g & y]) - mean(e[!g & y]))))