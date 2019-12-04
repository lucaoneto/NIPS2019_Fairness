# Application of the Method (when the sensitive feature is not known at test time) 
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

# Retrive Group Memebership and Test Binary Labels
l1 = levels(T[,10])
g = (T[,10] == l1[1])
l2 = levels(T[,15])
y = (T[,15] == l2[2])

# Train a standard RF (not using as imput also the sensitive features) 
# and get both the classification on the test set with label and probability
YT = T[,15]
M = randomForest(x=L[,-c(10,15)], y=L[,15], ntree=500, do.trace = FALSE)
YF_R = predict(M,T[,-c(10,15)])
YF_P = predict(M,T[,-c(10,15)],type="prob")
T0= T; T0[,10] = T[T[,10]==l1[2],10][1];
T1= T; T1[,10] = T[T[,10]==l1[1],10][1];
YF0_P = predict(M,T0[,-c(15)],type="prob")
YF1_P = predict(M,T1[,-c(15)],type="prob")

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
eta0 = as.numeric(YF0_P[,2])
eta1 = as.numeric(YF1_P[,2])
EXetaX0 = mean(eta0)
EXetaX1 = mean(eta1)
obj = Inf
ttmp = c()
for(theta in c(-logspace(-2,2,10000), logspace(-2,2,10000))) {
  tmp = abs(mean(eta1*as.numeric(1<=2*eta+theta*(eta0/EXetaX0-eta1/EXetaX1)))/EXetaX1 -
            mean(eta0*as.numeric(1<=2*eta+theta*(eta0/EXetaX0-eta1/EXetaX1)))/EXetaX0)
  ttmp = c(ttmp, tmp)
  if (obj > tmp){
    obj = tmp
    thetahat = theta
  }
}
yp = as.numeric(1<=2*eta+thetahat*(eta0/EXetaX0-eta1/EXetaX1))

#
e = as.numeric(y != yp);
print(""); print("RF Fair")
acc = 1 - mean(e);     print(sprintf("ACC = %f",acc))
err = mean(e[g]);      print(sprintf("ERR %s = %f",l1[1],err))
err = mean(e[!g]);     print(sprintf("ERR %s = %f",l1[2],err))
err = mean(e[g & y]);  print(sprintf("ERR %s %s = %f",l1[1],l2[2],err))
err = mean(e[!g & y]); print(sprintf("ERR %s %s = %f",l1[2],l2[2],err))
print(sprintf("DEO = %f",abs(mean(e[g & y]) - mean(e[!g & y]))))