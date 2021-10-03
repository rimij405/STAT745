# utils-lec3.R
#
# Utility functions used for classifier analysis from class.

## ---- error-rate-funcs ----
Error.rate.f <- function(mat) {
  (mat[1,2] + mat[2,1])/sum(mat)
}

All.error.rates.f <- function(Truth=(Default$default=="Yes"),
                             Pred=obj$fitted) {
  # Truth is logical; # Pred is the probability
  # Cover rejection rates from 0% to 100%
  Threshold.v <- sort(Pred[Truth]) # prob. for defaults only
  k <- length(Threshold.v)
  Error.mat <- matrix(0,k,3)
  for (i in 1:k) {
    prediction <- (Pred>=Threshold.v[i]) # doing prediction
    Classif.table <- table(prediction,Truth)
    Error.mat[i,1] <- Classif.table[1,2]
    Error.mat[i,2] <- Classif.table[2,1]
  }
  Error.mat[,3] <- apply(Error.mat[,1:2],1,sum) # total errors
  list(Err.rate=Error.mat/length(Truth),Threshold=Threshold.v)
}

Plot.Error.f <- function(lista=All.error.list, limit=1, cex=1) {
  mat <- lista$Err.rate
  cond <- (mat[,3]<limit)
  mat <- mat[cond,]
  Threshold <- lista$Threshold[cond]
  min.val <- min(mat[,3])
  ind <- match(min.val, mat[,3])
  vect <- c(2,4,1)
  par(mar = c(5.1, 4.1, 4.1, 2.1) + 0.1)
  matplot(Threshold, mat, type="l",col=vect, lty=vect, lwd=2, cex.lab=cex,
      main=paste("Optimum error rate is",round(min.val,4),"at Threshold =",
      round(Threshold[ind],3)), ylab="Fraction Misclassified", cex.main=cex)
  abline(h=min.val, lty=2, lwd=2)
  abline(v=Threshold[ind], lty=2, lwd=2)
  # When running the first time, we could use:
  # legend(locator(1), legend =c("Defaulting misclasified",
  #     "Non-defaulting misclasified", "Total Error"), col=vect, lty=vect)
  # "locator(1)" means that we need to click at the location of the desired
  # top left corner of legend
  legend(Threshold[ind]+0.04, max(mat[,3]),
         legend =c("Defaulting misclassified",
                   "Non-defaulting misclassified",
                   "Total Error"),
         col=vect, lty=vect)
}
