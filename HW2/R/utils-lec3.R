# utils-lec3.R
#
# Utility functions used for classifier analysis from class.

## ---- error-rate-funcs ----

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

Plot.Error.f <- function(lista=All.error.list, limit=1) {
  mat <- lista$Err.rate
  cond <- (mat[,3]<limit)
  mat <- mat[cond,]
  Threshold <- lista$Threshold[cond]
  min.val <- min(mat[,3])
  ind <- match(min.val, mat[,3])
  vect <- c(2,4,1)
  par(mar = c(4, 4.5, 2, 1) + 0.1)
  matplot(Threshold, mat, type="l",col=vect, lty=vect, lwd=2, cex.lab=2,
      main=paste("Optimum error rate is",round(min.val,4),"at Threshold =",
      round(Threshold[ind],3)), ylab="Fraction Misclassified", cex.main=2)
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

## ---- cross-validation-funcs ----

CV.error.f <- function(data=Default,k=10,t1=0,t2=1,m=20,
                       Rounds=2) {
  # k-fold CV; uses m=100 thresholds from t1 to t2
  Threshold.v <- seq(from=t1, to=t2, length=m)
  Cost.arr <- array(0,c(m,Rounds,k))
  for (i in 1:Rounds) {
    fold=sample(rep(1:k,length=nrow(data)))
    for (j in 1:k) {
      cond <- (fold==j)
      obj <- glm(default ~ balance + student,
                 data = data[!cond,], family = "binomial")
      Pred <- predict(obj, newdata = data[cond,],
                      type = "response")
      Truth=(data[cond,]$default=="Yes") # Truth is logical
      for (ind in 1:m) {
        prediction <- (Pred>=Threshold.v[ind])
        Cost.arr[ind,i,j] <- mean((prediction==F)&(Truth==T))*10 +
          mean((prediction==T)&(Truth==F)) # not using "table"
      }
    }
  }
  list(Cost=Cost.arr, Threshold=Threshold.v)
}

Plot.CV.Cost.f <- function(lista=CV.error.list,from=0, to=1,
                           Truth=(Default$default=="Yes"), Pred=obj$fitted) {
  Threshold <- lista$Threshold
  cond <- ((Threshold>=from)&(Threshold<=to)) # range
  Threshold <- Threshold[cond]
  Cost.arr <- lista$Cost[cond,,]
  N <- prod(dim(Cost.arr)[2:3]) # Rounds*k
  cost <- apply(Cost.arr,1,mean)
  cost.se <- apply(Cost.arr,1,sd)/sqrt(N)
  low <- cost - 2*cost.se;   high <- cost + 2*cost.se
  mat <- cbind(cost,low,high)
  # now calculating the optimal threshold
  min.val <- min(cost)
  ind <- match(min.val, cost) # Threshold[ind] is the optimal threshold
  par(mar = c(4.5,4.5,0,1))
  matplot(Threshold,mat,type="l", xlab=paste("Threshold (optimal at ",
      round(Threshold[ind],3),")"), ylab="Cost of Misclassification",lty=c(1,2,2))
  abline(h=min.val, lty=2);
  abline(v=Threshold[ind], lty=2)
  Threshold[ind] # the optimal threshold in case we need it
}
