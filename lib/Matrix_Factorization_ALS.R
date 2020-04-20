### ALS function

# Define a function to calculate RMSE
RMSE <- function(rating, est_rating){
  sqr_err <- function(obs){
    sqr_error <- (obs[3] - est_rating[as.character(obs[2]), as.character(obs[1])])^2
    return(sqr_error)
  }
  return(sqrt(mean(apply(rating, 1, sqr_err))))  
}

# Alternating Least Squares
# a function returns a list containing factorized matrices p(user) and q(movie), training and testing RMSEs.
als <- function(f = 10, lambda = 0.3, max.iter=1, data, train, test){
  # random assign value to matrix p and q
  p <- matrix(runif(f*U, -1, 1), ncol = U) 
  colnames(p) <- as.character(1:U)
  q <- matrix(runif(f*I, -1, 1), ncol = I)
  colnames(q) <- levels(as.factor(data$movieId))
  rate <- data %>% select(movieId, rating) %>% group_by(movieId) %>% summarise(rate_avg = mean(rating))
  q[1,] <- rate$rate_avg
  
  train_RMSE <- c()
  test_RMSE <- c()
  
  for(l in 1:max.iter){
  
  # fix q, compute p
    for(u in 1:U){
      user <- train[train$userId==u,]
      M.u <- q[,as.character(user$movieId)]
      A.u <- M.u%*%t(M.u) + lambda*nrow(user)*diag(f)
      R.u <- user$rating
      V.u <- M.u%*%R.u
      p[,u] <- solve(A.u)%*%V.u
    }
    
  # fix p, compute q
    for(i in 1:I){
      movie <- train[train$movieId==colnames(q)[i],]
      U.i<-p[,as.character(movie$userId)]
      A.i<-U.i%*%t(U.i)+lambda*nrow(movie)*diag(f)
      R.i<-movie$rating
      if (length(R.i)==1){
        V.i<-U.i*R.i
        q[,i]<-solve(A.i)%*%V.i
      }
      if (length(R.i)>1){
        V.i<-U.i%*%R.i
        q[,i]<-solve(A.i)%*%V.i
      }
    }
    
  # print the values of training and testing RMSE
    cat("epoch:", l, "\t")
    est_rating <- t(q) %*% p
    rownames(est_rating) <- levels(as.factor(data$movieId))
    
    train_RMSE_cur <- RMSE(train, est_rating)
    cat("training RMSE:", train_RMSE_cur, "\t")
    train_RMSE <- c(train_RMSE, train_RMSE_cur)
      
    test_RMSE_cur <- RMSE(test, est_rating)
    cat("test RMSE:",test_RMSE_cur, "\n")
    test_RMSE <- c(test_RMSE, test_RMSE_cur)
    }
  return(list(p = p, q = q, train_RMSE = train_RMSE, test_RMSE = test_RMSE))
}

