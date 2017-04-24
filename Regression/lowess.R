lwr <- function(x,y,t,colr){
  library(plot3D)
  library(ggplot2)
  
  n<-ncol(x)
  m<-nrow(y)
  
  
  
  Y <- data.matrix(y)
  
  X <-data.matrix(x)
  X <-cbind(1,X)
  
  pTheta <- solve(t(X)%*%X)%*%t(X)%*%Y
  theta <- matrix(nrow=m, ncol=n+1)
  for(i in 1:m){
    W <- exp( -((X[i,2]-X[,2])^2/(2*t^2)) )
    diagW <- diag(W)
    t0<-solve(t(X)%*%diagW%*%X)%*%t(X)%*%diagW%*%Y
    theta[i,]<- X[i,]%*%t0
  }
  
  plot(x,y)
  o <- order(x)
  lines(ix[o],res[o],col=colr)
  return(theta)
}