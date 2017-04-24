batchGradientDescentMulti <- function(x,y,alpha,iterations){
  library(plot3D)
  library(ggplot2)
  #x matriz de datos
  #y vector resultado
  #alpha learning rate
  n<-ncol(x)
  m<-nrow(y)
  
  cost_hist<-double(iterations)
  theta_hist<-list(iterations)
  theta <- matrix(0,nrow=n+1)
  #print(theta)
  
  #Vector Y
  Y <- data.matrix(y)
  
  #Matrix X 
  X <-data.matrix(x)
  X <-cbind(1,X)
  #print(X)
  
  computeCost<-function(X,Y,theta){
    h0 <- X%*%theta
    cost <- (h0-Y)^2
    return(1/2*sum(cost))
  }
  
  for(i in 1:iterations){
    h0 <- X%*%theta
    error <- h0-Y
    delta <- (alpha/m)*(t(X)%*%error)
    theta <- theta - delta
    cost_hist[i] <- computeCost(X,Y,theta)
    theta_hist[[i]] <- theta
  }
  
  
  plot(cost_hist,type = 'l',col='red',lwd=2,main = 'Cost function',ylab = 'cost',xlab = 'Iterations')
  
  plot(x[,1],y, col=rgb(0.2,0.4,0.6,0.4), main='Line reg',
       panel.first = for(i in 1:length(theta_hist)){
         #abline(as.numeric(theta_hist[[i]]), col=rgb(1,1,0,0.05))
         abline(rbind((theta_hist[[i]])[1],(theta_hist[[i]])[2]), col=rgb(1,1,0,0.05))
         
       }) 
  
  abline(as.numeric(rbind(theta[1],theta[2])), col='red') 
  
  return(theta)
}

batchGradientDescentMultiCS229 <- function(x,y,alpha,iterations){
  library(plot3D)
  library(ggplot2)
  #x matriz de datos
  #y vector resultado
  #alpha learning rate
  n<-ncol(x)
  m<-nrow(y)
  
  cost_hist<-double(iterations)
  theta_hist<-list(iterations)
  theta <- matrix(0,nrow=n+1)
  #print(theta)
  
  #Vector Y
  Y <- data.matrix(y)
  
  #Matrix X 
  X <-data.matrix(x)
  X <-cbind(1,X)
  #print(X)
  
  #NORMALIZE
  
  
  computeCost<-function(X,Y,theta){
    h0 <- X%*%theta
    cost <- (h0-Y)^2
    return(1/2*sum(cost))
  }
  
  for(i in 1:iterations){
    h0 <- X%*%theta
    error <- Y-h0
    delta <- (alpha)*(t(X)%*%error)
    theta <- theta + delta
    cost_hist[i] <- computeCost(X,Y,theta)
    theta_hist[[i]] <- theta
  }
  
  
  #plot(cost_hist,type = 'l',col='red',lwd=2,main = 'Cost function',ylab = 'cost',xlab = 'Iterations')
  
  #plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Line reg') 
  #for(i in 1:length(theta_hist)){
  #  abline(as.numeric(theta_hist[[i]]), col='green')
  #}
  #abline(as.numeric(theta), col='red') 
  
  plot(cost_hist,type = 'l',col='red',lwd=2,main = 'Cost function',ylab = 'cost',xlab = 'Iterations')
  
  plot(x[,1],y, col=rgb(0.2,0.4,0.6,0.4), main='Line reg',
       panel.first = for(i in 1:length(theta_hist)){
         #abline(as.numeric(theta_hist[[i]]), col=rgb(1,1,0,0.05))
         abline(rbind((theta_hist[[i]])[1],(theta_hist[[i]])[2]), col=rgb(1,1,0,0.05))
         
       }) 
  
  abline(as.numeric(rbind(theta[1],theta[2])), col='red') 
  return(theta)
}


SGD<- function(x,y,alpha){
  library(plot3D)
  library(ggplot2)
  #x matriz de datos
  #y vector resultado
  #alpha learning rate
  n<-ncol(x)
  m<-nrow(y)
  
  cost_hist<-double(m)
  theta_hist<-list(m)
  theta <- matrix(1,nrow=n+1)
  #print(theta)
  
  #Vector Y
  Y <- data.matrix(y)
  
  #Matrix X 
  X <-data.matrix(x)
  X <-cbind(1,X)
  #print(X)
  
  #NORMALIZE
  
  
  computeCost<-function(X,Y,theta){
    h0 <- X[i,]*theta
    cost <- (h0-Y[i])^2
    return(1/2*sum(cost))
  }
  
  for(i in 1:m){
    h0 <- X[i,]*theta
    error <- Y[i]-h0
    delta <- (alpha)*(X[i,]*error)
    theta <- theta + delta
    cost_hist[i] <- computeCost(X,Y,theta)
    theta_hist[[i]] <- theta
  }
  
  
  plot(cost_hist,type = 'l',col='red',lwd=2,main = 'Cost function',ylab = 'cost',xlab = 'Iterations')
  
  plot(x[,1],y, col=rgb(0.2,0.4,0.6,0.4), main='Line reg',
      panel.first = for(i in 1:length(theta_hist)){
        #abline(as.numeric(theta_hist[[i]]), col=rgb(1,1,0,0.05))
        abline(rbind((theta_hist[[i]])[1],(theta_hist[[i]])[2]), col=rgb(1,1,0,0.05))
         
      }) 
  
  abline(as.numeric(rbind(theta[1],theta[2])), col='red') 
  return(theta)
}

#HELPER FUNCTIONS

linearRegModel<- function(x,y){
  slm<-lm( data.matrix(y) ~ data.matrix(x) )
  slm
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
} 

denormalize <- function(x,origx) {
  return ((x)*(max(origx)-min(origx)+min(origx)))
}


                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           