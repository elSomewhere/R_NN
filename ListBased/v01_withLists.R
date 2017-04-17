################################################################################
#Algorithm:       Standard Feed-Forward Neural Network                         #
#Implementation:  imitate object oriented via lists - no type safety           #
################################################################################
#works with gates, also tested on MNIST number database

setwd("~/R Neural Net/R_NN/gitHub/ListBased")
rm(list = ls())
source("Network.R")
source("CostFunction.R")
source("ActivationFunction.R")
source("RegulationFunction.R")




sigmoid <- function(z){
  res <- 1.0/(1.0+exp(-z))
  return(res)
}

sigmoid_diff <- function(z){
  #this has a weird numerical glitch regarding handling with infinties which we have to research a bit...
  #res <- exp(-z)/((1+exp(-z))^2)
  #res <- exp(z)/((1+exp(z))^2)
  res <- sigmoid(z)*(1-sigmoid(z))
  return(res)
}

tanh <- function(z){
  a <- nan2num((exp(z)-exp(-z)))
  b <- nan2num((exp(z)+exp(-z)))
  res <- a/b
  return(res)
}

tanh_diff <- function(z){
  res <- 1-(tanh(z))^2
  return(res)
}

relu <- function(z){
  z[(z<0)] <- 0
  res <- z
  return(res)
}


relu_diff <- function(z){
  z[z>0] <- 1
  z[!(z>0)] <- 0
  res <- z
  return(res)
}

quadraticCost <- function(a,y){
  res <- 0.5*(sqrt(sum((a-y)^2)))^2
  res <- nan2num(res)
  return(res)
}

quadraticCost_diff <- function(a,y){
  res <- a-y
  res <- nan2num(res)
  return(res)
}

crossEntropyCost <- function(a,y){
  #fx <-  expression(y*log(a)+(1-y)*log(1-a))
  n <- length(a)
  res <- -(1/n)*sum(y*log(a)+(1-y)*log(1-a))
  res <- nan2num(res)
  return(res)
}

crossEntropyCost_diff <- function(a,y){
  #fx <-  expression(y*log(a)+(1-y)*log(1-a))
  #fx <- D(fx , "a")
  res <- -((y/a)-((1-y)/(1-a)))
  res <- nan2num(res)
  return(res)
}


nan2num <- function(x){
  x[is.nan(x)] <- 0
  x[is.infinite(x)&x>0] <- .Machine$double.xmax
  x[is.infinite(x)&x<0] <- -.Machine$double.xmax
  res <- x
  return(res)
}

L2Regulation <- function(lambda,w,n){
  res <- (lambda/(2*n))*sum(w^2)
  return(res)
}

L2Regulation_diff <- function(lambda,w,n){
  res <- (lambda/n*w)
  return(res)
}


noRegulation <- function(lambda,w,n){
  res <- 0
  return(0)
}

noRegulation_diff <- function(lambda,w,n){
  res <- 0
  return(0)
}

################
#training gates#
################
#xor
training_data <- list(list(matrix(c(1,1),2,1),matrix(0,1,1)),list(matrix(c(1,0),2,1),matrix(1,1,1)),list(matrix(c(0,1),2,1),matrix(1,1,1)),list(matrix(c(0,0),2,1),matrix(0,1,1)))
#or
training_data <- list(list(matrix(c(1,1),2,1),matrix(1,1,1)),list(matrix(c(1,0),2,1),matrix(1,1,1)),list(matrix(c(0,1),2,1),matrix(1,1,1)),list(matrix(c(0,0),2,1),matrix(0,1,1)))
#and
training_data <- list(list(matrix(c(1,1),2,1),matrix(1,1,1)),list(matrix(c(1,0),2,1),matrix(0,1,1)),list(matrix(c(0,1),2,1),matrix(0,1,1)),list(matrix(c(0,0),2,1),matrix(0,1,1)))

eta <- 1
epochs <- 15000
lambda <-  0.5
mini_batch_size <- 2
costFunction <- CostFunction(crossEntropyCost,crossEntropyCost_diff)
activationFunction <- ActivationFunction(sigmoid,sigmoid_diff)
regulationFunction <- RegulationFunction(noRegulation,noRegulation_diff)
network <- Network(c(2,2,1),activationFunction=activationFunction,costFunction=costFunction,regulationFunction=regulationFunction)
network_opt_a <- SGD.Network(network,training_data, epochs, mini_batch_size, eta,lambda,shuffle=TRUE,NULL,debug=FALSE)

feedForward.Network(network_opt_a,as.matrix(c(0,0)))$output #nand
feedForward.Network(network_opt_a,as.matrix(c(1,1)))$output #and // or
feedForward.Network(network_opt_a,as.matrix(c(1,0)))$output #xor // or
feedForward.Network(network_opt_a,as.matrix(c(0,1)))$output #xor // or


