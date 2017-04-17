ActivationFunction <- function(func,func_diff){
  me<-list(func=func,func_diff=func_diff)
  class(me)<-append(class(me),"ActivationFunction")
  return(me)
}


calcSigma.ActivationFunction <- function(obj,z){
  costFunction <- obj$func
  res <- do.call(costFunction,list(z))
  return(res)
}


calcSigmaDiff.ActivationFunction <- function(obj,z){
  costFunction_diff <- obj$func_diff
  res <- do.call(costFunction_diff,list(z))
  return(res)
}


getActivationFunction.ActivationFunction <- function(obj){
  res <- obj$func
  return(res)
}

getActivationFunctionDiff.ActivationFunction <- function(obj){
  res <- obj$func_diff
  return(res)
}