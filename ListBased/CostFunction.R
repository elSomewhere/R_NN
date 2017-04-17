CostFunction <- function(func,func_diff){
  me<-list(func=func,func_diff=func_diff)
  class(me)<-append(class(me),"CostFunction")
  return(me)
}


calculateCost.CostFunction <- function(obj,a,y){
  costFunction <- obj$func
  res <- do.call(costFunction,list(a,y))
  return(res)
}


calculateCostDiff.CostFunction <- function(obj,a,y){
  costFunction <- obj$func_diff
  res <- do.call(costFunction,list(a,y))
  return(res)
}


