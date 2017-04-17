RegulationFunction <- function(func,func_diff){
  me<-list(func=func,func_diff=func_diff)
  class(me)<-append(class(me),"RegulationFunction")
  return(me)
}