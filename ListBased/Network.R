#--- Create class
Network<-function(sizes=NULL,activationFunction=NULL,costFunction=NULL,regulationFunction=NULL) {
  
  
  #costfunction diff are partial derivatives of costfunction with respect to the activations of a given layer
  
  #each list element is a layer
  #in each given layer, each neuron has a row
  #in each given layer, each column shows incoming signals from the past layer
  #in a given layer i, the number of columns is equal to the number of rows in layer i-1
  
  #we store biasses and weights as lists of matrices
  #we omit biases of first layer cus that is input node which doesnt have biases
  biases <- list()
  for(i in 2:length(sizes)){
    biases[[length(biases)+1]] <- as.matrix(rnorm(sizes[i]))
  }
  
  #the weights are indexed as well - first element is weights from first to second layer
  #they are index k-j : meaning kth neuron in second layer, coming from jth neuron in first neuron
  #each neuron has its own row.the num columns is the weights from incoming lines
  weights <- list()
  for(i in 2:length(sizes)){
    k <- sizes[i]
    j <- sizes[i-1]
    weights[[length(weights)+1]] <- matrix(rnorm(k*j),k,j)
  }
  
  me<-list(num_layers=length(sizes),sizes=sizes,biases=biases,weights=weights,activationFunction=activationFunction,costFunction=costFunction,regulationFunction=regulationFunction)
  class(me)<-append(class(me),"Network")
  return(me)
}


#--- Specific accessor method
getNumLayers.Network<-function(obj) {
  return(obj$num_layers)
}
getSizes.Network<-function(obj) {
  return(obj$sizes)
}
getBiases.Network<-function(obj) {
  return(obj$biases)
}
getWeights.Network<-function(obj) {
  return(obj$weights)
}
getActivationFunction.Network<-function(obj) {
  return(obj$activationFunction)
}
getCostFunction.Network<-function(obj) {
  return(obj$costFunction_diff)
}


calculateDelta.Network <- function(obj,a,y,z){
  activationFunction_diff <- obj$activationFunction
  costFunction <- obj$costFunction
  costFunction_diff <- costFunction$func_diff
  sigma_diff <- activationFunction$func_diff
  res <- do.call(costFunction_diff,list(a,y))*do.call(sigma_diff,list(z))
  #res <- do.call(costFunction_diff,list(a,y))
  return(res)
}





SGD.Network <- function(network,training_data, epochs, mini_batch_size, eta, lambda, shuffle=TRUE,test_data=NULL,debug=FALSE){
  #training_data`` is a list of tuples ``(x, y)`` representing the training inputs and the desired outputs.
  
  #num training samples
  n <- length(training_data)
  
  #pick out a randomly chosen mini batch and train
  #then pick another randomly chose mini batch and train
  #do so until we exhausted the training set
  #when we're through we had an epoch
  #we then redo
  for(i in 1 :epochs){
    print(i)
    #in each epoch, we only select a coulpe of random training samples - this is the principle of SGD
    #randomly shuffle the training sdata
    if(shuffle){
      training_data <- sample(training_data)
    }
    
    
    #create this epochs set of mini batches
    mini_batches <- list()
    for(k in seq(1,n,mini_batch_size)){
      mini_batches[[length(mini_batches)+1]] <- training_data[k:(k+mini_batch_size-1)]
    }
    
    #in a given epoch, we train the network with the mini batches
    for(mini_batch in mini_batches){
      network <- update_mini_batch.Network(network,mini_batch,n,eta,lambda, debug)
    }
    
    
    #print(network$weights[[length(network$weights)]])
    if(!is.null(test_data)){
      evaluateGeneralized.Network(network,test_data)
    }
  }
  
  return(network)
}





update_mini_batch.Network <- function(network, mini_batch, n, eta, lambda, debug){
  #eta is learning rate
  #mini_batch is a list of tuples of (x and y)
  #Apply gradient descent to a single mini batch - we got the gradients as outputs fromt the backpropagation algorithm
  regulationFunction_diff <- network$regulationFunction$func_diff
  #create empty matrices with same shape as the biases - as many empty matrices as layers
  #this is essentially the matrix of C differentiated with respect to bias
  b <- getBiases.Network(network)
  nabla_b <- list()
  for(i in 1:length(b)){
    nabla_b[[length(nabla_b)+1]] <- matrix(0,dim(b[[i]])[1],dim(b[[i]])[2])
  }
  
  #create empty matrices with same shape as the weights - as many empty matrices as layers
  #this is essentially the matrix of C differentiated with respect to weight
  w <- getWeights.Network(network)
  nabla_w <- list()
  for(i in 1:length(w)){
    nabla_w[[length(nabla_w)+1]] <- matrix(0,dim(w[[i]])[1],dim(w[[i]])[2])
  }
  

  m <- length(mini_batch)
  numLayers <- getNumLayers.Network(network)
  
  #the nablas hold the summed up gradients for each training sample
  for(j in mini_batch){
    x <- j[[1]]
    y <- j[[2]]
   
    bp_output <- backProp(network,x,y)
    
    
    for(i in (numLayers-1):1){
      nabla_b[[i]] <- nabla_b[[i]]+bp_output[[1]][[i]]
      nabla_w[[i]] <- nabla_w[[i]]+bp_output[[2]][[i]]
    }
  }
  
  for(i in (numLayers-1):1){
    #after a given abckpropagation step, add the gradients
    #b[[i]] <- b[[i]] - (eta/m) * nabla_b[[i]]
    #w[[i]] <- w[[i]] - (eta/m) * nabla_w[[i]]
    b[[i]] <- b[[i]] - (eta) * (nabla_b[[i]]/m)
    #w[[i]] <- w[[i]] - ((eta) * (nabla_w[[i]]/m)) - ((eta) * do.call(regulationFunction_diff,list(lambda,w[[i]],m)))
    w[[i]] <- w[[i]] - ((eta) * (nabla_w[[i]]/m)) - ((eta) * do.call(regulationFunction_diff,list(lambda,w[[i]],n)))
  }
  
  network$biases <- b
  network$weights <- w
  
  if(debug){
    print("w is")
    print(w)
    print("b is")
    print(b)
  }
  return(network)
}



backProp <- function(network,x,y){
  numLayers <- getNumLayers.Network(network)
  activationFunction <- getActivationFunction.Network(network)
  costFunction <- getCostFunction.Network(network)
  
  
  #create empty matrices with same shape as the biases - as many empty matrices as layers
  #this is essentially the matrix of C differentiated with respect to bias
  b <- getBiases.Network(network)
  nabla_b <- list()
  for(i in 1:length(b)){
    nabla_b[[length(nabla_b)+1]] <- matrix(0,dim(b[[i]])[1],dim(b[[i]])[2])
  }
  
  #create empty matrices with same shape as the weights - as many empty matrices as layers
  #this is essentially the matrix of C differentiated with respect to weight
  w <- getWeights.Network(network)
  nabla_w <- list()
  for(i in 1:length(w)){
    nabla_w[[length(nabla_w)+1]] <- matrix(0,dim(w[[i]])[1],dim(w[[i]])[2])
  }
  
  #feed forward
  ffw <- feedForward.Network(network,x)
  zs <- ffw$zs
  activations <- ffw$activations
  
  lastLayer_index <- numLayers-1
  #backward pass
  #BP1
  #compute final output error with last activation output (which equals the feed forward output) and last z input into sigma function
  delta <- calculateDelta.Network(network,activations[[numLayers]],y,zs[[lastLayer_index]])
  #in a perfectly symbolic environment: simga diff w.r. z
  #this is C diff w.r. to a &
  
  
  
  #we also compute the final gradient
  #BP3
  nabla_b[[length(nabla_b)]] <- delta
  #BP4
  nabla_w[[length(nabla_w)]] <- delta%*%t(activations[[lastLayer_index]])
  #in a perfectly symbolic environment: simga diff w.r. z
  #this is C diff w.r. to a &
  
  #here real backpropagation starts
  #we are in the second last layer now - we will loop till we reach the first layer
  for(l in (numLayers-1):2){
    #l is actually l-1 here.... there is problem in the sense that zs & b & w are three long & activations is 4 long
    #BP3
    z_l <- zs[[l-1]]
    sigma_diff_zl <- do.call(activationFunction$func_diff,list(z_l))
    delta <- (t(w[[l]])%*%delta)*sigma_diff_zl #elementwise multiplication
    #BP3
    nabla_b[[l-1]] <- delta
    #BP4
    nabla_w[[l-1]] <- delta%*%t(activations[[l-1]])
  }
  
  res <- list(nabla_b,nabla_w)
  
  return(res)
}




#given an input a for the network, returns the corresponding output
feedForward.Network <- function(network,input){
  #feed forward
  #this usually starts at the second layer
  #compute, for each training example x
  #z_x_l = w_i*a_x_lm1+b_l
  #a_x_l = sigma(z_x_l)
  
  activationFunction <- getActivationFunction.Network(network)
  biases <- network$biases
  weights <- network$weights
  tuples <- mapply(list,biases,weights,SIMPLIFY=FALSE)
  
  
  activation <- input
  activations <- list(activation) # list to store all the activations, layer by layer
  zs <- list() #list to store all the z vectors - remember, z is what enters the acticvation function sigma. z is w*a+b
  
  for(i in tuples){
    
    #calculate z
    z <- i[[2]]%*%activation+i[[1]]
    zs[[length(zs)+1]] <- z
    
    #apply sigma function on z
    activation <- do.call(activationFunction$func,list(z)) #a_x_l = sigma(z_x_l)
    activations[[length(activations)+1]] <- activation
    
  }
  
  output <- activation
  
  res <- list(output,zs,activations)
  names(res) <- c("output","zs","activations")
  return(res)
}












GD.Network <- function(network,training_data, epochs, eta,test_data){
  #training_data`` is a list of tuples ``(x, y)`` representing the training inputs and the desired outputs.
  
  #num training samples
  n <- length(training_data)
  
  #pick out a randomly chosen mini batch and train
  #then pick another randomly chose mini batch and train
  #do so until we exhausted the training set
  #when we're through we had an epoch
  #we then redo
  for(i in 1 :epochs){
    print(i)
    
    
    network <- update_mini_batch.Network(network,training_data,eta, debug)
    
    evaluateGeneralized.Network(network,test_data)
  }
  
  return(network)
}






evaluate.Network <- function(network,training_data){
  total <- length(training_data)
  correct <- 0
  for(i in 1:total){
    x <- training_data[[i]][[1]]
    y <- training_data[[i]][[2]]
    y <- which(as.vector(y)==1)-1
    out <- feedForward.Network(network,x)
    out <- out$output
    y_hat <- which(as.vector(out)==max(as.vector(out)))
    if(y==y_hat){
      correct <- correct+1
    }
  }
  res <- correct/total
  return(res)
}


evaluateGeneralized.Network <- function(network,test_data){
  total <- length(test_data)
  correct <- 0
  for(i in 1:total){
    x <- test_data[[i]][[1]]
    y <- test_data[[i]][[2]]
    out <- feedForward.Network(network,x)
    out <- out$output
    id_max <- which(as.vector(out)==max(as.vector(out)))
    out[id_max,] <- 1
    out[-id_max,] <- 0
    id_max_y <- which(as.vector(y)==max(as.vector(y)))
    y[id_max_y,] <- 1
    y[-id_max_y,] <- 0
    if(sum(y==out)==10){
      correct <- correct+1
    }
  }
  print(paste0("got ",correct," of ",total, " correct"))
}


evaluateGeneralizedSingle.Network <- function(network,input){
  y <- input[[2]]
  out <- feedForward.Network(network,input[[1]])
  out_res <- out$output
  id_max <- which(as.vector(out_res)==max(as.vector(out_res)))
  out_res[id_max,] <- 1
  out_res[-id_max,] <- 0
  
  res <- list(y,out_res,out$output)
  return(res)
}






#
#
# training_data <- list(list(as.matrix(c(1,2)),as.matrix(c(1,2,3))),list(as.matrix(c(4,5)),as.matrix(c(6,7,8))),list(as.matrix(c(9,1)),as.matrix(c(2,3,4))),list(as.matrix(c(2,5)),as.matrix(c(3,7,5))),list(as.matrix(c(5,5)),as.matrix(c(6,7,8))),list(as.matrix(c(5,5)),as.matrix(c(1,7,8))),list(as.matrix(c(2,5)),as.matrix(c(3,7,8))),list(as.matrix(c(6,7)),as.matrix(c(1,4,8))))
# eta <- 0.05
# epochs <- 1500
# mini_batch_size <- 2
# network <- Network(c(2,3,2,3),sigmoid,sigmoid_diff,quadraticCost_diff)
# network_opt <- SGD.Network(network,training_data, epochs, mini_batch_size, eta)