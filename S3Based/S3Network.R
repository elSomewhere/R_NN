  ################################################################################
  #Algorithm:       Standard Feed-Forward Neural Network                         #
  #Implementation:  S3 classes + local environment approach for pass-by-reference#
  ################################################################################
  #WIP - missing: backprop algo & SGD
  
  
  rm(list = ls())
  
  ActivationFunction <- function(func,func_diff){
    me <- new.env(parent=globalenv())  
    me<-list(func=func,func_diff=func_diff)
    class(me)<-append(class(me),"ActivationFunction")
    return(me)
  }
  
  RegulationFunction <- function(func,func_diff){
    me <- new.env(parent=globalenv())  
    me<-list(func=func,func_diff=func_diff)
    
    
    
    class(me)<-append(class(me),"RegulationFunction")
    return(me)
  }
  
  CostFunction <- function(func,func_diff){
    me <- new.env(parent=globalenv())  
    me<-list(func=func,func_diff=func_diff)
    class(me)<-append(class(me),"CostFunction")
    return(me)
  }
  
  
  
  
  Node <- function(id,activationFunction,initZero){
    #environments are pass by reference....
    thisEnv <- environment()
  
    #constructor variables  
    id <- id
    activationFunction <- activationFunction
    if(initZero){
      bias <- 0
    }else{
      #bias <- 0.1
      bias <- rnorm(1)
    }
    
    #other variables
    totalInput <- 0 #equivalent to z
    activation <- 0 #equivalent to sigma(z)
    inputLinks <- NULL
    outputLinks <- NULL
    numInputLinks <- 0
    numOutputLinks <- 0
    outputDer <- 0
    inputDer <- 0
    accInputDer <- 0
    numAccumulatedDers <- 0
    
    #this is the accessor list
    me <- list(
      thisEnv = thisEnv,
      updateOutput = function(){
        thisEnv$totalInput <- thisEnv$bias
        for(j in 1:length(thisEnv$inputLinks)){
          thisEnv$totalInput <-  thisEnv$totalInput + thisEnv$inputLinks[[j]]$getWeight() * thisEnv$inputLinks[[j]]$getSource()$getActivation()
        }
        thisEnv$activation <- do.call(thisEnv$activationFunction$func,list(thisEnv$totalInput))
        return(thisEnv$activation)
      },
      getEnv = function(){
        return(get("thisEnv",thisEnv))
      },getId = function(){
        return(thisEnv$id)
      },setId = function(value){
        thisEnv$id <- value
        return(thisEnv$id)
      },getInputLinks = function(){
        return(get("inputLinks",thisEnv))
      },setInputLinks = function(value){
        return(assign("inputLinks",value,thisEnv))
      },addInputLinks = function(value){
        thisEnv$inputLinks[[length(thisEnv$inputLinks)+1]] <- value
        thisEnv$numInputLinks <- thisEnv$numInputLinks+1
        return(get("inputLinks",thisEnv))
      },getNumInputLinks = function(){
        return(get("numInputLinks",thisEnv))
      },getOutputLinks = function(){
        return(get("outputLinks",thisEnv))
      },getNumOutputLinks = function(){
        return(get("numOutputLinks",thisEnv))
      },setOutputLinks = function(value){
        return(assign("outputLinks",value,thisEnv))
      },addOutputLinks = function(value){
        thisEnv$outputLinks[[length(thisEnv$outputLinks)+1]] <- value
        thisEnv$numOutputLinks <- thisEnv$numOutputLinks+1
        return(get("outputLinks",thisEnv))
      },getBias = function(){
        return(get("bias",thisEnv))
      },setBias = function(value){
        return(assign("bias",value,thisEnv))
      },getTotalInput = function(){
        return(get("totalInput",thisEnv))
      },setTotalInput = function(value){
        return(assign("totalInput",value,thisEnv))
      },getActivation = function(){
        return(get("activation",thisEnv))
      },setActivation = function(value){
        return(assign("activation",value,thisEnv))
      },getOutputDer = function(){
        return(get("outputDer",thisEnv))
      },setOutputDer = function(value){
        return(assign("outputDer",value,thisEnv))
      },getInputDer = function(){
        return(get("inputDer",thisEnv))
      },setInputDer = function(value){
        return(assign("inputDer",value,thisEnv))
      },getAccInputDert = function(){
        return(get("accInputDer",thisEnv))
      },setAccInputDert = function(value){
        return(assign("accInputDer",value,thisEnv))
      },getNumAccumulatedDers = function(){
        return(get("numAccumulatedDers",thisEnv))
      },setNumAccumulatedDers = function(value){
        return(assign("numAccumulatedDers",value,thisEnv))
      },getActivationFunction = function(){
        return(get("activationFunction",thisEnv))
      },setActivationFunction = function(value){
        return(assign("activationFunction",value,thisEnv))
      }
    )
    
    ## Define the value of the list within the current environment.
    assign('this',me,envir=thisEnv)
    
    class(me)<-append(class(me),"Node")
    return(me)
  }
  
  ##################
  
  Link <- function(source,dest,regularizationFunction,initZero){
    thisEnv <- environment()
    
    id <- paste0(source$getId()," - ",dest$getId())
    source <- source
    dest <- dest
    if(initZero){
      weight <- 0
    }else{
      #weight <- rnorm(1)-0.5
      weight <- rnorm(1)
    }
    errorDer <-  0
    accErrorDer <- 0
    numAccumulatedDers <- 0
    regularizationFunction <- regularizationFunction
    
    me <- list(
      thisEnv = thisEnv,
      getEnv = function(){
        return(get("thisEnv",thisEnv))
      },getId = function(){
        return(get("id",thisEnv))
      },setId = function(value){
        return(assign("id",value,thisEnv))
      },getSource = function(){
        return(get("source",thisEnv))
      },setSource= function(value){
        return(assign("source",value,thisEnv))
      },getDest = function(){
        return(get("dest",thisEnv))
      },setDest = function(value){
        return(assign("dest",value,thisEnv))
      },getWeight = function(){
        return(get("weight",thisEnv))
      },setWeight = function(value){
        return(assign("weight",value,thisEnv))
      },getErrorDer = function(){
        return(get("errorDer",thisEnv))
      },setErrorDer = function(value){
        return(assign("errorDer",value,thisEnv))
      },getAccErrorDer = function(){
        return(get("accErrorDer",thisEnv))
      },setAccErrorDer = function(value){
        return(assign("accErrorDer",value,thisEnv))
      },getNumAccumulatedDers = function(){
        return(get("numAccumulatedDers",thisEnv))
      },setNumAccumulatedDers = function(value){
        return(assign("numAccumulatedDers",value,thisEnv))
      },getRegularizationFunction = function(){
        return(get("regularizationFunction",thisEnv))
      },setRegularizationFunction = function(value){
        return(assign("regularizationFunction",value,thisEnv))
      }
    )
    
    ## Define the value of the list within the current environment.
    assign('this',me,envir=thisEnv)
    
    class(me)<-append(class(me),"Link")
    return(me)
  }
  
  
  #################
  
  Network <- function(shape, activationFunction, outputActivationFunction, regularizationFunction, costFunction, regLambda){
    thisEnv <- environment()
    
    shape <- shape
    activationFunction <- outputActivationFunction
    regularizationFunction <- regularizationFunction
    regLambda <- regLambda
    costFunction <- costFunction
    numLayers <- length(shape)
    
    networkLayers <- list()
    
    ##############
    #create nodes#
    ##############
    #initialize inputlayer
    inputLayerNodes <- list()
    id <- 1
    for(i in 1:shape[1]){
      inputLayerNodes[[length(inputLayerNodes)+1]] <- Node(paste0("L1N",id),activationFunction,FALSE)
      id <- id + 1
    }
    networkLayers[[length(networkLayers)+1]] <- Layer(1,inputLayerNodes,TRUE,FALSE)
    
    
    #initialize hiddenLayers
    for(i in 2 : (numLayers-1)){
      id <- 1
      hiddenLayerNodes <- list()
      for(j in 1:shape[i]){
        hiddenLayerNode <- Node(paste0("L",i,"N",id),activationFunction,FALSE)
        #create link for each node of previous layer
        inputLinks <- list()
        for(k in 1:shape[i-1]){
          link <- Link(networkLayers[[i-1]]$getNodes()[[k]],hiddenLayerNode,regularizationFunction,FALSE)
          networkLayers[[i-1]]$getNodes()[[k]]$addOutputLinks(link)
          hiddenLayerNode$addInputLinks(link)
          inputLinks[[length(inputLinks)+1]] <- link
        }
        hiddenLayerNodes[[length(hiddenLayerNodes)+1]] <- hiddenLayerNode
        id <- id + 1
      }
      networkLayers[[length(networkLayers)+1]] <- Layer(i,hiddenLayerNodes,FALSE,FALSE)
    }
    
    #initialize outputlayer
    id <- 1
    outputLayerNodes <- list()
    for(i in 1:shape[numLayers]){
      outputLayerNode <- Node(paste0(paste0("L",numLayers,"N",id)),outputActivationFunction,FALSE)
      #create link for each node of previous layer
      inputLinks <- list()
      for(k in 1:shape[numLayers-1]){
        link <- Link(networkLayers[[numLayers-1]]$getNodes()[[k]],outputLayerNode,regularizationFunction,FALSE)
        networkLayers[[numLayers-1]]$getNodes()[[k]]$addOutputLinks(link)
        outputLayerNode$addInputLinks(link)
        inputLinks[[length(inputLinks)+1]] <- link
      }
      outputLayerNodes[[length(outputLayerNodes)+1]] <- outputLayerNode
      id <- id + 1
    }
    networkLayers[[length(networkLayers)+1]] <- Layer(numLayers,outputLayerNodes,FALSE,TRUE)
    
    
    me <- list(
      thisEnv = thisEnv,
      getEnv = function(){
        return(get("thisEnv",thisEnv))
      },getShape = function(){
        return(thisEnv$shape)
      },setShape = function(value){
        thisEnv$shape <- value
        return(thisEnv$shape)
      },getActivationFunction = function(){
        return(thisEnv$activationFunction)
      },setActivationFunction = function(value){
        thisEnv$activationFunction <- value
        return(thisEnv$activationFunction)
      },getRegularizationFunction = function(){
        return(thisEnv$regularizationFunction)
      },setRegularizationFunction = function(value){
        thisEnv$regularizationFunction <- value
        return(thisEnv$regularizationFunction)
      },getCostFunction = function(){
        return(thisEnv$costFunction)
      },setCostFunction = function(value){
        thisEnv$costFunction <- value
        return(thisEnv$costFunction)
      },getNetworkLayers = function(){
        return(thisEnv$networkLayers)
      },setNetworkLayers = function(value){
        thisEnv$networkLayers <- value
        return(thisEnv$networkLayers)
      },getNumLayers = function(){
        return(thisEnv$numLayers)
      },getRegLambda = function(){
        return(thisEnv$regLambda)
      })
    
    ## Define the value of the list within the current environment.
    assign('this',me,envir=thisEnv)
    
    class(me)<-append(class(me),"Network")
    return(me) 
  }
  
  #################
  
  Layer <- function(id,nodes,isInputLayer,isOutputLayer){
    thisEnv <- environment()
    
    id <- id
    nodes <- nodes
    numNodes <- length(nodes)
    isInputLayer <- isInputLayer
    isOutputLayer <- isOutputLayer
    
    me <- list(
      getActivations = function(){
        res <- matrix(NA,thisEnv$numNodes,1)
        for(i in 1:thisEnv$numNodes){
          res[i,] <- thisEnv$nodes[[i]]$getActivation()
        }
        return(res)
      },
      thisEnv = thisEnv,
      getEnv = function(){
        return(get("thisEnv",thisEnv))
      },getId = function(){
        return(thisEnv$id)
      },setId = function(value){
        thisEnv$id <- value
        return(thisEnv$id)
      },getNodes = function(){
        return(thisEnv$nodes)
      },setNodes = function(value){
        thisEnv$nodes <- value
        return(thisEnv$nodes)
      },getNumNodes = function(){
        return(thisEnv$numNodes)
      })
    
    
    ## Define the value of the list within the current environment.
    assign('this',me,envir=thisEnv)
    
    class(me)<-append(class(me),"Layer")
    return(me)
    
  }
  
  inputs <- matrix(c(1,2,3),3,1)
  
  
  forwardPropagate <- function(network, inputs){
    
    #update input layer - the input layer has no incoming weights and biases therefore we do not actually actually build a "z" and corresponding activation
    inputLayer <- network$getNetworkLayers()[[1]]
    for(i in 1 : inputLayer$getNumNodes()){
      node <- inputLayer$getNodes()[[i]]
      node$setActivation(inputs[i,1])
      
      print(node$getActivation())
    }
    
    #update hidden alyers
    for(i in 2:network$getNumLayers()){
      hiddenLayer <- network$getNetworkLayers()[[i]]
      for(j in 1:hiddenLayer$getNumNodes()){
        node <- hiddenLayer$getNodes()[[j]]
        node$updateOutput()
      }
    }
    
    
    lastLayer <- network$getNetworkLayers()[[network$getNumLayers()]]
    res <- lastLayer$getActivations()
    return(res)
  }
  
  
  
  
  backwardPropagate <- function(network, targets){
    
    #The output node is a special case. We use the user-defined error function for the derivative
    #output cost function
    costFunction <- network$getCostFunction()$func
    costFuntion_diff <- network$getCostFunction()$func_diff
    
    #get the last layer and compute the output error  w.r. to tota input
    outputLayer <- network$getNetworkLayers()[[network$getNumLayers()]]
    for(i in 1:outputLayer$getNumNodes()){
      outputNode <- outputLayer$getNodes()[[i]]
      #set the output derivatives
      outputNode$setOutputDer(do.call(costFuntion_diff,list(outputNode$getActivation(),targets)))
    }
    
    
    #going backwards through the hidden layers
    for(i in (network$getNumLayers()-1):1){
      hiddenLayer <- network$getNetworkLayers()[[i]]
      for(j in 1:hiddenLayer$getNumNodes()){
        hiddenNode <- hiddenLayer$getNodes()[[j]]
        hiddenNode$setInputDer(hiddenNode$getOutputDer()*do.call(hiddenNode$getActivationFunction()$func_diff,list(hiddenNode$getTotalInput())))
        hiddenNode$setAccInputDert(hiddenNode$getAccInputDert()+hiddenNode$getInputDer())
        hiddenNode$setNumAccumulatedDers(hiddenNode$getNumAccumulatedDers()+1)
      }
      
      for(j in 1:hiddenLayer$getNumNodes()){
        hiddenNode <- hiddenLayer$getNodes()[[j]]
        for(k in 1 : hiddenNode$getNumInputLinks()){
          link <- hiddenNode$getInputLinks()[[k]]
          link$setErrorDer(hiddenNode$getInputDer()*link$getSource()$getActivation())
          link$setAccErrorDer(link$getAccErrorDer()+link$getErrorDer())
          link$setNumAccumulatedDers(link$getNumAccumulatedDers()+1)
        }
      }
      
      if(i>1){
        previousHiddenLayer <- network$getNetworkLayers()[[i-1]]
        for(j in 1:previousHiddenLayer$getNumNodes()){
          previousHiddenNode <- previousHiddenLayer$getNodes()[[j]]
          #Compute the error derivative with respect to each node's output.
          previousHiddenNode$setOutputDer(0)
          for(k in 1:previousHiddenNode$getNumOutputLinks()){
            outputLink <- previousHiddenNode$getOutputLinks()[[k]]
            previousHiddenNode$setOutputDer(outputLink$getWeight()*outputLink$getDest()$getInputDer())
          }
        }
      }
    }
    
    
    
    updateWeights <- function(network, learningRate, regularizationRate){
      for(i in 1:network$getNumLayers()){
        currentLayer <- network$getNetworkLayers()[[i]]
        for (j in 1:currentLayer$getNumNodes()){
          node <-  currentLayer$getNodes()[[j]]
          #Update the node's bias.
          if(node$getNumAccumulatedDers()>0){
            node$setBias(node$getBias() * node$getAccInputDert() / node$getNumAccumulatedDers())
            node$setAccInputDert(0)
            node$setNumAccumulatedDers(0)
          }
          
          #Update the weights coming into this node.
          for(k in 1 : node$getNumInputLinks()){
            link <- node$getInputLinks()[[k]]
            if(is.null(link$getRegularizationFunction())){
              regulDer <- 0
            }else{
              regulDer <- do.call(link$getRegularizationFunction()$func_diff(),list(network$getRegLambda(),link$getWeight(),))
            }
          }
        }
      }
    }
    
    
    
    #update input layer - the input layer has no incoming weights and biases therefore we do not actually actually build a "z" and corresponding activation
    inputLayer <- network$getNetworkLayers()[[1]]
    for(i in 1 : inputLayer$getNumNodes()){
      node <- inputLayer$getNodes()[[i]]
      node$setActivation(inputs[i,1])
      
      print(node$getActivation())
    }
    
    #update hidden alyers
    for(i in 2:network$getNumLayers()){
      hiddenLayer <- network$getNetworkLayers()[[i]]
      for(j in 1:hiddenLayer$getNumNodes()){
        node <- hiddenLayer$getNodes()[[j]]
        node$updateOutput()
      }
    }
    
    
    lastLayer <- network$getNetworkLayers()[[network$getNumLayers()]]
    res <- lastLayer$getActivations()
    return(res)
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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
  
  
  L2Regulation <- function(lambda,w,n){
    res <- (lambda/(2*n))*sum(w^2)
    return(res)
  }
  
  L2Regulation_diff <- function(lambda,w,n){
    res <- (lambda/n*w)
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
  
  nan2num <- function(x){
    x[is.nan(x)] <- 0
    x[is.infinite(x)&x>0] <- .Machine$double.xmax
    x[is.infinite(x)&x<0] <- -.Machine$double.xmax
    res <- x
    return(res)
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  regularizationFunction <- RegulationFunction(L2Regulation,L2Regulation_diff)
  activationFunction <- ActivationFunction(sigmoid,sigmoid_diff)
  outputActivationFunction <- ActivationFunction(sigmoid,sigmoid_diff)
  costFunction <- CostFunction(quadraticCost,quadraticCost_diff)
  
  network <- Network(c(3,4,3), activationFunction, outputActivationFunction, regularizationFunction, costFunction,0.5)
  
  input <- matrix(c(3,1,3),3,1)
  output <- matrix(c(4,2,1),3,1)
  forwardPropagate(network,input)


test1 <- network$getNetworkLayers()[[2]]
test2 <- test1$getNodes()[[1]]
test3 <- test2$getInputLinks()[[1]]
test3 <- test3$getSource()
test3$getBias()
test4 <- test3
test4$setBias(3)
test4$getBias()
test3$getBias()

test4 <- network$getNetworkLayers()[[1]]
test5 <- test4$getNodes()[[1]]
test6 <- test5$getOutputLinks()[[1]]
test6$getId()
test6$getWeight()

#indeed, the network works by reference
test3$setWeight(33)
test3$getWeight()
test6$getWeight()

network$getNetworkLayers()[[1]]$getNodes()[[1]]$getOutputLinks()[[1]]$getId()

network$thisEnv$networkLayers[[1]]$thisEnv$nodes[[1]]$thisEnv$outputLinks[[1]]$thisEnv$weight
network$thisEnv$networkLayers[[2]]$thisEnv$nodes[[1]]$thisEnv$inputLinks[[1]]$thisEnv$weight
network$thisEnv$networkLayers[[1]]$thisEnv$nodes[[1]]$thisEnv$outputLinks[[1]]$thisEnv$weight <- 66
network$thisEnv$networkLayers[[1]]$thisEnv$nodes[[1]]$thisEnv$outputLinks[[1]]$thisEnv$weight
network$thisEnv$networkLayers[[2]]$thisEnv$nodes[[1]]$thisEnv$inputLinks[[1]]$thisEnv$weight







network2 <- Network(c(3,4,3), activationFunction, outputActivationFunction, regularizationFunction, costFunction)
test1 <- network2$getNetworkLayers()[[2]]
test2 <- test1$getNodes()[[1]]
test3 <- test2$getInputLinks()[[1]]
test3$getId()
test3$getWeight()

test4 <- network2$getNetworkLayers()[[1]]
test5 <- test4$getNodes()[[1]]
test6 <- test5$getOutputLinks()[[1]]
test6$getId()
test6$getWeight()

#indeed, the network works by reference
test3$setWeight(33)
test3$getWeight()
test6$getWeight()

network2$getNetworkLayers()[[1]]$getNodes()[[1]]$getOutputLinks()[[1]]$getId()

network2$thisEnv$networkLayers[[1]]$thisEnv$nodes[[1]]$thisEnv$outputLinks[[1]]$thisEnv$weight
network2$thisEnv$networkLayers[[2]]$thisEnv$nodes[[1]]$thisEnv$inputLinks[[1]]$thisEnv$weight
network2$thisEnv$networkLayers[[1]]$thisEnv$nodes[[1]]$thisEnv$outputLinks[[1]]$thisEnv$weight <- 66
network2$thisEnv$networkLayers[[1]]$thisEnv$nodes[[1]]$thisEnv$outputLinks[[1]]$thisEnv$weight
network$thisEnv$networkLayers[[2]]$thisEnv$nodes[[1]]$thisEnv$inputLinks[[1]]$thisEnv$weight
