ArgMin3 <- function(a,b,c)
{
  if (a<b)
  {
    if (a<c)
    {
      return (0)
    }
    else
    {
      return (2)
    }
  }
  else
  {
    if (b<c)
    {
      return (1)
    }
    else
    {
      return (2);
    }
  }
  return (0)
}

library(compiler)
ArgMin3Cmp <- cmpfun(ArgMin3)

myDTW <- function(FUN,averageS,sequence)
{
  #averageS <- rs
  #sequence <- s1
  
  tupleAssociation <- list();
  #for (t in 1:length(averageS))
  #  tupleAssociation[[t]] <- list();
  for (t in 1:length(averageS))
    tupleAssociation[[t]] <- data.frame(v1 = numeric(), 
                                        v2 = numeric(),
                                        v3 = numeric(),
                                        v4 = numeric(),
                                        stringsAsFactors = FALSE);
  
  #for t=1:size(averageS,2)
  #tupleAssociation{t}=[];
  #end
  
  sl <- length(averageS) * length(sequence)
  seq1 <- rep(0, sl)
  #mat1 <- matrix(seq1, length(sequences[[1]]))
  
  costMatrix <- matrix(seq1, length(averageS))
  pathMatrix <- matrix(seq1, length(averageS))
  
  
  
  #costMatrix[1,1] <- quat_similarityCmp(unlist(averageS[1]),unlist(sequence[1]))
  costMatrix[1,1] <- FUN(unlist(averageS[1]),unlist(sequence[1]))
  
  
  pathMatrix[1,1] <- -1;
  
  
  for (i in 2:length(averageS))
  {
    #costMatrix[i,1] <- costMatrix[i-1,1] + quat_similarityCmp(unlist(averageS[i]),unlist(sequence[1]));
    costMatrix[i,1] <- costMatrix[i-1,1] + FUN(unlist(averageS[i]),unlist(sequence[1]));
    
    pathMatrix[i,1] <- 2;
  }
  
  for (j in 2:length(sequence))
  {
    #costMatrix[1,j] <- costMatrix[1,j-1] + quat_similarityCmp(unlist(sequence[j]),unlist(averageS[1]));
    costMatrix[1,j] <- costMatrix[1,j-1] + FUN(unlist(sequence[j]),unlist(averageS[1]));
    pathMatrix[1,j] <- 1;
  }
  
  for (i in 2:length(averageS))
  {
    for (j in 2:length(sequence))
    {
      #indiceRes <- ArgMin3(costMatrix[i-1,j-1],costMatrix[i,j-1],costMatrix[i-1,j]);
      indiceRes <- ArgMin3Cmp(costMatrix[i-1,j-1],costMatrix[i,j-1],costMatrix[i-1,j]);
      pathMatrix[i,j] <- indiceRes;
      
      if (indiceRes==0)
      {
        res <- costMatrix[i-1,j-1];
      }
      else if (indiceRes==1)
      {
        res <- costMatrix[i,j-1]
      }
      else if (indiceRes==2)
      {
        res <- costMatrix[i-1,j]
      }
      #costMatrix[i,j] <- res + distanceTo(averageS[i],sequence[j])
      #costMatrix[i,j] <- res + quat_similarity(unlist(averageS[i]),unlist(sequence[j]))
      #costMatrix[i,j] <- res + quat_similarityCmp(unlist(averageS[i]),unlist(sequence[j]))
      costMatrix[i,j] <- res + FUN(unlist(averageS[i]),unlist(sequence[j]))
      
    }
  }
  
  i <- length(averageS)
  j <- length(sequence)
  
  distance <- 0
  
  path1 <- list()
  path2 <- list()
  
  a <- 1
  while(TRUE)
  {
    path1[[a]] <- i
    path2[[a]] <- j
    a <- a + 1
    
    ttt <- tupleAssociation[[i]]
    nr <- nrow(ttt) + 1
    ttt[nr,1:4] <- unlist(sequence[j])[1:4]
    
    tupleAssociation[[i]] <- ttt
    #print(costMatrix[i,j])
    distance <- costMatrix[i,j] + distance
    #print(costMatrix[i,j])
    if (pathMatrix[i,j]==0)
    {
      i=i-1;
      j=j-1;
      #print(paste('0)', i))
    } else if (pathMatrix[i,j]==1)
    {
      j=j-1;
      #print(paste('1)', i))
    } else if (pathMatrix[i,j]==2)
    {
      i=i-1;          
      #print(paste('2)', i))
    } else
    {
      break
    }
  }
  
  path1 <- rev(unlist(path1))
  path2 <- rev(unlist(path2))
  
  #plot(path1, path2)
  #end
  
  #return(distance / (length(averageS) + length(sequence)))
  #return (costMatrix[length(averageS), length(sequence)] / (length(averageS) + length(sequence)))
  normalized_distance = costMatrix[length(averageS), length(sequence)] / (length(averageS) + length(sequence))
  newList <- list("path1" = path1, "path2" = path2, 'normalized_distance' = normalized_distance)
  return (newList)
}
myDTWCmp <- cmpfun(myDTW)


euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
library(compiler)
euc.distCmp <- cmpfun(euc.dist)

euc.dist2D <- function(x1, x2) sqrt((x1[1] - x2[1]) ^ 2 + (x1[2] - x2[2]) ^ 2)
library(compiler)
euc.dist2DCmp <- cmpfun(euc.dist)

to.dtw.list <- function(df)
{
  out.list <- list()
  for (a in 1:nrow(df))
  {
    out.list[[a]] <- as.numeric(df[a,])
  }
  return (out.list)
}

DTWClassifier <- function(signal, templates, FUN)
{
  #signal <- sig4
  #templates <- list(sig2, sig3, sig5)
  #FUN <- euc.distCmp
  results <- list()
  norm.dist <- list()
  for (a in 1:length(templates))
  {
    results[[a]] <- JAVA.DTW(FUN, signal, templates[[a]])
	#results[[a]] <- myDTWCmp(FUN, signal, templates[[a]])
    norm.dist[[a]] <- results[[a]]$normalized_distance
  }
  norm.dist <- unlist(norm.dist)
  class.id <- which(norm.dist == min(norm.dist))[1]
  return (list(class.id = class.id, results = results))
}

plot.DTW.alignment <- function(DTW.results)
{
  best.res <- DTW.results$results[[DTW.results$class.id]]
  plot(best.res$path2, best.res$path1, 
       xlab = "Reference signal [sample id]", 
       ylab = "Input signal [sample id]", 
       main ="DTW alignment", 
       type='l')
}

JAVA.DTW <- function(FUN,averageS,sequence)
{
	library(rJava)
	.jinit()
	# note that you have to point to place where utils directory is
	#Change the path
	.jaddClassPath(dir("e:\\Publikacje\\headmotionclassification\\github\\gotowe\\do wyslania\\JAVA_DTW\\dist\\",full.names=TRUE))
	objv <- .jnew("dtw.Spincalc")
	assign("obj", objv, envir = .GlobalEnv)

	library("jdx")
	o1 <- convertToJava(averageS)
	o2 <- convertToJava(sequence)
	
	# call method
	result=.jcall(obj, returnSig="Ljava/util/LinkedHashMap;", method="testConvertion2", o1, o2, "e")
	res <- convertToR(result)
	return(res)
}