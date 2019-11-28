############################################################
#Important functions for DBA and quaternions smoothing
############################################################

#' Quaternion Markley averaging algorithms.
#'
#' See: F. Landis Markley, Yang Cheng, John Lucas Crassidis, and Yaakov Oshman.  "Averaging Quaternions", Journal of Guidance, Control, and Dynamics, Vol. 30, No. 4 (2007), pp. 1193-1197. https://doi.org/10.2514/1.28949
#' @param Q a data frame of quaternions (four dimensional vectors) to be averaged. Each row of data frame holds one quaternion.
#'
#' @return 4D quaternion vector.
#'
#' @examples
#' Q <- data.frame(c(0.9999986, 0.9999986, 0.9999986, 0.9999986, 0.9999986, 0.9999986),
#' c(0.0008716584, 0.0008716584, 0.0009590162, 0.0009590162, 0.001046359, 0.001046359),
#' c(0.0009608439, 0.001048034, 0.0008736689, 0.001048034, 0.0009608439, 0.0008736689),
#' c(0.001046359, 0.0009590162, 0.001046359, 0.0008716584, 0.0008716584, 0.0009590162))
#' avg.quaternion.markley(Q)
avg.quaternion.markley <- function(Q)
{
  q <- t(as.matrix(Q[1,]))
  
  q %*% t(q)
  
  A <- rep(0, 16)
  A <- matrix(A, 4)
  M <- nrow(Q)
  for (a in 1:M)
  {
    q <- t(as.matrix(Q[a,]));
    A <- (q %*% t(q)) + A #rank 1 update
  }
  A <- (1.0 / M) * A
  
  return(eigen(A)$vectors[,1])
}

library(compiler)
avg.quaternion.markleyCmp <- cmpfun(avg.quaternion.markley)

#helper function
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

#helper function
distanceTo <- function(a,b)
{
  dist <- (a-b)*(a-b)
  return (dist)
}

#helper function
norm_vec <- function(x) sqrt(sum(x^2))
#helper function
dot_prod <- function(x, y) sum(x * y)
#helper function
quat_similarity2 <- function (x, y)
{
  dot = dot_prod(x, y)
  if(dot < 0.0)
  {
    dot = dot_prod(x, -y)
  }
  return (1 - dot)
}
#helper function
quat_similarity <- function (x, y)
{
  return (1 - abs(sum(x * y)))
}

library(compiler)
quat_similarityCmp <- cmpfun(quat_similarity)

#helper function
getSingleSIgnal <- function(all_quat, id)
{
  ss <- list()
  for (a in 1:length(all_quat))
  {
    ss[[a]] <- all_quat[[a]][id,]
  }
  return(ss)
}

#helper function
DBA_one_iteration <- function(averageS,sequences)
{
  #averageS <- ds1[[4]]$quaternion
  averageS <- averageS$quaternion
  #sequences <- ds1
  
  
  tupleAssociation <- list();
  for (t in 1:nrow(averageS))
    tupleAssociation[[t]] <- data.frame(v1 = numeric(),
                                        v2 = numeric(),
                                        v3 = numeric(),
                                        v4 = numeric(),
                                        stringsAsFactors = FALSE);
  
  
  
  
  numberOfSignals <- length(sequences)
  
  for (k in 1:numberOfSignals)
  {
    #sequence <- getSingleSIgnal(sequences, k)
    sequence <- sequences[[k]]$quaternion
    
    sl <- nrow(averageS) * nrow(sequence)
    seq1 <- rep(0, sl)
    
    costMatrix <- matrix(seq1, nrow(averageS))
    pathMatrix <- matrix(seq1, nrow(averageS))
    
    
    
    costMatrix[1,1] <- quat_similarityCmp(averageS[1,],sequence[1,])
    pathMatrix[1,1] <- -1;
    
    
    for (i in 2:nrow(averageS))
    {
      costMatrix[i,1] <- costMatrix[i-1,1] + quat_similarityCmp(averageS[i,],sequence[1,])
      pathMatrix[i,1] <- 2;
    }
    
    for (j in 2:nrow(sequence))
    {
      costMatrix[1,j] <- costMatrix[1,j-1] + quat_similarityCmp(sequence[j,],averageS[1,])
      pathMatrix[1,j] <- 1;
    }
    
    
    for (i in 2:nrow(averageS))
    {
      for (j in 2:nrow(sequence))
      {
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
        costMatrix[i,j] <- res + quat_similarityCmp(averageS[i,],sequence[j,])
        
      }
    }
    i <- nrow(averageS)
    j <- nrow(sequence)
    
    
    while(TRUE)
    {
      ttt <- tupleAssociation[[i]]
      nr <- nrow(ttt) + 1
      ttt[nr,1:4] <- sequence[j,][1:4]
      
      tupleAssociation[[i]] <- ttt
      
      
      if (pathMatrix[i,j]==0)
      {
        i=i-1;
        j=j-1;
      } else if (pathMatrix[i,j]==1)
      {
        j=j-1;
      } else if (pathMatrix[i,j]==2)
      {
        i=i-1;
      } else
      {
        break
      }
    }
  }
  
  averageSR <- data.frame(x = rep(0,nrow(averageS)), 
                          y = rep(0,nrow(averageS)),
                          z = rep(0,nrow(averageS)),
                          w = rep(0,nrow(averageS)))
  for (t in 1:nrow(averageS))
  {
    df <- tupleAssociation[[t]]
    #averageSR[[t]] <- avg.quaternion.markleyCmp(df)
    averageSR[t,] <- avg.quaternion.markleyCmp(df)
  }
  
  norm.distance[norm.distance.index] <<- costMatrix[nrow(averageS), nrow(sequence)] / (nrow(averageS) + nrow(sequence))
  return(averageSR)
}

library(compiler)
DBA_one_iterationCmp <- cmpfun(DBA_one_iteration)

################################
#helper function
my_dba <- function(sequences, iterationsCount = 100, index = -1, eps = 0.001)
{
  norm.distance <<- rep(NA, iterationsCount)
  
  if (index < 0)
  {
    index <- round(runif(1, 1, length(sequences)))
  }
  average <- sequences[[index]]
  sequences <- sequences[-index]
  for (i in 1:iterationsCount)
  {
    norm.distance.index <<- i
    
    average$quaternion=DBA_one_iterationCmp(average,sequences)
    
    message(paste('Iteration: ', i, ", normalized distance:", norm.distance[norm.distance.index], sep = ""))
    if (norm.distance[norm.distance.index] < eps)
    {
      message("Coverage")
      return(average)
    }
    if (i > 1)
    {
      if (abs(norm.distance[norm.distance.index - 1] - norm.distance[norm.distance.index]) < eps)
      {
        message("Coverage")
        return(average)
      }
    }
  }
  return (average)
}
library(compiler)
my_dbaCmp <- cmpfun(my_dba)


#' Weighted quaternion Markley averaging algorithms.
#'
#' See: F. Landis Markley, Yang Cheng, John Lucas Crassidis, and Yaakov Oshman.  "Averaging Quaternions", Journal of Guidance, Control, and Dynamics, Vol. 30, No. 4 (2007), pp. 1193-1197. https://doi.org/10.2514/1.28949
#'
#' @param Q a data frame of quaternions (four dimensional vectors) to be averaged. Each row of data frame holds one quaternion.
#' @param weights weights vector.
#'
#' @return 4D quaternion vector.
#'
#' @examples
#' Q <- data.frame(c(0.9999986, 0.9999986, 0.9999986, 0.9999986, 0.9999986, 0.9999986),
#' c(0.0008716584, 0.0008716584, 0.0009590162, 0.0009590162, 0.001046359, 0.001046359),
#' c(0.0009608439, 0.001048034, 0.0008736689, 0.001048034, 0.0009608439, 0.0008736689),
#' c(0.001046359, 0.0009590162, 0.001046359, 0.0008716584, 0.0008716584, 0.0009590162))
#'
#' x <- seq(-2,2,length=6)
#' y <- dnorm(x,mean=0, sd=1)
#' y <- y / sum(y)
#' wavg.quaternion.markley(Q, y)
wavg.quaternion.markley <- function(Q, weights)
{
  q <- t(as.matrix(Q[1,]))
  
  q %*% t(q)
  
  A <- rep(0, 16)
  A <- matrix(A, 4)
  M <- nrow(Q)
  wSum <- 0
  for (a in 1:M)
  {
    q <- t(as.matrix(Q[a,]));
    w_i <- weights[a]
    A <- w_i * (q %*% t(q)) + A #rank 1 update
    wSum <- wSum + w_i
  }
  A <- (1.0 / wSum) * A
  return(eigen(A)$vectors[,1])
}

#helper function
gaussianQuaternionSmoother <- function(quaternionSignal, windowSize)
{
  #quaternionSignal <- as
  #windowSize <- 10
  startInd <- floor(windowSize / 2)
  startLoop <- startInd
  endInd <- ceiling(windowSize / 2)
  startInd+endInd
  endLoop <- length(quaternionSignal)-endInd
  
  x <- seq(-2,2,length=windowSize)
  y <- dnorm(x,mean=0, sd=1)
  y <- y / sum(y)
  
  
  smoothedSignal <- list()
  for (a in 1:length(quaternionSignal))
  {
    smoothedSignal[[a]] <- quaternionSignal[[a]]
  }
  
  for (a in startLoop:endLoop)
  {
    ii <- 1
    sampleToSmooth <- data.frame(v1 = numeric(),
                                 v2 = numeric(),
                                 v3 = numeric(),
                                 v4 = numeric(),
                                 stringsAsFactors = FALSE);
    for (b in (-startInd+1):endInd)
    {
      sampleToSmooth[ii,] <- quaternionSignal[[a+b]]
      ii <- ii + 1
    }
    
    smoothedSignal[[a]] <- wavg.quaternion.markley(sampleToSmooth, y)
  }
  return (smoothedSignal)
}

############################################################
#
############################################################

#helper function
euler2quaternion <- function(xR, yR, zR)
{
  a1 <- c(zR, yR, xR) * (pi/180)
  q <- EA2Q(a1,'zyx')
  return(q)
}

#helper function
quaternion2euler <- function(q)
{
  ea <- Q2EA(q,'zyx') * (180/pi)
  return(c(ea[3], ea[2], ea[1]))
}

#helper function
resampleArray <- function(sig, newSignalLength) {
  xi <- seq(from = 1, to = length(sig), length.out = newSignalLength)
  retSig = interp1(1:length(sig), sig, xi, method = "nearest")#"linear", "nearest", "pchip", "cubic", "spline"
  return (retSig)
}

