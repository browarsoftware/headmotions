#Change the path
path <- 'e:\\Publikacje\\headmotionclassification\\github\\gotowe\\do wyslania\\'
source(paste(path, 'QuaternionProcessing.R', sep = ''))
source(paste(path, 'dtw.R', sep = ''))
source(paste(path, 'plots.3d.R', sep = ''))
source(paste(path, 'averaging.R', sep = ''))

generate.features.from.eigen <- function(D, eigenvectors, mf)
{
  D<- (D)
  D <- D - mf
  lista.cech <- list()
  for (a in 1:ncol(D))
  {
    my.data <- D[,a]
    lista.cech[[a]] <- as.vector(t(eigenvectors) %*% (my.data))
  }
  return (lista.cech)
}

generate.eigen <- function(df.all, number.of.eigen, debug = FALSE)
{
  D <- df.all
  D <- t(D)
  mf <- colMeans(D)

  nrow(D) # 399 images
  ncol(D) # 64*64 = 4096 pixels
  
  D <- D - mf
  A <- cov(D)
  # Calculate the largest 20 eigenvalues and corresponding eigenvectors
  library(rARPACK)
  eigs <- rARPACK::eigs(A, number.of.eigen, which = "LM")
  # Eigenvalues
  eigenvalues <- eigs$values
  # Eigenvectors (also called loadings or "rotation" in R prcomp function: i.e. prcomp(A)$rotation)
  eigenvectors <- eigs$vectors
  if (debug)
  {
    plot(1-eigenvalues^2/sum(eigenvalues^2))
    sum(eigs$vectors[,2])
  }
  return(list(eigenvalues = eigenvalues, eigenvectors = eigenvectors, mf = mf))
}

read.data.from.folder <- function(dir.path)
{
  rd <- list()
  files <- list.files(dir.path)
  for (a in 1:length(files))
  {
    df.motion <- read.log.quaternion(paste(dir.path, "\\", files[a], sep=''))
    
    df.motion <- rotateRecording(df.motion,
                                 as.numeric(df.motion$quaternion[1,]),
                                 1, nrow(df.motion$quaternion), FALSE)
    rd[[a]] <- df.motion
  }
  return(rd)
}

read.data.from.folder2 <- function(dir.path)
{
  rd <- list()
  files <- list.files(dir.path)
  for (a in 1:length(files))
  {
    df.motion <- read.log.quaternion(paste(dir.path, "\\", files[a], sep=''))
    
    df.motion <- rotateRecording(df.motion,
                                 as.numeric(df.motion$quaternion[1,]),
                                 1, nrow(df.motion$quaternion), FALSE)
    
    rd[[a]] <- interpolate.df(df.motion, new.signal.length = new.signal.length)$coord
  }
  return(rd)
}



read.data.from.folder.marcin <- function(dir.path, begin_id, quat.to.rotate = NA)
{
  rd <- list()
  files <- list.files(dir.path)
  which.get <- c()
  if (length(begin_id) == 1)
  {
    value <- paste("^", begin_id, "-", sep = "")
    for (a in 1:length(files))
    {
      which.get <- c(which.get, grepl(value, files[a]))
    }
  } else
  {
    which.get <- rep(FALSE, length(files))
    for (b in 1:length(begin_id))
    {
      
      value <- paste("^", begin_id[b], "-", sep = "")
      for (a in 1:length(files))
      {
        which.get[a] <- which.get[a] || grepl(value, files[a])
      }
    }
  }
  
  files <- files[which.get]
  for (a in 1:length(files))
  {
    df.motion <- read.log.quaternion(paste(dir.path, "\\", files[a], sep=''), quat.to.rotate)
    df.motion <- rotateRecording(df.motion,
                                 as.numeric(df.motion$quaternion[1,]),
                                 1, nrow(df.motion$quaternion), FALSE)
    rd[[a]] <- interpolate.df(df.motion, new.signal.length = new.signal.length)$coord
  }
  return(rd)
}



read.data.to.df <- function(data.in.list)
{
  id.helper <- 1
  class.names <- c()
  for (motion.id in 1:length(motions))
  {
    vec.help <- c(data.in.list[[motion.id]][[1]]$x, data.in.list[[motion.id]][[1]]$y, data.in.list[[motion.id]][[1]]$z)
    if (motion.id == 1)
    {
      df.help <- data.frame(vec.help)
      id.helper <- id.helper + 1
    } else {
      df.help[id.helper] <- data.frame(vec.help)
      id.helper <- id.helper + 1
    }
    
    for (a in 2:length(data.in.list[[motion.id]]))
    {
      vec.help <- c(data.in.list[[motion.id]][[a]]$x, data.in.list[[motion.id]][[a]]$y, data.in.list[[motion.id]][[a]]$z)
      df.help[id.helper] <- data.frame(vec.help)
      id.helper <- id.helper + 1
    }
    class.names <- c(class.names, rep(motions[motion.id], length(data.in.list[[motion.id]])))
  }
  df.all <- df.help
  return (list(df.all = df.all, class.names = class.names))
}

list.to.large.data.frame <- function(my.list)  
{
  p1.data.frame.3d <- my.list[[1]][[1]]
  for (a in 1:length(my.list))
    for (b in 1:length(my.list[[a]]))
    {
      if (a == 1 && b == 1)
      {
        p1.data.frame.3d <- my.list[[a]][[b]]
      } else
      {
        p1.data.frame.3d <- rbind(p1.data.frame.3d, my.list[[a]][[b]])
      }
    }
  return (p1.data.frame.3d)
}

new.signal.length = 100
samples.count <- 100
motions <- c("clockwise", "counterclockwise",  "left", "node_head", "omega_left", "omega_right", "right")
number.of.eigen <- 15
confusion.matrix.template <- matrix(rep(0, length(motions)^2), nrow = length(motions), ncol = length(motions))
colnames(confusion.matrix.template) <- motions
rownames(confusion.matrix.template) <- motions
kernel <- 'radial'
dir.path <- paste(path, "\\data\\", sep = "")
dtw.results.path <- paste(path, '\\two_stages_results\\', sep = "")


set.seed(1)
persons.id <- c(1,2,3,4,5,6,7,8,9,10,11,12)
for (person.id in persons.id)
{
  print(person.id)
    set1 <- c(persons.id[-person.id])
    p1 <- list()
    print(paste('person id: ', person.id))
    print('reading training data...')
    for (a in 1:length(motions))
    {
      print(paste("   reading motion: ", motions[a]))
      dir.path.help <- paste(dir.path, motions[a], sep = "")
      q2 <- EulerAnglesToQuaternion(pi, 0, 0)
      p1[[a]] <- read.data.from.folder.marcin(dir.path.help, set1)
    }
    
    set2 <- c(person.id)
    p2 <- list()
    print('reading validation data...')
    for (a in 1:length(motions))
    {
      print(paste("   reading motion: ", motions[a]))
      dir.path.help <- paste(dir.path, motions[a], sep = "")
      q2 <- EulerAnglesToQuaternion(pi, 0, 0)
      p2[[a]] <- read.data.from.folder.marcin(dir.path.help, set2)
    }
  

  p1.data.frame.3d <- list.to.large.data.frame(p1)
  p2.data.frame.3d <- list.to.large.data.frame(p2)
  
  p1.class.names <- read.data.to.df(p1)$class.names
  p2.class.names <- read.data.to.df(p2)$class.names
  
  require(FactoMineR)
  pca.res <- PCA(p1.data.frame.3d, scale.unit = FALSE, ncp = 2)
  col.menas.help <- colMeans(p1.data.frame.3d)
  xxx <- pca.res$svd$V
  p1.data.frame.3d.2d <- pca.res$ind$coord
  
  
  p2.data.frame.3d.2d <- (as.matrix(p2.data.frame.3d[1,], 1, 2) - col.menas.help)%*% xxx
  for (a in 2:nrow(p2.data.frame.3d))
  {
    p2.data.frame.3d.2d <- rbind(p2.data.frame.3d.2d, (as.matrix(p2.data.frame.3d[a,], 1, 2) - col.menas.help)%*% xxx)
  }
  
  df.to.features.df <- function(df.helper)
  {
    how.many.columns <- nrow(df.helper) / samples.count
    rr <- data.frame(c(df.helper[1:samples.count,1], df.helper[1:samples.count,2]))
    
    for (a in 2:how.many.columns)
    {
      rr[,a] <- data.frame(c(df.helper[(((a-1) * samples.count) + 1):(a * samples.count),1], 
                             df.helper[(((a-1) * samples.count) + 1):(a * samples.count),2]))
    }
    return(rr)
  }
  
  p1.data.frame.2d.cols <- df.to.features.df(p1.data.frame.3d.2d)
  p2.data.frame.2d.cols <- df.to.features.df(p2.data.frame.3d.2d)
  
  
  ge <- generate.eigen(p1.data.frame.2d.cols, number.of.eigen, FALSE)
  eigenvalues <- ge$eigenvalues
  eigenvectors <- ge$eigenvectors
  mf <- ge$mf
  lc <- generate.features.from.eigen(p1.data.frame.2d.cols, eigenvectors, mf)
  
  classfier <- list(eigenvalues = eigenvalues, eigenvectors = eigenvectors, mf = mf, features = lc, 
                    class.names1 = p1.class.names,
                    class.names2 = p2.class.names,
                    p2.data.frame.2d.cols = p2.data.frame.2d.cols)
  
  liczba.wierszy <- length(classfier$features)
  liczba.kolumn <- length(classfier$eigenvalues)
  df <- data.frame(v1 = rep(0, liczba.wierszy))
  for (a in 2:(liczba.kolumn))
  {
    df[a] <- rep(0, liczba.wierszy)
  }
  for (a in 1:liczba.wierszy)
  {
    df[a,] <-  1/sqrt(classfier$eigenvalues) * classfier$features[[a]]
  }
  library(e1071)
  df$class <- factor(classfier$class.names1)
  svm.res <- svm(factor(class) ~ ., data = df, scale = FALSE, kernel = kernel)
  classfier$svm.res <- svm.res
  
  
  
  
  confusion.matrix.template <- matrix(rep(0, length(motions)^2), nrow = length(motions), ncol = length(motions))
  colnames(confusion.matrix.template) <- motions
  rownames(confusion.matrix.template) <- motions
  
  cm <- confusion.matrix.template
  for (a in 1:ncol(classfier$p2.data.frame.2d.cols))
  {
    vv1 <- as.vector(t(classfier$eigenvectors) %*% (classfier$p2.data.frame.2d.cols[[a]] - classfier$mf))
    vv1 <- 1/sqrt(classfier$eigenvalues) * vv1
    predict(classfier$svm.res,t(vv1))
    
    
    class.name.svd <- as.numeric(predict(classfier$svm.res,t(vv1)))
    
    cm[classfier$class.names2[a], class.name.svd] <-
      cm[classfier$class.names2[a], class.name.svd] + 1
  }
  
  cm2 <- cm
  err <- rep(0, nrow(cm2))
  for (a in 1:nrow(cm2))
  {
    for (b in 1:ncol(cm2))
    {
      if (a != b)
      {
        err[a] <- err[a] + cm2[a,b]
      }
      cm2[a,b] <- cm2[a,b] / sum(cm[a,])
    }
    err[a] <- err[a] / sum(cm[a,])
  }
  cm.error <- cbind(cm2, err)
  rownames(cm.error) <- motions
  
  person.to.classify <- person.id
  
  write.table(x = cm, file = paste(dtw.results.path,person.to.classify,'_res.csv',sep=''))
  write.table(x = cm.error, file = paste(dtw.results.path,person.to.classify,'_res_error.csv',sep=''))
}  


persons.id <- c(1,2,3,4,5,6,7,8,9,10,11,12)
path.to.results <- paste(dtw.results.path, sep = '')
tab <- list()
#for (person.to.classify in 1:12)
aa <- 1
for (person.to.classify in persons.id)
{
  tab[[aa]] <- read.table(paste(path.to.results,person.to.classify,'_res.csv',sep=''))
  aa <- aa + 1
}
tab.summary <- tab[[1]]
aa <- 1
for (person.to.classify in persons.id)
{
  tab.summary <- tab.summary + tab[[aa]]
  aa <- aa + 1
}

cm <- tab.summary
cm2 <- tab.summary
err <- rep(0, nrow(cm2))
for (a in 1:nrow(cm2))
{
  for (b in 1:ncol(cm2))
  {
    if (a != b)
    {
      err[a] <- err[a] + cm2[a,b]
    }
    cm2[a,b] <- cm2[a,b] / sum(cm[a,])
  }
  err[a] <- err[a] / sum(cm[a,])
}
cm.error <- cbind(cm2, err)

correct <- 0
for (a in 1:nrow(cm))
{
  correct <- cm[a,a] + correct
}
cm
correct / sum(cm)
 