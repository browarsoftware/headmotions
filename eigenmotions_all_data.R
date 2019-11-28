#Change the path
path <- 'e:\\Publikacje\\headmotionclassification\\github\\gotowe\\do wyslania\\'
source(paste(path, 'QuaternionProcessing.R', sep = ''))
source(paste(path, 'dtw.R', sep = ''))
source(paste(path, 'plots.3d.R', sep = ''))
source(paste(path, 'averaging.R', sep = ''))

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

prepare.classifier <- function(dftf, number.of.eigen, debug = FALSE)
{
  #25 jest OK
  ge <- generate.eigen(dftf, number.of.eigen, debug)
  eigenvalues <- ge$eigenvalues
  eigenvectors <- ge$eigenvectors
  mf <- ge$mf
  lc <- generate.features.from.eigen(dftf, eigenvectors, mf)
  return (list(eigenvalues = eigenvalues, eigenvectors = eigenvectors, lc = lc, mf = mf))
}


new.signal.length = 100
motions <- c("clockwise", "counterclockwise",  "left", "node_head", "omega_left", "omega_right", "right")
number.of.eigen <- 40
confusion.matrix.template <- matrix(rep(0, length(motions)^2), nrow = length(motions), ncol = length(motions))
colnames(confusion.matrix.template) <- motions
rownames(confusion.matrix.template) <- motions

dir.path <- paste(path, "\\data\\", sep = "")
path.to.results <- paste(path, "\\eigen_results_", number.of.eigen, "\\", sep = "")


set.seed(1)
persons.id <- c(1,2,3,4,5,6,7,8,9,10,11,12)
for (person.id in persons.id)
{
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

  p1.all <- read.data.to.df(p1)
  p2.all <- read.data.to.df(p2)
  
  print('classifier training...')
  classifier <- prepare.classifier(p1.all$df.all, number.of.eigen)
  classifier$eigenvalues
  
  
  classiffy.motion <- function(vector.to.classify, list.of.variables, eigenvalues)
  {
    id <- 1
    min <- Inf
    for (a in 1:length(list.of.variables))
    {
      v1 <- list.of.variables[[a]]
      distance <- sqrt(sum(1/eigenvalues * (v1 - vector.to.classify)^2))
      if (distance < min)
      {
        id <- a
        min <- distance
      }
    }
    return (id)
  }
  
  actual <- c()
  results <- c()
  for (aaa in 1:ncol(p2.all$df.all))
  {
    vv1 <- as.vector(t(classifier$eigenvectors) %*% as.vector(t(p2.all$df.all[aaa] - classifier$mf)))
    class.id <- classiffy.motion(vv1, classifier$lc, classifier$eigenvalues)
    results <- c(results, p1.all$class.names[class.id])
    actual <- c(actual, p2.all$class.names[aaa])
  }
  
  cm <- confusion.matrix.template
  for (dd in 1:length(actual))
  {
    cm[actual[dd], results[dd]] <- cm[actual[dd], results[dd]] + 1
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
  
  
  write.table(x = cm, file = paste(path.to.results,person.id,'_res.csv',sep=''))
  write.table(x = cm.error, file = paste(path.to.results,person.id,'_res_error.csv',sep=''))
}  




persons.id <- c(1,2,3,4,5,6,7,8,9,10,11,12)
tab <- list()
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