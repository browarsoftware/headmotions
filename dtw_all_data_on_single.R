#Change the path
path <- 'e:\\Publikacje\\headmotionclassification\\github\\gotowe\\do wyslania\\'
source(paste(path, 'QuaternionProcessing.R', sep = ''))
source(paste(path, 'dtw.R', sep = ''))
source(paste(path, 'plots.3d.R', sep = ''))
source(paste(path, 'averaging.R', sep = ''))

list.to.dtw.list <- function(list.to.convert)
{
  output.list <- list()
  for (a in 1:length(list.to.convert))
  {
    output.list[[a]] <- to.dtw.list(list.to.convert[[a]]$quaternion)
  }
  return (output.list)
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
  #print(files)
  for (a in 1:length(files))
  {
    df.motion <- read.log.quaternion(paste(dir.path, "\\", files[a], sep=''), quat.to.rotate)
    df.motion <- rotateRecording(df.motion,
                                 as.numeric(df.motion$quaternion[1,]),
                                 1, nrow(df.motion$quaternion), FALSE)
    rd[[a]] <- df.motion
  }
  return(rd)
}

train.classfier <- function(template.signal, path.to.save, motions, template.id)
{
  library("jdx")
  library(rJava)
  .jinit()
  .jaddClassPath(dir(java.path,full.names=TRUE))
  obj <- .jnew("dtw.Spincalc")
  .jmethods(obj)
  iterations.count = as.integer(100)
  index = as.integer(1)
  eps = 0.001
  bb <- list.to.dtw.list(template.signal[[1]])
  
  sequences.java <- convertToJava(bb)
  iterations.count.java <- convertToJava(iterations.count)
  index.java <- convertToJava(index)
  eps.java <- convertToJava(eps)
  
  
  template <- list()
  
  for (a in 1:length(template.signal))
  {
    averaged.sig <- template.signal[[a]]
    
    #length(averaged.sig)
    bb <- list.to.dtw.list(averaged.sig)
    length(bb)
    sequences.java <- convertToJava(bb)
    result=.jcall(obj, returnSig="Ljava/util/LinkedHashMap;", method="DBAtest", 
                  sequences.java, 
                  iterations.count, 
                  index.java,
                  eps.java)
    res <- convertToR(result)
    averaged.sig <- averaged.sig[[res$avg.signal.id + 1]]
    averaged.sig$quaternion <- res$avg.signal
    
    save.log.quaternion(averaged.sig, paste(path.to.save,template.id,'_',motions[a],".csv",sep=''))  
  }
}

set.seed(1)
persons.id <- c(1,2,3,4,5,6,7,8,9,10,11,12)
dir.path <- paste(path, "\\data\\", sep = "")
classifiers.path <- paste(path, '\\dtw_classifiers_single_classifier\\', sep = "")
set1 <- c(2)
motions <- c("clockwise", "counterclockwise",  "left", "node_head", "omega_left", "omega_right", "right")
java.path <- paste(path, 'JAVA_DTW\\dist\\', sep = '')
dtw.results.path <- paste(path, '\\dtw_results_single_classifier\\', sep = "")

for (person.id in persons.id)
{
  print(paste('person id: ', person.id))
  print('reading training data...')
  
  set2<- persons.id[-person.id]
  p2 <- list()
  for (a in 1:length(motions))
  {
    dir.path.help <- paste(dir.path, motions[a], sep = "")
    q2 <- EulerAnglesToQuaternion(pi, 0, 0)
    p2[[a]] <- read.data.from.folder.marcin(dir.path.help, set2)
  }
  template.signal <- p2
  print('classifier training...')
  train.classfier(template.signal, classifiers.path, motions, person.id)
}
########################reading classifiers


confusion.matrix.template <- matrix(rep(0, length(motions)^2), nrow = length(motions), ncol = length(motions))
colnames(confusion.matrix.template) <- motions
rownames(confusion.matrix.template) <- motions


for (person.to.classify in persons.id)
{
  print(paste("classification of person: ", person.to.classify))
  all.classfiers <- list()
  index <- 1

  class.ex <- list()
  for (a in 1:length(motions))
  {
    all.classfiers[[a]] <- read.log.quaternion(paste(classifiers.path,person.to.classify,'_',motions[a],".csv",sep=''))
  }
  
  set1 <- c(person.to.classify)
  p1 <- list()
  for (a in 1:length(motions))
  {
    dir.path.help <- paste(dir.path, motions[a], sep = "")
    p1[[a]] <- read.data.from.folder.marcin(dir.path.help, set1)
  }
  
  for (a in 1:length(all.classfiers))
  {
      all.classfiers[[a]] <- to.dtw.list(all.classfiers[[a]]$coord)
  }
  actual <- c()
  results <- c()
  for (aa in 1:length(p1))
    for (bb in 1:length(p1[[aa]]))
    {
      my.signal.dtw <- to.dtw.list(p1[[aa]][[bb]]$coord)
      rr <- DTWClassifier(my.signal.dtw, all.classfiers, euc.distCmp)$class.id
      
      results <- c(results, rr)
      actual <- c(actual, aa)
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
  
  write.table(x = cm, file = paste(dtw.results.path,person.to.classify,'_res.csv',sep=''))
  write.table(x = cm.error, file = paste(dtw.results.path,person.to.classify,'_res_error.csv',sep=''))
  
}


tab <- list()
aa <- 1
for (person.to.classify in persons.id)
{
  tab[[aa]] <- read.table(paste(dtw.results.path,person.to.classify,'_res.csv',sep=''))
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