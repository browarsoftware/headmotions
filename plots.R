#########################
#Plot 1
error <- c(0.9203791, 0.9478673, 0.9488152, 0.9374408, 0.943128, 0.9308057, 0.9232227, 0.9175355, 0.943128, 0.9620853, 0.9753555,
           0.9232227, 0.9327014, 0.9393365, 0.9222749, 0.9232227, 0.9393365, 0.9393365)

names(error) <- c('Eigen 5', 'Eigen 10', 'Eigen 15', 'Eigen 20', 'Eigen 25', 'Eigen 30', 'Eigen 35', 'Eigen 40',
                  'Two-Stage PCA', 'DTW', 'DTW bagged',
                  'NN 50', 'NN 100', 'NN 200', 'NN 50x50', 'NN 100x100', 'NN 200x200', 'RF')


color <- c("cyan","cyan","cyan","cyan","cyan","cyan","cyan","cyan",
           "magenta", "orange","green", 
           'blue', 'blue', 'blue', 'blue', 'blue', 'blue', 'red')

barplot(error,
        main="Recognition rate of classification methods",
        #xlab="Method",
        ylab="Recognition rate",
        border="black",
        col=color,
        #density=10,
        ylim=c(0.8,1.0),
        xpd=FALSE,
        las=2
)
par(mar=c(8,6,2,2))

####################
#Plot 2
dir.path <- "e:\\Publikacje\\headmotionclassification\\github\\gotowe\\data\\"
motions <- c("clockwise", "counterclockwise",  "left", "node_head", "omega_left", "omega_right", "right")
id <- 1
p.id <- 1
x.helper <- c()
y.helper <- c()
z.helper <- c()
color.color <- c()
color.all <- c("red", "green", "black","cyan", "magenta", "yellow", "blue",
               "pink", "violet", "purple", "brown", "navy")
persons.id <- c(1,2,3,4,5,6,7,8,9,10,11,12)
for (p.id in persons.id)
{
  my.dir.path <- paste(dir.path, motions[id], "\\", persons.id[p.id], "-01.log", sep="")
  df.motion <- read.log.quaternion(my.dir.path)
  df.motion <- rotateRecording(df.motion,
                               as.numeric(df.motion$quaternion[1,]),
                               1, nrow(df.motion$quaternion), FALSE)
  
  x.helper <- c(x.helper, df.motion$coord$x)
  y.helper <- c(y.helper, df.motion$coord$y)
  z.helper <- c(z.helper, df.motion$coord$z)
  color.color <- c(color.color, rep(color.all[p.id], length(df.motion$coord$x)))
}
  library(scatterplot3d)
  scatterplot3d(x.helper, z.helper, y.helper,
                color = color.color,
                xlab="X",
                ylab="Z",
                zlab="Y", #type='l'
                xlim = c(-1,1), ylim = c(-1,1), zlim = c(-1,1),
                angle = 20, scale = 0.2,
                main = "Clockwise motions"
  )


  for (p.id in persons.id)
  {
    my.dir.path <- paste(dir.path, motions[id], "\\", persons.id[p.id], "-01.log", sep="")
    df.motion <- read.log.quaternion(my.dir.path)
    df.motion <- rotateRecording(df.motion,
                                 as.numeric(df.motion$quaternion[1,]),
                                 1, nrow(df.motion$quaternion), FALSE)
    
    x.helper <- c(x.helper, df.motion$coord$x)
    y.helper <- c(y.helper, df.motion$coord$y)
    z.helper <- c(z.helper, df.motion$coord$z)
    color.color <- c(color.color, rep(color.all[p.id], length(df.motion$coord$x)))
  }
library(scatterplot3d)
scatterplot3d(x.helper, z.helper, y.helper,
              color = color.color,
              xlab="X",
              ylab="Z",
              zlab="Y", #type='l'
              xlim = c(-1,1), ylim = c(-1,1), zlim = c(-1,1),
              angle = 20, scale = 0.2,
              main = "Clockwise motions",
              pch=16
)
  
  
#############################################
#Plot 3
dir.path <- "e:\\Publikacje\\headmotionclassification\\github\\gotowe\\data\\"
motions <- c("clockwise", "counterclockwise",  "left", "node_head", "omega_left", "omega_right", "right")
id <- 5
p.id <- 1
x.helper <- c()
y.helper <- c()
z.helper <- c()
color.color <- c()
color.all <- c("red", "green", "black","cyan", "magenta", "yellow", "blue",
               "pink", "violet", "purple", "brown", "navy")
pch.pch <- c()
for (file.id in 1:12)
{
  file.name <- file.id
  if (file.name < 10)
  {
    file.name <- paste("0", file.name, sep = "")
  } else
  {
    file.name <- paste(file.name, sep = "")
  }
  
  my.dir.path <- paste(dir.path, motions[id], "\\", persons.id[p.id], "-", file.name,".log", sep="")
  df.motion <- read.log.quaternion(my.dir.path)
  df.motion <- rotateRecording(df.motion,
                               as.numeric(df.motion$quaternion[1,]),
                               1, nrow(df.motion$quaternion), FALSE)
  
  x.helper <- c(x.helper, df.motion$coord$x)
  y.helper <- c(y.helper, df.motion$coord$y)
  z.helper <- c(z.helper, df.motion$coord$z)
  
  xx = plot.in.3d(df.motion$coord$x,
                  df.motion$coord$y,
                  df.motion$coord$z, color = NULL,color.base = 'g', new.plot = FALSE)
  color.color <- c(color.color, xx)
  pch.pch <- c(pch.pch, rep(20, length(df.motion$coord$x)))
}

df.motion <- read.log.quaternion("e:\\Publikacje\\headmotionclassification\\github\\gotowe\\dtw_classifiers\\1_omega_left.csv")
df.motion <- rotateRecording(df.motion,
                             as.numeric(df.motion$quaternion[1,]),
                             1, nrow(df.motion$quaternion), FALSE)
x.helper <- c(x.helper, df.motion$coord$x)
y.helper <- c(y.helper, df.motion$coord$y)
z.helper <- c(z.helper, df.motion$coord$z)

pch.pch <- c(pch.pch, rep(15, length(df.motion$coord$x)))

xx = plot.in.3d(df.motion$coord$x,
                df.motion$coord$y,
                df.motion$coord$z, color.base = 'r',new.plot = FALSE)
color.color <- c(color.color, xx)
 length(xx)
 length(df.motion$coord$x)

library(scatterplot3d)
scatterplot3d(x.helper, z.helper, y.helper,
              color = color.color,
              xlab="X",
              ylab="Z",
              zlab="Y", #type='l'
              xlim = c(-1,1), ylim = c(-1,1), zlim = c(-1,1),
              angle = 20, scale = 0.2,
              main = "Omega left motions",
              pch=pch.pch
)


#############################################
#Plot 4
dir.path <- "e:\\Publikacje\\headmotionclassification\\github\\gotowe\\data\\"
motions <- c("clockwise", "counterclockwise",  "left", "node_head", "omega_left", "omega_right", "right")
motions.names <- c("Clockwise", "Counterclockwise",  "Left", "Node head", "Omega left", "Omega right", "right")
id <- 7
p.id <- 4
x.helper <- c()
y.helper <- c()
z.helper <- c()
color.color <- c()
color.all <- c("red", "green", "black","cyan", "magenta", "yellow", "blue",
               "pink", "violet", "purple", "brown", "navy")
pch.pch <- c()

#df.motion <- read.log.quaternion(paste("e:\\Publikacje\\headmotionclassification\\github\\gotowe\\dtw_classifiers\\1_",motions[id],".csv",sep=""))
df.motion <- read.log.quaternion(paste("e:\\Publikacje\\headmotionclassification\\github\\gotowe\\data\\", motions[id],"\\",p.id,"-02.log",sep="")) 
df.motion <- rotateRecording(df.motion,
                             as.numeric(df.motion$quaternion[1,]),
                             1, nrow(df.motion$quaternion), FALSE)
x.helper <- c(x.helper, df.motion$coord$x)
y.helper <- c(y.helper, df.motion$coord$y)
z.helper <- c(z.helper, df.motion$coord$z)

pch.pch <- c(pch.pch, rep(1, length(df.motion$coord$x)))

xx = plot.in.3d(df.motion$coord$x,
                df.motion$coord$y,
                df.motion$coord$z, color.base = 'r',new.plot = FALSE)

if (length(xx) != length(df.motion$coord$x))
{
  xx <- c(xx, xx[length(xx)])
}
color.color <- c(color.color, xx)

library(scatterplot3d)
scatterplot3d(x.helper, z.helper, y.helper,
              color = color.color,
              xlab="X",
              ylab="Z",
              zlab="Y", #type='l'
              xlim = c(-1,1), ylim = c(-1,1), zlim = c(-1,1),
              angle = 20, scale = 0.2,
              main = motions.names[id],
              pch=pch.pch
)


###################################################
#to wywaliæ
###################################################
path <- 'e:\\Publikacje\\headmotionclassification\\github\\'
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
    #save.log.quaternion(averaged.sig, paste('e:\\publikacje\\headmotionclassification\\r\\class_marcin\\',template.id,'_',motions[a],".csv",sep=''))
    #qqq <- read.log.quaternion(paste('e:\\publikacje\\headmotionclassification\\r\\class_marcin\\',template.id,'_',motions[a],".csv",sep=''))
    #template[[a]] <- qqq
  }
}

set.seed(1)
persons.id <- c(1,2,3,4,5,6,7,8,9,10,11,12)
dir.path <- "e:\\Publikacje\\headmotionclassification\\github\\gotowe\\data\\"
#my.path <- 'e:\\Publikacje\\headmotionclassification\\github\\gotowe\\dtw_classifiers\\'
classifiers.path <- 'e:\\Publikacje\\headmotionclassification\\github\\gotowe\\dtw_classifiers_single_classifier\\'
set1 <- c(2)
motions <- c("clockwise", "counterclockwise",  "left", "node_head", "omega_left", "omega_right", "right")
java.path <- "e:\\Publikacje\\headmotionclassification\\JAVA\\DTW\\dist\\"
dtw.results.path <- "e:\\Publikacje\\headmotionclassification\\github\\gotowe\\dtw_results_single_classifier\\"

set2<- persons.id
p2 <- list()
for (a in 1:length(motions))
{
  dir.path.help <- paste(dir.path, motions[a], sep = "")
  p2[[a]] <- read.data.from.folder.marcin(dir.path.help, set2)
}
all.time <- c()
for (a in 1:length(motions))
{
  print(motions[a])
  print(length(p2[[a]]))
  min <- nrow(p2[[a]][[1]]$coord)
  max <- nrow(p2[[a]][[1]]$coord)
  for (b in 2:length(p2[[a]]))
  {
    if (min < nrow(p2[[a]][[b]]$coord))
      min <- nrow(p2[[a]][[b]]$coord)
    if (max > nrow(p2[[a]][[b]]$coord))
      max <- nrow(p2[[a]][[b]]$coord)
    all.time <- c(all.time, (p2[[a]][[b]]$time[2:length(p2[[a]][[b]]$time)] - p2[[a]][[b]]$time[1:(length(p2[[a]][[b]]$time)-1)]))
  }
  print(paste(min, max))
}

for (b in 1:length(p2[[1]]))
{
  print(nrow(p2[[1]][[b]]$coord))
}

ave(all.time)
sd(all.time)