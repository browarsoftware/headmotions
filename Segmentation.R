#Change the path
path <- 'e:\\Publikacje\\headmotionclassification\\github\\gotowe\\do wyslania\\'
source(paste(path, 'QuaternionProcessing.R', sep = ''))
source(paste(path, 'dtw.R', sep = ''))
source(paste(path, 'plots.3d.R', sep = ''))

filePath <- 'e:\\Publikacje\\headmotionclassification\\github\\gotowe\\do wyslania\\data\\segment.log'



df.helper <- read.csv(filePath, sep = ";", header = FALSE, stringsAsFactors = FALSE)
df.motion <- read.log.quaternion(filePath)


dist <- list()
for (a in 1:(length(df.motion$quaternion$x) - 1))
{
  dist[[a]] <- angleBetweenQuaternions(as.numeric(df.motion$quaternion[a,]), as.numeric(df.motion$quaternion[a+1,]))
}
dist <- unlist(dist) * 180 / pi
plot(log(dist+1), col="black", type='l', xlab = "Sample id", ylab = "log(alpha + 1)", main = "Head motion segmentation")


mmed <- function(x,k=11){runmed(x,k)}
filtered <- mmed(dist, k=15)
filtered[filtered < 1] <- 0
filtered[filtered > 1] <- 1
filtered <- mmed(filtered, k=15)
lines(filtered, type="l", col='red')

min.length <- 10
result <- list()

start <- 0
a <- 1
results.list <- list()
b <- 1
while (a < length(filtered) && start == 0)
{
  if (filtered[a] == 1)
    start = a
  a <- a + 1
}
if (start == 0)
{
  
} else
if (start == length(filtered))
{
  results.list[[b]] <- c(start, start)
  b <- b + 1
} else
{
  for (a in (start + 1):length(filtered))
  {
    if (filtered[a] == 0 && start == 0)
    {
      
    }
    else if (filtered[a] == 1 && start == 0)
      start <- a
    else if (filtered[start] == 1 && filtered[a] == 0)
    {
      results.list[[b]] <- c(start, a-1)
      start = 0
      b <- b + 1
    }
  }
}

results.list2 <- list()
c <- 1
for (a in 1:length(results.list))
{
  vec <- results.list[[a]]
  if (vec[2] - vec[1] > min.length)
  {
    results.list2[[c]] <- results.list[[a]]
    print(vec)
    c <- c + 1
  }
}


for (a in 1:length(results.list))
{
  vec <- results.list[[a]]
  if (vec[2] - vec[1] > 20)
  lines(x=c(vec[1],vec[1], vec[1], vec[2], vec[2], vec[2]), y=c(0,2,2,2,2,0),col="blue")
}



