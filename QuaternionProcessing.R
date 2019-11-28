##
# x - pitch, y- yaw, z - roll
##

plot.in.3d <- function(x,y,z, color=NULL, use.spheres = FALSE, color.base = 'r', new.plot=TRUE, lim = c(-1,1), radius = 1)
{
  require(rgl)
  plot.type = 'p'
  if (use.spheres)
	plot.type = 's'
  if (new.plot == TRUE) 
  {
    rgl.open()
  }
  if (is.null(color))
  {
    color <- as.hexmode(floor(seq(from = 64, to = 254,by = ((255-64)/(length(x))))))
	if (color.base == 'r')
		color <- paste("#", color, "0000", sep = "")
    if (color.base == 'g')
		color <- paste("#00", color, "00", sep = "")
	if (color.base == 'b')
		color <- paste("#0000", color, sep = "")
	
    plot3d(x, y, z, 
           xlab = "X", ylab = "Y", zlab = "Z",
           col = color, add=(!new.plot),
           xlim = lim,ylim = lim,zlim = lim, radius = radius, type = plot.type)
  } else
  {
	plot3d(x, y, z, 
             xlab = "X", ylab = "Y", zlab = "Z",
             col = color, add=(!new.plot),
             xlim = lim,ylim = lim,zlim = lim, radius = radius, type = plot.type)
    #if (class(color) == 'hexmode')
    #{
    #  plot3d(x, y, z, 
    #         xlab = "X", ylab = "Y", zlab = "Z",
    #         col = paste("#", color, color, "00", sep = ""), add=(!new.plot),
    #         xlim = lim,ylim = lim,zlim = lim)
    #} else {
    #  plot3d(x, y, z, 
    #         xlab = "X", ylab = "Y", zlab = "Z",
    #         col = 'green', add=(!new.plot),
    #         xlim = lim,ylim = lim,zlim = lim)
    #}
  }
  return (color)
}


##########################################################################################

mulleft <- function(this, other)
{
  newX = other[4] * this[1] + other[1] * this[4] + other[2] * this[3] - other[3] * this[2]
  newY = other[4] * this[2] + other[2] * this[4] + other[3] * this[1] - other[1] * this[3]
  newZ = other[4] * this[3] + other[3] * this[4] + other[1] * this[2] - other[2] * this[1]
  newW = other[4] * this[4] - other[1] * this[1] - other[2] * this[2] - other[3] * this[3]
  return (c(newX,newY,newZ,newW))
}


transform <- function(x,y,z,w,vx,vy,vz)
{
  tmp2 = c(-x, -y, -z, w)
  tmp1 = c(vx, vy, vz, 0)
  rr = mulleft(mulleft(tmp2, tmp1),c(x,y,z,w))
  return (c(rr[1],rr[2],rr[3]))
}

inverse <- function(q) {
  d = sum(q*q);
  return (c(-q[1]/d, -q[2]/d, -q[3]/d, q[4]/d))
}



read.log.quaternion <- function(path.to.file, quat.to.rotate = c(0))
{
  df <- read.csv(path.to.file, sep = ";", header = FALSE, stringsAsFactors = FALSE)
  df[df$V1 == 'q',3]
  library(RSpincalc)
  dfq <- df[df$V1 == 'q',3]
  dfv <- df[df$V1 == 'v',3]
  xx <- list()
  yy <- list()
  zz <- list()
  
  xq <- list()
  yq <- list()
  zq <- list()
  wq <- list()
  
  xv <- list()
  yv <- list()
  zv <- list()
  
  time <- as.numeric(df[df$V1 == 'q',2])
  
  #V <- c(-1,0,0)
  #V <- c(0,1,0)
  vec = c(0,0,-1)
  #3, 2, 1, 4
  for (a in 1:length(dfq))
  {
    #x y z w
    #y,-z,-w,-x
    quat <- as.numeric(unlist(strsplit(dfq[a], ",")))
    
    
    if (length(quat.to.rotate) > 1)
    {
      quat <- mulleft(quat, quat.to.rotate)
      xx <- getPitchRad(quat)
      yy <- getYawRad(quat)
      zz <- getRollRad(quat)
      euler <- c(xx, yy, zz)
    } else 
    {
        euler <- as.numeric(unlist(strsplit(dfv[a], ",")))
    }
    Vrot <- transform(quat[1], quat[2], quat[3], quat[4],
                      vec[1], vec[2], vec[3])
    
    xx[[a]] <- Vrot[1]
    yy[[a]] <- Vrot[2]
    zz[[a]] <- Vrot[3]
    
    xq[[a]] <- quat[1]
    yq[[a]] <- quat[2]
    zq[[a]] <- quat[3]
    wq[[a]] <- quat[4]
    
    #q <- c(quat[4], quat[3], quat[2], quat[1])
    #v <- Q2EA(q,'zyx')
    #v <- df.motion$euler.angles[a,]
    
    #xv[[a]] <- v[1]
    #yv[[a]] <- v[2]
    #zv[[a]] <- v[3]
    
    
    xv[[a]] <- euler[1]
    yv[[a]] <- euler[2]
    zv[[a]] <- euler[3]
    
  }
  return (list(time = time, coord = data.frame(x = unlist(xx), y = unlist(yy), z = unlist(zz)),
               quaternion = data.frame(x = unlist(xq), y = unlist(yq), z = unlist(zq), w = unlist(wq)),
               euler.angles = data.frame(x = unlist(xv), y = unlist(yv), z = unlist(zv))))
  
}



mul <- function(a, b) 
{
  y1 = a[1]*b[1] - a[2]*b[2] - a[3]*b[3] - a[4]*b[4]
  y2 = a[1]*b[2] + a[2]*b[1] + a[3]*b[4] - a[4]*b[3]
  y3 = a[1]*b[3] - a[2]*b[4] + a[3]*b[1] + a[4]*b[2]
  y4 = a[1]*b[4] + a[2]*b[3] - a[3]*b[2] + a[4]*b[1]
  return (c(y1, y2, y3, y4))
}

save.log.quaternion <- function(motion.df, path.to.file, from = 1, to = -1)
{
  #path.to.file <- 'e:\\Publikacje\\headmotionclassification\\test\\aaa.log'
  #motion.df <- df.motion
  fileConn<-file(path.to.file)
  #writeLines(c("Hello","World"), fileConn)
  allLines <- list()
  if (to == -1)
    to = length(motion.df$time)
  if (to > length(motion.df$time))
    to = length(motion.df$time)
  if (length(motion.df$time) == 0)
    return (0)
  if (from > length(motion.df$time))
    return (0)
  if (from > to)
    return (0)
  for (a in from:to)
  {
    line <- paste('q;',motion.df$time[a], ";", sep='')
    #for (b in 1:4)
    line <- paste(line, motion.df$quaternion[a,1], ',',
                  motion.df$quaternion[a,2], ',',
                  motion.df$quaternion[a,3], ',',
                  motion.df$quaternion[a,4], ',',
                  sep='')
    #writeLines(line, fileConn)
    allLines[[2*a - 1]] <- line
    
    line <- paste('v;',motion.df$time[a], ";", sep='')
    
    qq <- as.numeric(motion.df$quaternion[a,])
    
    xx <- getPitchRad(qq)
    yy <- getYawRad(qq)
    zz <- getRollRad(qq)
    #for (b in 1:3)
    line <- paste(line,xx,',',yy,',',zz, sep='')
    #writeLines(line, fileConn)
    allLines[[2*a]] <- line
  }
  writeLines(unlist(allLines), fileConn)
  close(fileConn)
}


############################################################

getGimbalPole <- function(q) {
  #t = y * x + z * w;
  #return t > 0.499f ? 1 : (t < -0.499f ? -1 : 0);
  t = q[2] * q[1] + q[3] * q[4];
  if (t > 0.499)
    return (1)
  if (t < -0.499)
    return (-1)
  return (0)
  #return t > 0.499f ? 1 : (t < -0.499f ? -1 : 0)
}

getRollRad <- function(q)
{
  pole = getGimbalPole(q)
  if (pole == 0)
  {
    return (atan2(2.0 * (q[4] * q[3] + q[2] * q[1]), 1.0 - 2.0 * (q[1] * q[1] + q[3] * q[3])))
  } else
  {
    return (pole * 2.0 * atan2(q[3], q[4]))
  }
}




#final int pole = getGimbalPole();
#return pole == 0 ? MathUtils.atan2(2f * (w * z + y * x), 1f - 2f * (x * x + z * z)) : (double)pole * 2f
#* MathUtils.atan2(y, w);


#/** Get the pitch euler angle in radians, which is the rotation around the x axis. Requires that this quaternion is normalized.
#* @return the rotation around the x axis in radians (between -(PI/2) and +(PI/2)) */
#  public double getPitchRad () {
#    final int pole = getGimbalPole();
#    return pole == 0 ? (double)Math.asin(MathUtils.clamp(2f * (w * x - z * y), -1f, 1f)) : (double)pole * MathUtils.PI * 0.5f;
#  }

clamp <- function (value, min, max) {
  if (value < min) return (min)
  if (value > max) return (max)
  return (value)
}

getPitchRad <- function(q)
{
  pole = getGimbalPole(q)
  if (pole == 0)
  {
    return(asin(clamp(2.0 * (q[4] * q[1] - q[3] * q[2]), -1.0, 1.0)))
  }
  else
  {
    return (pole * pi * 0.5)
  }
}

#/** Get the yaw euler angle in radians, which is the rotation around the y axis. Requires that this quaternion is normalized.
#* @return the rotation around the y axis in radians (between -PI and +PI) */
#  public double getYawRad () {
#    return getGimbalPole() == 0 ? MathUtils.atan2(2f * (y * w + x * z), 1f - 2f * (y * y + x * x)) : 0f;
#  }

getYawRad <- function(q)
{
  if (getGimbalPole(q) == 0)
  {
    return (atan2(2.0 * (q[2] * q[4] + q[1] * q[3]), 1.0 - 2.0 * (q[2] * q[2] + q[1] * q[1])))
  } else
  {
    return (0)
  }
}

quaternionConjugate <- function(q)
{
  return (c(-q[1],-q[2],-q[3],q[4]))
}

rotateVectorByQuaternion <- function(q, v)
{
  qr <- mul(mul(q, c(v,0)),quaternionConjugate(q))
  return(qr[1:3])
}

rotateRecording <- function(signal, quat, from, to, only.euler.angles = TRUE)
{
  signal.helper <- signal
  for (a in from:to)
  {
    #a <- 1
    qq <- 
      mulleft(inverse(quat),
              as.numeric(signal.helper$quaternion[a,]))
    signal.helper$euler.angles$x[a] <- getPitchRad(qq)
    signal.helper$euler.angles$y[a] <- getYawRad(qq)
    signal.helper$euler.angles$z[a] <- getRollRad(qq)
    if(!only.euler.angles)
    {
      vec = c(0,0,-1)
      #3, 2, 1, 4
      
      #x y z w
      #y,-z,-w,-x
      vec = c(0,0,-1)
      Vrot <- transform(qq[1], qq[2], qq[3], qq[4],
                        vec[1], vec[2], vec[3])
      signal.helper$quaternion$x[a] <- qq[1]
      signal.helper$quaternion$y[a] <- qq[2]
      signal.helper$quaternion$z[a] <- qq[3]
      signal.helper$quaternion$w[a] <- qq[4]
      
      signal.helper$coord$x[a] <- Vrot[1]
      signal.helper$coord$y[a] <- Vrot[2]
      signal.helper$coord$z[a] <- Vrot[3]
      
    }
  }
  return (signal.helper)
}

######################################

EulerAnglesToQuaternion <- function(yaw, pitch, roll)
{
  hr = roll * 0.5
  shr = sin(hr)
  chr = cos(hr)
  hp = pitch * 0.5
  shp = sin(hp)
  chp = cos(hp)
  hy = yaw * 0.5
  shy = sin(hy)
  chy = cos(hy)
  chy_shp = chy * shp
  shy_chp = shy * chp
  chy_chp = chy * chp
  shy_shp = shy * shp
  
  x = (chy_shp * chr) + (shy_chp * shr)
  y = (shy_chp * chr) - (chy_shp * shr)
  z = (chy_chp * shr) - (shy_shp * chr)
  w = (chy_chp * chr) + (shy_shp * shr)
  return (c(x,y,z,w))
}


normalizeQuaternion <- function(q)
{
  div <- sqrt(sum(q * q))
  q <- q / div
  return(q)
}


angleBetweenQuaternions <- function(q1, q2)
{
  q1n <- normalizeQuaternion(q1)
  q2n <- normalizeQuaternion(q2)
  ang <- 2 * acos(mulleft(q1n, inverse(q2n))[4])
  if (ang > pi) ang <- 2*pi - ang
  return (ang)
}


get.paths.from.dir <- function(dir.name)
{
  files <- list.files(dir.name)
  persons.id <- c()
  paths <- c()
  for (a in 1:length(files))
  {
    persons.id <- c(persons.id, strsplit(files[a], "-")[[1]][1])
  }
  for (a in 1:length(files))
  {
    paths <- c(paths, paste(dir.name, "\\", files[a], sep=''))
  }
  return (list(persons.id=persons.id, paths = paths))
}


load.data.from.dir <- function(dir.name)
{
  files <- list.files(dir.name)
  persons.id <- c()
  all.data.list <- list()
  for (a in 1:length(files))
  {
    persons.id <- c(persons.id, strsplit(files[a], "-")[[1]][1])
  }
  for (a in 1:length(files))
  {
    df.motion <- read.log.quaternion(paste(dir.name, "\\", files[a], sep=''))
    df.motion <- rotateRecording(df.motion,
                                 as.numeric(df.motion$quaternion[1,]),
                                 1, nrow(df.motion$quaternion), FALSE)
    
    
    ll <- length(all.data.list)
    all.data.list[[ll + 1]] <- df.motion
  }
  return (list(persons.id=persons.id, all.data.list = all.data.list))
}


make.sample <- function(user.data, size)
{
  sample.id <- sample(length(user.data$persons.id), size)
  return (list(persons.id=user.data$persons.id[sample.id], all.data.list = user.data$all.data.list[sample.id]))
}

Qlerp <- function(Q1, Q2, fracT)
{
  N <- dim(Q1)[1]
  if (fracT == 0.0) return (Qzero(N))
  if (fracT == 1.0) return (Qone(N))
  return (normalizeQuaternion(Q1*(1-fracT) + Q2*fracT))
}

Qlerp<-function(Q1, Q2, fracT)
{#  linear quaternion interpolation
  # http://www.lfd.uci.edu/~gohlke/code/transformations.py.html
  N <- dim(Q1)[1]
  if (fracT == 0.0) return (Qzero(N))
  if (fracT == 1.0) return (Qone(N))
  qn <- normalizeQuaternion(Q1*(1-fracT) + Q2*fracT)
  return (qn)
}

Qslerp<-function(Q1, Q2, fracT)
{# spherical linear interpolation
  # http://www.lfd.uci.edu/~gohlke/code/transformations.py.html
  N <- dim(Q1)[1]
  if (fracT == 0.0) return (Qzero(N))
  if (fracT == 1.0) return (Qone(N))
  Q3 <- Q2
  Qd <- Q1 %Q.% Q2
  QdLtz <- which(Qd < 0)
  Qd[QdLtz] <- -Qd[QdLtz]
  Q3[QdLtz] <- -Q2[QdLtz]
  QdLt.95 <- which(Qd < 0.95)
  angleQ = acos(Qd[QdLt.95])
  Q3[QdLt.95] <- (Q1[QdLt.95] * sin(angleQ * (1-fracT)) + Q3[QdLt.95] * sin(angleQ * fracT))/sin(angleQ)
  Q3[-QdLt.95] <- Qlerp(Q1[QdLt.95], Q3[QdLt.95],fracT)
  Q3
}


signal.from.quaternion <- function(quat)
{
  euler.angles <- data.frame(x = rep(0, nrow(quat)), y = rep(0, nrow(quat)), z = rep(0, nrow(quat)))
  coord <-  data.frame(x = rep(0, nrow(quat)), y = rep(0, nrow(quat)), z = rep(0, nrow(quat)))
  time <- 1:nrow(quat)
  quat.helper <- quat
  for (a in 1:nrow(quat))
  {
    qq <- as.numeric(quat.helper[a,])
    vec = c(0,0,-1)
    #3, 2, 1, 4
    euler.angles$x[a] <- getPitchRad(qq)
    euler.angles$y[a] <- getYawRad(qq)
    euler.angles$z[a] <- getRollRad(qq)
    #x y z w
    #y,-z,-w,-x
    vec = c(0,0,-1)
    
    Vrot <- transform(qq[1], qq[2], qq[3], qq[4],
                      vec[1], vec[2], vec[3])

    coord$x[a] <- Vrot[1]
    coord$y[a] <- Vrot[2]
    coord$z[a] <- Vrot[3]
  }
  return (list(time = time, coord = coord, quaternion = quat.helper, euler.angles = euler.angles))
}


interpolate.df <- function(vv1, new.signal.length = 40)
{
  old.signal.length <- nrow(vv1$quaternion)
  step <- old.signal.length / new.signal.length
  new.data.frame <- data.frame(x = rep(0, new.signal.length), y = rep(0, new.signal.length), 
                               z = rep(0, new.signal.length), w = rep(0, new.signal.length))
  
  start.id <- 1
  for (a in 1:new.signal.length)
  {
    end.id <- start.id + step
    end.id.ceiling <- ceiling(end.id)
    if (end.id.ceiling > old.signal.length)
      end.id.ceiling <- old.signal.length
    start.id.calc <- end.id.ceiling -1
    if (start.id.calc < 1)
      start.id.calc <- 1
    
    q1 <- vv1$quaternion[start.id.calc,]
    q2 <- vv1$quaternion[end.id.ceiling,]
    fracT <- end.id.ceiling - end.id
    
    new.data.frame[a,] <- Qlerp(q1, q2, fracT)
    start.id <- end.id
  }  
  sss <- signal.from.quaternion(new.data.frame)
  return (sss)
}


interpolate.signal <- function(vv1, new.signal.length = 40)
{
  old.signal.length <- nrow(vv1)
  step <- old.signal.length / new.signal.length
  new.data.frame <- data.frame(x = rep(0, new.signal.length), y = rep(0, new.signal.length), 
                               z = rep(0, new.signal.length), w = rep(0, new.signal.length))
  
  start.id <- 1
  for (a in 1:new.signal.length)
  {
    end.id <- start.id + step
    end.id.ceiling <- ceiling(end.id)
    if (end.id.ceiling > old.signal.length)
      end.id.ceiling <- old.signal.length
    start.id.calc <- end.id.ceiling -1
    if (start.id.calc < 1)
      start.id.calc <- 1
    
    q1 <- vv1[start.id.calc,]
    q2 <- vv1[end.id.ceiling,]
    fracT <- end.id.ceiling - end.id
    
    new.data.frame[a,] <- Qlerp(q1, q2, fracT)
    start.id <- end.id
  }  
  return (new.data.frame)
}

################################
#


transformQuaternion <-function(q1, yaw, pitch, roll)
{
  q2 <- EulerAnglesToQuaternion(yaw, pitch, roll)
  q3 <- mulleft(q1, q2)
  return (q3)
}

transformRecording <- function(rec, yaw = 0, pitch = 0, roll = 0)
{
  #rec <- p1[[a]][[b]]
  #yaw <- 0
  #pitch <- 0
  #roll <- 0
  
  rec.copy <- rec
  vec = c(0,0,-1)
  #3, 2, 1, 4
  for (a in 1:nrow(rec.copy$quaternion))
  {
    #x y z w
    #y,-z,-w,-x
    #quat <- as.numeric(unlist(strsplit(dfq[a], ",")))
    #euler <- as.numeric(unlist(strsplit(dfv[a], ",")))
    q1 <- rec.copy$quaternion[a,]
    q1 <- transformQuaternion(q1,yaw, pitch, roll)
    
    Vrot <- transform(as.numeric(q1[1]), 
                      as.numeric(q1[2]), 
                      as.numeric(q1[3]), 
                      as.numeric(q1[4]),
                      vec[1], vec[2], vec[3])
    rec.copy$quaternion[a,] <- q1
    rec.copy$coord[a,] <- Vrot
  }
  return (rec.copy)
}