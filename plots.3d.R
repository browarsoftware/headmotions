
plot.alignment.in.3d <- function(x1, y1, z1,
                                 x2, y2, z2,
                                 path1, path2,
                                 draw.text = FALSE)
{
  foo <- plot.in.3d(x1, y1, z1, radius = 0.01)
  foo <- plot.in.3d(x2, y2, z2, new.plot=FALSE, color.base = 'g', radius = 0.01)
  for (a in 1:length(path1))
  {
    lines3d(c(x1[best.res$path1[a]],
              x2[best.res$path2[a]]),
            c(y1[best.res$path1[a]],
              y2[best.res$path2[a]]),
            c(z1[best.res$path1[a]],
              z2[best.res$path2[a]]),
            color="green")
    rgl.texts((x1[best.res$path1[a]] + x2[best.res$path2[a]]) / 2,
              (y1[best.res$path1[a]] + y2[best.res$path2[a]]) / 2,
              (z1[best.res$path1[a]] + z2[best.res$path2[a]]) / 2,
              text = a, color = "white")
  }
}