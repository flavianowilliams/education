# clean up workspace

rm(list = ls())

# close all figure windows created with x11()

graphics.off()

library(tidyverse)

# initial values
df <- data.frame(color="red",  # color of the ball
                 cex=2,  # size of the ball
                 t=0,  # time position of this ball
                 nmax = 500,
                 itmax = 50,
                 lambda=1,  # wavenumber
                 freq=1,  # frequency
                 ym=2)  # amplitude máxima)

df$xlm = 4*df$lambda
df$dx = df$xlm/df$nmax
dt = df$xlm/df$itmax

# create a function, accepting in a data frame and counter
snapshot <- function(df, dt, ct, outdir = "frames") {
  # open PNG device
  png(filename = sprintf("%s/frame-%04d.png", outdir, ct), width = 960, height = 540)
  # remove any margin
  par(mar = c(0.5, 0.5, 0.5, 0.5))
  # create blank canvas
  plot(c(0, 0), type = "n", col = "white", xlim = c(0, df$xlm), 
       ylim = c(-10, 10), yaxt = "n", ann = FALSE, xaxt = "n", bty = "n")
  # add baseline calculate new position using projectile formula
  k = 2*pi/df$lambda
  om = 2*pi*df$freq
  x0 = dt*ct
  xpos = c(rep(0,df$nmax))
  ypos = c(rep(0,df$nmax))
  for (i in 1:df$nmax) {
    xpos[i] = df$dx*i
    ypos[i] = df$ym*exp(-0.5*((xpos[i]-x0)/(0.1))**2)
  }
  # draw the point(s)
  yp = df$ym
  points(xpos, ypos, type = "p", cex = 1, pch = 16, col = "black")
  points(x0, yp, type = "p", cex = df$cex, pch = 16, col = df$color)
  dev.off()
  df
}

for (i in seq(df$itmax)) {
  snapshot(df, dt, i, outdir = "frames")
  # could put some status messages in here, this may take some time.
}

system("convert -delay 10 -loop 0 frames/*.png pulso.gif")