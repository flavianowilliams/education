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
                 lambda=1,  # wavenumber
                 freq=1,  # frequency
                 ym=2,  # amplitude máxima
                 dt=0.1)

df$xlm = 4*df$lambda
df$dx = df$xlm/df$nmax

# create a function, accepting in a data frame and counter
snapshot <- function(df, ct, outdir = "frames") {
  # open PNG device
  png(filename = sprintf("%s/frame-%04d.png", outdir, ct), width = 960, height = 270)
  # remove any margin
  par(mar = c(0, 0, 0, 0))
  # create blank canvas
  plot(c(0, 0), type = "n", col = "white", xlim = c(-0.25, df$xlm), 
       ylim = c(-10, 10), yaxt = "n", ann = FALSE, xaxt = "n", bty = "n")
  text(-0.25, 0, "Direção de oscilação", srt = 90)
  arrows(-0.15, -df$ym, -0.15, df$ym, length = 0.25, angle = 30, code = 3, col = par("fg"), lty = par("lty"),
         lwd = par("lwd"))
  # add baseline calculate new position using projectile formula
  k = 2*pi/df$lambda
  om = 2*pi*df$freq
  t = df$dt*ct
  # mcu
  #xpos0 = c(rep(0,df$nmax))
  #ypos0 = c(rep(0,df$nmax))
  #for (i in 1:df$nmax) {
  #  xpos0[i] <- df$ym*cos(om*t)
  #  ypos0[i] <- df$ym*sin(om*t)
  #}
  #points(xpos0, ypos0, type = "p", cex = 1, pch = 16, col = "blue")
  # movimento vertical
  xp = 0
  yp = df$ym*cos(k*xp-om*t)
  points(xp, yp, type = "p", cex = df$cex, pch = 16, col = df$color)
  # onda progressiva
  xpos = c(rep(0,df$nmax))
  ypos = c(rep(0,df$nmax))
  for (i in 1:df$nmax) {
    xpos[i] = df$dx*i
    ypos[i] <- df$ym*cos(k*xpos[i]-om*t)
  }
  points(xpos, ypos, type = "p", cex = 1, pch = 16, col = "black")
  dev.off()
  df
}

for (i in seq(280)) {
  df <- snapshot(df, i, outdir = "frames")
  # could put some status messages in here, this may take some time.
}

system("convert -delay 10 -loop 0 frames/*.png wave.gif")
