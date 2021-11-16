# clean up workspace

rm(list = ls())

# close all figure windows created with x11()

graphics.off()

library(tidyverse)
library(ggplot2)
library(gganimate)

dxpos = 0.1
dt = 0.1
ym = 1
lambda = 20
freq = 1
k = 2*pi/lambda
om = 2*pi*freq

wave = data.frame(xpos = c(seq(0,10,dxpos)), tpos = rep(c(seq(0,10,dxpos))))

head(wave)

yxt = function(x, t, ym, k, om) y = ym*cos(k*x-om*t)

wave = wave %>%
  mutate(ypos = yxt(xpos, dt, ym, k, om))

ggplot(data = wave, aes(x = xpos, y = ypos))+
  geom_line()
