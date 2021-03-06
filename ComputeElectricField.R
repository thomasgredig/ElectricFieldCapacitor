#################################
# Numerically compute the electric
# field of a parallel plate capacitor
#
# Author: THomas Gredig
# Date: 2020-04-10
#################################

library(ggplot2)
source('func.R')
K = 9e9   # dielectric constant in vacuum
Q.line = 2e-9
Q = 8.8e-9
# Q = 1200*0.3*0.3/(4*pi*K)/0.2

# for LINE charge
#################
# source charge positions
sx = c(seq(from=-0.15, to=0.15, by=0.001),seq(from=-0.15, to=0.15, by=0.001))
l1 = length(sx)/2
sy = c(rep(0.2, l1), rep(0, l1))
# source charge magnitudes
dQ = c(rep(Q/l1,l1), rep(-Q/l1,l1))



d = data.frame( x=sx,y=sy,dQ)

d1 = data.frame()
for(oy in seq(from=0.02, to=0.18, by=0.001)) {
  ox=-0.05
  numericallyAddElectricField(dQ, sx, sy, ox, oy) -> E
  #numericallyAddElectricFieldSquare(dQ, sx, sy, sz, ox, oy) -> E
  d1 = rbind(d1, data.frame(x=ox, y=oy, dQ=0, Ex=E[1], Ey=E[2]))
}

d=rbind(d,d1[,1:3])
ggplot(d, aes(x,y,col=factor(dQ)))+
  geom_point()

ggplot(d1, aes(y, Ey)) +
  geom_line(col='red') +
  xlab('y-position (m)') +
  ylab(expression(paste('E'[y],' (V/m)'))) +
  theme_bw()
ggsave('Ey.png',width=4,height=3,dpi=200)

ggplot(d1, aes(y, Ex)) +
  geom_line(col='red') +
  xlab('y-position (m)') +
  ylab(expression(paste('E'[x],' (V/m)'))) +
  theme_bw()
ggsave('Ex.png',width=4,height=3,dpi=200)

# calculate the voltage
# V = - I Ey * dy
dV = sum(diff(d1$y)*d1$Ey[-1])
dV
# work
1.6e-19*dV
# for a line charge the work is:
# -7.321573e-17



# for a SQUARE charge
#####################
# source charge positions
sx = c(seq(from=-0.15, to=0.15, by=0.01),seq(from=-0.15, to=0.15, by=0.01))
l1 = length(sx)/2
sz = c(seq(from=-0.15, to=0.15, by=0.01),seq(from=-0.15, to=0.15, by=0.01))
l2 = length(sz)/2
sx = rep(sx,each=l2)
sz = rep(sz,times=l1)
l3 = length(sx)/2
sy = c(rep(0.2, l3), rep(0, l3))
# source charge magnitudes
dQ = c(rep(Q/l3,l3), rep(-Q/l3,l3))



d = data.frame( x=sx,y=sy,dQ)

d1 = data.frame()
for(oy in seq(from=0.18, to=0.02, by=-0.001)) {
  ox=-0.05
  numericallyAddElectricFieldSquare(dQ, sx, sy, sz, ox, oy) -> E
  d1 = rbind(d1, data.frame(x=ox, y=oy, dQ=0, Ex=E[1], Ey=E[2]))
}

d=rbind(d,d1[,1:3])
ggplot(d, aes(x,y,col=factor(dQ)))+
  geom_point()

ggplot(d1, aes(y, Ey)) +
  geom_line(col='red') +
  xlab('y-position (m)') +
  ylab(expression(paste('E'[y],' (V/m)'))) +
  theme_bw()
ggsave('Ey2.png',width=4,height=3,dpi=200)

ggplot(d1, aes(y, Ex)) +
  geom_line(col='red') +
  xlab('y-position (m)') +
  ylab(expression(paste('E'[x],' (V/m)'))) +
  theme_bw()
ggsave('Ex2.png',width=4,height=3,dpi=200)

# calculate the voltage
# V = - I Ey * dy
dV = sum(diff(d1$y)*d1$Ey[-1])
dV
# work
1.6e-19*dV
# for a square charge the work is:
# -




