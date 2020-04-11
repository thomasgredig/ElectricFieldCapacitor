# Functions

# (sx,sy): vectors
# ox,oy: single position
#
# For Line Charge
numericallyAddElectricField <- function(dQ,sx,sy,ox,oy) {
  Ex = 0
  Ey = 0
  for(i in 1:length(sx)){
    rx = ox-sx[i]
    #print(rx)
    ry = oy-sy[i]
    r = sqrt(rx^2+ry^2)
    Ex = Ex + K*dQ[i]/(r^2)*(rx/r)
    Ey = Ey + K*dQ[i]/(r^2)*(ry/r)
  }
  c(Ex,Ey)
}


# For a Square Charge
numericallyAddElectricFieldSquare <- function(dQ,sx,sy,sz,ox,oy) {
  Ex = 0
  Ey = 0
  for(i in 1:length(sx)){
    rx = ox-sx[i]
    #print(rx)
    ry = oy-sy[i]
    rz = 0-sz[i]
    r = sqrt(rx^2+ry^2+rz^2)
    Ex = Ex + K*dQ[i]/(r^2)*(rx/r)
    Ey = Ey + K*dQ[i]/(r^2)*(ry/r)
  }
  c(Ex,Ey)
}
