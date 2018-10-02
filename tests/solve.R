
library(mplib)
library(grid)

curves <- mpsolve(knot(1,1) + knot(2,2))
grid.bezier(curves[[1]][1:4, 1], curves[[1]][1:4, 2], default="in")

curves <- mpsolve(knot(1,1) + knot(2,2) + cycle())
grid.bezier(curves[[1]][1:4, 1], curves[[1]][1:4, 2], default="in")
grid.bezier(curves[[1]][5:8, 1], curves[[1]][5:8, 2], default="in")

curves <- mpsolve(knot(0,0) + dir(0) + dir(0) + knot(1, 1))
grid.bezier(curves[[1]][1:4, 1], curves[[1]][1:4, 2], default="in")
