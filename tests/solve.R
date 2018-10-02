
library(mplib)
library(grid)

curves <- mpsolve(knot(1,1) + knot(2,2))
grid.bezier(curves[[1]][1:4, 1], curves[[1]][1:4, 2], default="in")

curves <- mpsolve(knot(1,1) + knot(2,2) + cycle())
grid.bezier(curves[[1]][1:4, 1], curves[[1]][1:4, 2], default="in")
grid.bezier(curves[[1]][5:8, 1], curves[[1]][5:8, 2], default="in")

curves <- mpsolve(knot(0,0) + dir(0) + dir(0) + knot(1, 1))
grid.bezier(curves[[1]][1:4, 1], curves[[1]][1:4, 2], default="in")

grid.newpage()
p <- knot(0, 0) + knot(10, 10) + knot(10, -5) + cycle()
curves <- mpsolve(p)
pushViewport(viewport(x=unit(1, "in"), y=unit(1, "in"),
                      just=c("left", "bottom")))
grid.bezier(curves[[1]][1:4, 1], curves[[1]][1:4, 2], default="pt")
grid.bezier(curves[[1]][5:8, 1], curves[[1]][5:8, 2], default="pt")
grid.bezier(curves[[1]][9:12, 1], curves[[1]][9:12, 2], default="pt")

grid.newpage()
curves <- mpsolve(knot(0, 0) +
                  knot(2, 20) -
                  knot(10, 5) + cp(2, 2) + cp(9, 4.5) +
                  knot(3, 10) + tension(3) + tension(-4) +
                  knot(1, 14) + dir(2, 0) + dir(0, 1) +
                  knot(5, -4))
pushViewport(viewport(x=unit(1, "in"), y=unit(1, "in"),
                      just=c("left", "bottom")))
grid.bezier(curves[[1]][1:4, 1], curves[[1]][1:4, 2], default="pt")
grid.bezier(curves[[1]][5:8, 1], curves[[1]][5:8, 2], default="pt")
grid.bezier(curves[[1]][9:12, 1], curves[[1]][9:12, 2], default="pt")
grid.bezier(curves[[1]][13:16, 1], curves[[1]][13:16, 2], default="pt")
grid.bezier(curves[[1]][17:20, 1], curves[[1]][17:20, 2], default="pt")
