
library(mplib)
library(grid)

options(metapost.units="in")

curves <- mpsolve(knot(1,1) + knot(2,2))
grid.draw(curves)

curves <- mpsolve(knot(1,1) + knot(2,2) + cycle())
grid.draw(curves)

curves <- mpsolve(knot(0,0) + dir(0) + dir(0) + knot(1, 1))
grid.draw(curves)

options(metapost.units="pt")

grid.newpage()
p <- knot(0, 0) + knot(10, 10) + knot(10, -5) + cycle()
curves <- mpsolve(p)
pushViewport(viewport(x=unit(1, "in"), y=unit(1, "in"),
                      just=c("left", "bottom")))
grid.draw(curves)

grid.newpage()
curves <- mpsolve(knot(0, 0) +
                  knot(2, 20) -
                  knot(10, 5) + cp(2, 2) + cp(9, 4.5) +
                  knot(3, 10) + tension(3) + tension(-4) +
                  knot(1, 14) + dir(2, 0) + dir(0, 1) +
                  knot(5, -4))
pushViewport(viewport(x=unit(1, "in"), y=unit(1, "in"),
                      just=c("left", "bottom")))
grid.draw(curves)
