
pathGrob <- function(controls, pathIndex=1) {
    BezierGrob(controls$x, controls$y,
               open=is.null(attr(controls, "cycle")),
               default.units="pt",
               name=paste0("path-", pathIndex))
}


makeContent.mplibgrob <- function(x) {
    pathControls <- mpsolve(x$path)
    paths <- mapply(pathGrob, pathControls, 1:length(pathControls),
                    SIMPLIFY=FALSE)
    setChildren(x, do.call(gList, paths))
}

mplibGrob <- function(x, ...) {
    UseMethod("mplibGrob")
}

## A solved path (scale already fixed)
mplibGrob.mpcontrols <- function(x,
                                    gp=gpar(),
                                    name=NULL) {
    path <- pathGrob(x)
    gTree(children=do.call(gList, path),
          gp=gp, name=name, cl="mpsolvedgrob")    
}

## Several solved paths (scale already fixed)
mplibGrob.mpcontrolList <- function(x,
                                    gp=gpar(),
                                    name=NULL) {
    paths <- mapply(pathGrob, x, 1:length(x), SIMPLIFY=FALSE)
    gTree(children=do.call(gList, paths),
          gp=gp, name=name, cl="mpsolvedgrob")    
}

## An unsolved path
mplibGrob.mppath <- function(x,
                             gp=gpar(),
                             name=NULL) {
    gTree(path=x, gp=gp, name=name, cl="mplibgrob")
}

grid.mplib <- function(...) {
    grid.draw(mplibGrob(...))
}
