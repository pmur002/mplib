
pathGrobs <- function(controls) {
    ncurves <- (nrow(controls) - 1) %/% 3
    curve <- vector("list", ncurves)
    for (i in 1:ncurves) {
        subset <- ((i-1)*3 + 1):(i*3 + 1)
        curve[[i]] <- bezierGrob(controls[subset, 1], controls[subset, 2],
                                 default.units="native")
    }
    gTree(children=do.call("gList", curve))
}

knotToMatrix <- function(x, len) {
    x$x <- convertX(x$x, "pt", valueOnly=TRUE)
    x$y <- convertX(x$y, "pt", valueOnly=TRUE)
    x <- lapply(x, rep, length=len)
    do.call(cbind, x)
}

pathToArray <- function(x) {
    len <- max(sapply(x$knots, length))
    knots <- lapply(x$knots, knotToMatrix, len)
    abind(knots, along=3)
}

mpsolve <- function(x) {
    if (!(inherits(x, "path") && inherits(x, "mpobj")))
        stop("'x' must be a MetaPost path")
    nPaths <- max(sapply(x$knots, length))
    nKnots <- length(x)
    controls <- solvePath(as.numeric(pathToArray(x)),
                          nKnots, nPaths)
    dim(controls) <- c(nKnots, 4, nPaths)
    controls
}
