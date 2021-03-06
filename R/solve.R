
knotToMatrix <- function(x, len) {
    x$x <- convertX(x$x, "bigpts", valueOnly=TRUE)
    x$y <- convertX(x$y, "bigpts", valueOnly=TRUE)
    if (any(is.na(x$x) | is.na(x$y)))
        stop("Knot locations cannot be missing values")
    x$cp.left.x <- convertX(x$cp.left.x, "bigpts", valueOnly=TRUE)
    x$cp.left.y <- convertY(x$cp.left.y, "bigpts", valueOnly=TRUE)
    x$cp.right.x <- convertX(x$cp.right.x, "bigpts", valueOnly=TRUE)
    x$cp.right.y <- convertY(x$cp.right.y, "bigpts", valueOnly=TRUE)
    x <- lapply(x, rep, length=len)
    do.call(cbind, x)
}

pathList <- function(x, n) {
    len <- max(sapply(x$knots, length))
    knotList <- lapply(x$knots, knotToMatrix, len)
    lapply(1:n,
           function(i) {
               do.call(rbind, lapply(knotList, function(k) k[i,]))
           })
}

tidyResult <- function(x, cycle) {
    if (is.null(x)) {
        warning("mplib failed")
    }
    pts <- matrix(x, byrow=TRUE, ncol=2)
    ncurves <- nrow(pts) %/% 4
    if (!cycle) {
        ncurves <- ncurves - 1
        pts <- pts[1:(ncurves*4), ]
    }
    if (ncurves > 1) {
        subset <- -seq(5, ncurves*4, by=4)
    } else {
        subset <- 1:4
    }
    controls <- list(x=pts[subset, 1], y=pts[subset, 2])
    class(controls) <- "mpcontrols"
    controls
}

mpsolve <- function(x) {
    if (!(inherits(x, "mppath")))
        stop("'x' must be a MetaPost path")
    nPaths <- max(sapply(x$knots, length))
    nKnots <- length(x)
    cycle <- inherits(x$knots[[nKnots]], "cycle")
    if (cycle) {
        x$knots <- x$knots[-nKnots]
        nKnots <- nKnots - 1
    }
    paths <- pathList(x, nPaths)
    solvedPaths <- lapply(paths, solvePath, nKnots, cycle)
    pathControls <- lapply(solvedPaths, tidyResult, cycle)
    class(pathControls) <- "mpcontrolList"
    pathControls
}
