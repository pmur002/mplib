
knotToMatrix <- function(x, len) {
    x$x <- convertX(x$x, "pt", valueOnly=TRUE)
    x$y <- convertX(x$y, "pt", valueOnly=TRUE)
    if (any(is.na(x$x) | is.na(x$y)))
        stop("Knot locations cannot be missing values")
    x$cp.left.x <- convertX(x$cp.left.x, "pt", valueOnly=TRUE)
    x$cp.left.y <- convertY(x$cp.left.y, "pt", valueOnly=TRUE)
    x$cp.right.x <- convertX(x$cp.right.x, "pt", valueOnly=TRUE)
    x$cp.right.y <- convertY(x$cp.right.y, "pt", valueOnly=TRUE)
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

pathGrobs <- function(x, pathIndex, cycle) {
    pts <- matrix(x, byrow=TRUE, ncol=2)
    ncurves <- nrow(pts) %/% 4
    if (!cycle) {
        ncurves <- ncurves - 1
        pts <- pts[1:(ncurves*4), ]
    }
    bezierGrob(pts[, 1], pts[, 2], id=rep(1:ncurves, each=4),
               default.units="pt",
               name=paste0("path-", pathIndex))
}

mpsolve <- function(x, name=NULL) {
    if (!(inherits(x, "path") && inherits(x, "mpobj")))
        stop("'x' must be a MetaPost path")
    nPaths <- max(sapply(x$knots, length))
    nKnots <- length(x)
    cycle <- inherits(x$knots[[nKnots]], "cycle")
    if (cycle) {
        x$knots <- x$knots[-nKnots]
        nKnots <- nKnots - 1
    }
    paths <- pathList(x, nPaths)
    controls <- lapply(paths, solvePath, nKnots, cycle)
    gTree(children=do.call("gList",
                           mapply(pathGrobs, controls, 1:nPaths,
                                  MoreArgs=list(cycle), SIMPLIFY=FALSE)),
          name=name)
}
