# http://www.ncdm.uic.edu/publications/files/proc-094.pdf
# R code written by Simon Urbanek

## update this list when scagnostics are added to the Java code!
.scagnostics.names <- c("Outlying", "Skewed", "Clumpy", "Sparse", "Striated", "Convex", "Skinny", "Stringy", "Monotonic")

scagnostics <- function(x, ...) UseMethod("scagnostics", x)

## calls scagnostics Java code for a given data which must be a Java reference of the class "[[D"
.scagnostics.for.data <- function(data, bins, maxBins, names=NULL) {
  results <- .jcall("scagnostics/Main","[[D","computeScagnostics",data,as.integer(bins)[1],as.integer(maxBins)[1])
  r <- lapply(results, .jevalArray)
  n <- length(r)
  s <- length(r[[1]])
  if (n == 1) {
    r <- r[[1]]
    names(r) <- .scagnostics.names
  } else {
    r <- matrix(unlist(r), s)
    rownames(r) <- .scagnostics.names
    if (!is.null(names)) {
      dn <- length(names)
      l <- data.frame(x=rep(1:dn,dn),y=rep(1:dn,each=dn)) # full grid
      l <- l[l$x < l$y,] # collapse to those satisfying x < y
      colnames(r) <- paste(names[l$x], "*", names[l$y])
    }
  }
  class(r) <- "scagnostics"
  r
}

## --- scagnostics methods for different data types ---

scagnostics.default <- function(x, y, bins=50, maxBins=1000, ...) {
  if (length(x) != length(y)) stop("x and y must have the same length")

  complete <- !is.na(x) & !is.na(y)  
  x <- as.double(x[complete])
  y <- as.double(y[complete])
  if (length(x) < 3) stop("Need at least three valid cases")
  if (diff(range(x)) == 0) stop("x is constant")
  if (diff(range(y)) == 0) stop("y is constant")
  data <- .jarray(list(.jarray(x), .jarray(y)), "[D")
  .scagnostics.for.data(data, bins, maxBins)
}

scagnostics.data.frame <- function(x, bins=50, maxBins=1000, ...) {
    if (ncol(x) < 1) stop("need at least two variables")
    if (nrow(x) < 3) stop("need at least three cases")
    scagnostics.list(x, bins, maxBins, ...)
}

scagnostics.list <- function(x, bins=50, maxBins=1000, ...) {
    if (length(x) < 2) stop("need at least two variables")
    n <- lengths(x)
    if (!all(n == n[1])) stop("all variables must have the same length")
    if (is.null(names(x))) names(x) <- paste0("V", seq.int(length(x)))
    cj <- lapply(x, function(x) {
        dv <- as.double(x)
        if (all(is.na(dv)) || (diff(range(dv, na.rm=TRUE)) == 0))
            NULL
        else
            .jarray(dv)
    })
    nils <- sapply(cj, is.null)
    if (any(nils)) {
        warning("Following variables are constant or non-numeric and will be removed: ",
                paste(names(cj)[nils], collapse=", "))
        cj <- cj[!nils]
    }
    data <- .jarray(cj, "[D")
    .scagnostics.for.data(data, bins, maxBins, names(cj))
}

scagnostics.matrix <- function(x, bins=50, maxBins=1000, ...) {
    if (dim(x)[2] < 1) stop("need at least two variables")
    if (!is.numeric(x)) stop("the matrix must be numeric")
    if (is.null(colnames(x))) colnames(x) <- paste0("V", seq.int(ncol(x)))
    nils <- apply(x, 2, function(o) all(is.na(o)) || (diff(range(o, na.rm=TRUE)) == 0))
    if (any(nils)) {
        warning("Following variables are constant and will be removed: ",
                paste(colnames(x)[nils], collapse=", "))
        x <- x[,!nils]
    }
    data <- .jarray(lapply(1:ncol(x), function(i) .jarray(as.double(x[,i]))), "[D")
    nam <- colnames(x)
    .scagnostics.for.data(data, bins, maxBins, nam)
}

## --- outliers

scagnosticsOutliers <- function(scagnostics) {
  if (!inherits(scagnostics, "scagnostics")) stop("scagnostics must be of the class `scagnostics'")
  js <- if (!is.matrix(scagnostics)) .jarray(.jarray(scagnostics), "[D")
        else .jarray(lapply(1:(dim(scagnostics)[2]), function(i) .jarray(as.double(scagnostics[,i]))), "[D")
  result <- .jcall("scagnostics.Scagnostics", "[Z", "computeScagnosticsOutliers", js)
  if (!is.null(colnames(scagnostics)))
    names(result) <- colnames(scagnostics)
  result
}

## --- exemplars

scagnosticsExemplars <- function(scagnostics) {
  if (!inherits(scagnostics, "scagnostics")) stop("scagnostics must be of the class `scagnostics'")
  js <- if (!is.matrix(scagnostics)) .jarray(.jarray(scagnostics), "[D")
        else .jarray(lapply(1:(dim(scagnostics)[2]), function(i) .jarray(as.double(scagnostics[,i]))), "[D")
  result <- .jcall("scagnostics.Scagnostics", "[Z", "computeScagnosticsExemplars", js)
  if (!is.null(colnames(scagnostics)))
    names(result) <- colnames(scagnostics)
  result
}

## -- grid

scagnosticsGrid <- function(scagnostics) {
  if (!inherits(scagnostics, "scagnostics")) stop("scagnostics must be of the class `scagnostics'")
  n <- as.integer(sqrt(2 * dim(scagnostics)[2] + 0.25) + 1)  ## the solution of n*(n-1)/2=d with extra 0.5 for rounding
  l <- data.frame(x=rep(1:n,n), y=rep(1:n, each=n)) # full grid
  l[l$x < l$y,] # collapse to those satisfying x < y
}

## -- initialization

.onLoad <- function(libname, pkgname)
  .jpackage("scagnostics", "scagnostics.jar")
