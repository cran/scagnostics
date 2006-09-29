\name{scagnostics}
\alias{scagnostics}
\alias{scagnostics.default}
\alias{scagnostics.matrix}
\alias{scagnostics.data.frame}
\alias{scagnostics.list}
\alias{scagnosticsOutliers}
\alias{scagnosticsExemplars}
\alias{scagnosticsGrid}
\title{Calculcate scagnostics for pairs of variables}
\author{Lee Wilkinson <leland.wilkinson@gmail.com>, Anushka Anand (University of
Illinois at Chicago), R package: Simon Urbanek
<simon.urbanek@r-project.org>}
\description{
Scagnostics (scatterplot diagnostics) summarize potentially
interesting patterns in 2d scatterplots.
}
\usage{
scagnostics(x, ...)
scagnosticsOutliers(scagnostics)
scagnosticsExemplars(scagnostics)
scagnosticsGrid(scagnostics)
}
\arguments{
\item{x}{object to calculate scagnostics on: a vector, a matrix or a data.frame}
\item{...}{...}
\item{scagnostics}{objects returned from the \code{scagnostics} function}
}
\value{
\code{scagnostics} returns a vector (for a pair) or a matrix
consisting of scagnostics (rows) by variable pairs (columns).

\code{scagnostics.outliers} and \code{scagnostics.exemplars} return a
logical vector.

\code{scagnostics.grid} returns a data frame with columns \code{x} and
\code{y} which as a pair define the index of variables corresponding
to the entries in the scagnostics matrix. Hence it has as many rows as
there are columns in the scagnostics matrix and each row defines one
pair of variables.
}
\references{
  Wilkinson L., Anand, A., and Grossman, R. (2006).
  \emph{High-Dimensional visual analytics: Interactive exploration
  guided by pairwise views of point distributions}. IEEE Transactions
  on Visualization and Computer Graphics, November/December 2006
  (Vol. 12, No. 6) pp. 1363-1372.

  Wilkinson L., Anand, A., and Grossman, R. (2005).
  \emph{Graph-Theoretic Scagnostics}. Proceedings of the 2005 IEEE
  Symposium on Information Visualization, p. 21.
  
  \url{http://www.cs.uic.edu/~wilkinson/}
}
\examples{
# tesing various forms
scagnostics(1:10, 1:10)
scagnostics(rnorm(100), rnorm(100))
scagnostics(as.matrix(mtcars))

# more real use on a dataset
s <- scagnostics(mtcars)

# look at outliers
o <- scagnosticsOutliers(s)
o[o]
# one outlier, let's plot it
g <- scagnosticsGrid(s)
go <- g[o,]
plot(mtcars[[go$x]], mtcars[[go$y]], pch=19,
     xlab=names(mtcars)[go$x], ylab=names(mtcars)[go$y])

# find and plot exemplars
e <- scagnosticsExemplars(s)
e[e]
ge <- g[e,]
par(mfrow = c(2,2))
for (i in 1:dim(ge)[1])
  plot(mtcars[[ge$x[i]]], mtcars[[ge$y[i]]], pch=19,
       xlab=names(mtcars)[ge$x[i]], ylab=names(mtcars)[ge$y[i]])
}
\keyword{hplot}
