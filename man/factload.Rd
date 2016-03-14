\name{factload}
\alias{factload}

\title{
R-mode Exploratory Factor Analysis
}
\description{
 This function produces a factor loading matrix given by the specified factor extraction method, number of factors and rotation method. R-mode Exploratory Factor Analysis is based on the factorization of the variables. See "References" for more details.
}
\usage{
factload(data, cormeth = "spearman", data.cor, 
         method = "pc", nfac = 1, rotation = "varimax")
}

\arguments{
  \item{data}{
  Dataset to be analyzed for R-mode Exploratory Factor Analysis
}
  \item{cormeth}{
  Correlation coefficient. Type \code{polycor} for polychoric correlation coefficient (see \code{\link{polychor}}), for else see \code{\link{cor}}. If \code{data} is \code{NULL}, and \code{data.cor} is specified; \code{cormeth} will be ignored. 
}
  \item{data.cor}{
  Given correlation matrix. However, if \code{data} is specified, \code{data.cor} will be replaced.
}
  \item{method}{
  Factor extraction method. Type \code{pc} for Principal component factor analysis, \code{mle} for Maximum likelihood estimation and \code{prax} for Principal axis factor analysis (see "References")
}
  \item{nfac}{
  The number of factors
}
  \item{rotation}{
  Rotation method. Type \code{varimax} for orthogonal and \code{promax} for oblique rotation. 
}
}


\value{
 Returns a loadings class factor loading matrix
}

\references{
Reyment, R., Joreskog, K.G., 1993. Applied Factor Analysis in the Natural Sciences. Cambridge University Press, Cambridge, United Kingdom. 

Klinke, S., Wagner, C., 2008. Visualizing exploratory factor analysis models, in: Paulo, B. (Ed.), Compstat 2008: Proceedings in Computational Statistics: 18th Symposium Held in Porto, Portugal, 2008.

Harman, Harry H., and Wayne H. Jones. "Factor analysis by minimizing residuals (minres)." Psychometrika 31.3 (1966): 351-368.
}



\examples{
data(timss2011)
timss2011 <- na.omit(timss2011)
factload(timss2011,nfac=5,method="prax",cormeth="spearman")
factload(timss2011,nfac=8,method="pc",cormeth="pearson",rotation="promax")
timsscor <- cor(timss2011)
factload(data.cor=timsscor,nfac=10,method="mle")
}

\keyword{ Dandelion plot }
\keyword{ Exploratory Factor Analysis }
