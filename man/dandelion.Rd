\name{dandelion}
\alias{dandelion}
\title{
Dandelion Plot 
}
\description{
A Dandelion plot for R-mode Exploratory Factor Analysis methods. The loading matrix and the factor variances are being visualized. 
} 
\usage{ 
dandelion(fact_load, bound = 0.5, mcex=c(1,1), palet)
}
\arguments{
  \item{fact_load}{A "loadings" class object. Factor loading matrix. 
}
  \item{bound}{
Minimum loadings to visualize. It should be set between 0 and 1. For example, bound=0.5 will only visualize loadings more than 0.5.
 }
  \item{mcex}{
A vector with two points. First value determines the size of labels within dandelion plot, and the second determines the size of labels within uniquenesses and communalities graphs
 }
  \item{palet}{
A vector of color pallette. The first and the last elements of the vector are the colors of positive and negative loadings. 
 }
}
\details{
A Dandelion Plot visualizes both factor variances and loadings in the same time. Each central line represents a different factor and is connected to a star graph. These star graphs visualize the factor loadings for the corresponding factor. Negative and positive loadings are indicated by two different colors. Explained variance of each factor can be observed by the size of each star graph or by the angle between the current and the consecutive central line. For example, explained variance of first factor is determined by the angle between the first and second central line. 
Communalities and uniquenesses are also given on the right hand side along barchart of cummulative explanation ratios of factors (with individual variances on top). 
}
\references{
Artur Manukyan, Erhan Cene, Ahmet Sedef, Ibrahim Demir, \emph{Dandelion plot: a method for the visualization of R-mode exploratory factor analyses}. Computational Statistics 29.6 (2014): 1769-1791.
}
\author{
Artur Manukyan, Ahmet Sedef, Erhan Cene, Ibrahim Demir
}

\examples{
# E.F.A. of Timss 2011 Student Questionnaire Example for 5 and 8 number of factors
data(timss2011)
timss2011 <- na.omit(timss2011)
dandpal <- rev(rainbow(100, start = 0, end = 0.2))
facl <- factload(timss2011,nfac=5,method="prax",cormeth="spearman")
dandelion(facl,bound=0,mcex=c(1,1.2),palet=dandpal)
facl <- factload(timss2011,nfac=8,method="mle",cormeth="pearson")
dandelion(facl,bound=0,mcex=c(1,1.2),palet=dandpal)
}

\keyword{ Factor Analysis }
\keyword{ Visaulization }
\keyword{ Dandelion Plot }
