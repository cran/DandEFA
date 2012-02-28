\name{dandelion}
\alias{dandelion}
\title{
Dandelion Plot 
}
\description{
Creating a Dandelion plot for R-mode Exploratory Factor Analysis methods. Loadings matrix and Factor variances are being visualized.
} 
\usage{
dandelion(fact_load, bound = 0.5)
}
\arguments{
  \item{fact_load}{
A "loadings" class object. 
}
  \item{bound}{
Minimum loadings to visualize. Value should be between 0 and 1. For example, bound=0.5 will only visualize loadings more than 0.5.
 }
}
\details{
A Dandelion Plot visualizes both factor variances and loadings in the same time. Each central line represents a different factor and is connected to a star graph. These star graphs visualize the factor loadings for the corresponding factor. Negative and positive loadings are indicated by red and blue regions, respectively. Explained variance of each factor can be observed by the size of each star graph or by the angle between the consecutive central lines. For example, explained variance of first factor is determined by the angle between first and second central line. 
Communalities and uniquenesses are also given on the right hand side. Communality star graph can also be used as an identifier for the directions of variables in order to interpret the factor loadings.
}
\references{
Artur Manukyan, Ibrahim Demir, Ahmet Sedef (2011) "A New Graphical Approach to Exploratory Factor Analysis", \emph{ABSTRACT BOOK for the 5th Annual International Conference on Mathematics and Statistics}, Atina, Greece, June 2011.
}
\author{
Artur Manukyan, Ahmet Sedef, Erhan Cene, Ibrahim Demir(Advisor)
}

\examples{
# Timss 2007 Student Questionnaire Example for 5 and 8 loadings
data(timss2007)
timss_cor <- cor(timss2007,method="spearman",use="na.or.complete")
dandelion(factanal(covmat=timss_cor,factors=5)$loadings,bound=0)
dandelion(factanal(covmat=timss_cor,factors=8)$loadings,bound=0)


# Porsuk stream example for 6 and 10 factors
data(wqporsuk)
fact_wqporsuk <- factanal(covmat = wqporsuk,factors=6)$loadings
dandelion(fact_wqporsuk,bound=0)
fact_wqporsuk <- factanal(covmat = wqporsuk,factors=10)$loadings
dandelion(fact_wqporsuk,bound=0)
}

\keyword{ Factor Analysis }
\keyword{ Visaulization }
\keyword{ Dandelion Plot }
