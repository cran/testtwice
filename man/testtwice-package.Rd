\name{testtwice-package}
\alias{testtwice-package}
\concept{Causal inference}
\concept{Testing twice}
\concept{Observational study}
\concept{Sensitivity analysis}
\concept{Matched pairs}
\concept{Adaptive inference}
\docType{package}
\title{
\packageTitle{testtwice}
}
\description{
\packageDescription{testtwice}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{testtwice}
\packageIndices{testtwice}
}
\author{
\packageAuthor{testtwice}

Maintainer: \packageMaintainer{testtwice}
}
\references{
Rosenbaum, P. R. (2012) <doi:10.1093/biomet/ass032> Testing one hypothesis twice in observational studies. Biometrika, 99(4), 763-774.
}

\keyword{ htest }

\examples{
data(smokerlead)
attach(smokerlead)

# The following example reproduces parts of the first column of Table 3 in Rosenbaum (2012).
w<-rank(abs(lead))
wd<-rank(abs(lead))*dose
wl<-rank(abs(llead))
H<-cbind(w,wd,wl)
tt(lead,H,gamma=2.8)
tt(lead,H,gamma=3.2)

rm(w,wd,wl,H)
detach(smokerlead)
}
