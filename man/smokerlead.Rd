\name{smokerlead}
\alias{smokerlead}
\concept{Observational study}
\concept{Matched pairs}
\docType{data}
\title{
Pair Differences in Blood Lead Levels in 679 Matched Pairs of One Smoker and One Control.
}
\description{
Data from the 2007-2008 US National Health and Nutrition Examination Survey.  There
are 679 matched pair differences in blood lead levels in ug per dl, comparing a daily smoker
and a nonsmoking control.  A daily smoker smoked every day for the last 30 days and
smoked at least 10 cigarettes per day.  Nonsmokers smoked fewer than 100 cigarettes
in their lives.  The data are described further in Rosenbaum (2012).
}
\usage{data("smokerlead")}
\format{
  A data frame with 679 observations on the following 3 variables.
  \describe{
    \item{\code{lead}}{Smoker-minus-control pair differences in blood lead
    levels, ug per dl.}
    \item{\code{llead}}{Smoker-minus-control pair differences in
    logs of blood lead levels.}
    \item{\code{dose}}{Number of cigarettes smoked per day for the smoker in
    the matched pair.}
  }
}
\details{
Pairs were matched for age, gender, education, income and ethnicity.
}
\source{
The data are originally from the 2007-2008 US National Health and Nutrition Examination Survey,
but the current example appeared in Rosenbaum (2012).
}
\references{
Rosenbaum, P. R. (2012) <doi:10.1093/biomet/ass032> Testing one hypothesis twice in observational studies. Biometrika, 99(4), 763-774.

US National Health and Nutrition Examination Survey.
}
\examples{
data(smokerlead)
attach(smokerlead)
# Compare with Table 1 in Rosenbaum (2012).
quantile(lead,c(0,1/16,1/8,1/4,1/2,3/4,7/8,15/16,1))
quantile(dose,c(0,1/16,1/8,1/4,1/2,3/4,7/8,15/16,1))
oldpar<-par(mfrow=c(1,2))
boxplot(llead,ylab="Difference in Logs of Lead Levels",xlab="Smoker-Control")
abline(h=0,col="red")
plot(dose,llead,ylab="Difference in Logs of Lead Levels",xlab="Cigarettes Per Day")
lines(lowess(dose,llead),col="red")
detach(smokerlead)
par(oldpar)
}
\keyword{datasets}
