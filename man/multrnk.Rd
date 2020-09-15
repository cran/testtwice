\name{multrnk}
\alias{multrnk}
\concept{Signed rank test}

\title{
A family of Signed Ranks for Matched Pair Differences.
}
\description{
Computes a family of signed ranks that includes the ranks
of Wilcoxon, Stephenson (1981) and Rosenbaum (2011).
}
\usage{
multrnk(y, m1 = 2, m2 = 2, m = 2, exact=FALSE)
}
\arguments{
  \item{y}{
A vector of matched pair differences.
}
  \item{m1}{
See m.
}
  \item{m2}{
See m.
}
  \item{m}{
The three integers (m,m1,m2) with 1<=m1<=m2<=m define a U-statistic
and an associated signed rank statistic.  See Details.
}
\item{exact}{
In large samples, it is appropriate to set exact=FALSE, and this
is the default.  When testing hypotheses, it is reasonable to set exact=FALSE for all
sample sizes.

If exact=FALSE, then the rank scores are approximated using expression (9)
in Rosenbaum (2011).  If exact=TRUE, then ranks are proportional to (8) in
Rosenbaum (2011).  The exact ranks involve combinatorial coefficients
that grow very quickly with the sample size, whereas the approximate
ranks do not have this property.

The confidence intervals for attributable effects in Rosenbaum (2003; 2007;
2011, Appendix) are based on the exact ranks.
}
}
\details{
Setting m1=2, m2=2, and m=2 yields the U-statistic that is nearly the same
as Wilcoxon's signed rank statistic.  See Lehmann (1975, Appendix, Example 6)
or Pratt and Gibbons (1981, Section 3.5).

Setting m1=m2=m>=2 yields the method of Stephenson (1981).  The ranks used by
Stephenson (1981) are closely related to ranks proposed by Conover and
Salsburg (1988) who aimed for higher power when only a subset of
treated individuals are affected by the treatment; see Rosenbaum (2007).

Stephenson (1981) looks at m differences in y, finds the one pair difference
with the largest absolute value, and score a 1 if the difference is
positive or a zero if it is negative.  He sums this score over all
subsets of m distinct differences.

The general case, 1<=m1<=m2<=m is discussed in Rosenbaum (2011), with
further evaluation of performance in Rosenbaum (2015).  The statistic in Rosenbaum (2011) may be inverted to obtain a confidence interval for an attributable effect; see also Rosenbaum (2003) for the special case of
m1=m2=m=2 analogous to Wilcoxon's test.

For instance, the statistic (8,5,8) has Pitman efficiency of .97 compared
to Wilcoxon for Normal or logistic errors in a randomized experiment, but
has higher design sensitivity when used in a sensitivity analysis in an
observational study; see Rosenbaum (2011, Tables 1 and 3).


}
\value{
A vector with the same length as y containing the ranks.
}
\references{
Conover, W. J. and Salsburg, D. S. (1988) <doi:10.2307/2531906> Locally most powerful tests for detecting treatment effects when only a subset of patients can be expected to respond to treatment. Biometrics, 189-196.

Lehmann, E. L. (1975). Nonparametrics.  San Francisco: Holden-Day.

Pratt, J. W. and Gibbons, J. D. (1981) <doi:10.1007/978-1-4612-5931-2>  Concepts of Nonparametric Theory.  New York: Springer.

Rosenbaum, P. R. (2003) <doi:10.1198/0003130031405> Exact confidence intervals for nonconstant effects by inverting the signed rank test. American Statistician, 57(2), 132-138.

Rosenbaum, P. R. (2007) <doi:10.1111/j.1541-0420.2007.00783.x> Confidence intervals for uncommon but dramatic responses to treatment. Biometrics, 63(4), 1164-1171.

Rosenbaum, P. R. (2011) <doi:10.1111/j.1541-0420.2010.01535.x> A New U-Statistic with Superior Design Sensitivity in Matched Observational Studies. Biometrics, 67(3), 1017-1027.

Rosenbaum, P. R. (2012) <doi:10.1093/biomet/ass032> Testing one hypothesis twice in observational studies. Biometrika, 99(4), 763-774.

Rosenbaum, P. R. (2015) <doi:10.1080/01621459.2014.960968> Bahadur efficiency of sensitivity analyses in observational studies. Journal of the American Statistical Association, 110(509), 205-217.

Stephenson, W. R. (1981) <doi:10.1080/01621459.1981.10477749> A general class of one-sample nonparametric test statistics based on subsamples. Journal of the American Statistical Association, 76(376), 960-966.
}
\author{
Paul R. Rosenbaum
}
\note{
The performance of various ranks when used in sensitivity analyses is discussed in Rosenbaum (2015).
}


\seealso{
An alternative family of signed ranks is calculated by the function bmhranks().
}
\examples{
# The following example reproduces part of column 3 of Table 3 of Rosenbaum (2012).
data("smokerlead")
attach(smokerlead)
u878<-multrnk(lead,m1=7,m2=8,m=8)
u867<-multrnk(lead,m1=6,m2=7,m=8)
u878l<-multrnk(llead,m1=7,m2=8,m=8)
u867l<-multrnk(llead,m1=6,m2=7,m=8)
H<-cbind(u878,u867,u878l,u867l)
tt(lead,H,gamma=3)
tt(lead,H,gamma=3.4)
rm(u878,u867,u878l,u867l,H)
detach(smokerlead)
#
# ----------------------
# The following examples are intended to aid understanding of
# some of the technical details.

# Exact and approximate ranks
# Exact and approximate ranks are highly correlated.
a<-multrnk(1:50,m1=4,m2=5,m=5,exact=FALSE)
b<-multrnk(1:50,m1=4,m2=5,m=5,exact=TRUE)
cor(a,b)

# Compare the following with Section 3.5 in Pratt and Gibbons (1981)
multrnk(1:10,exact=TRUE)

# Stephenson (1981) ranks for m=5 with 10 pair differences.
a<-multrnk(1:10,m1=5,m2=5,m=5,exact=TRUE)
sum(a)
choose(10,5)
a
# There are 252 ways to pick 5 differences from the 10 differences.
# In 70/252 subsets of size 5, the pair with absolute rank 9
# has the largest absolute pair difference, choose(9-1,5-1) = 70,
# and determines the sign.
}

\keyword{ htest }
