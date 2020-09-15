\name{tt}
\alias{tt}
\concept{Causal inference}
\concept{Testing twice}
\concept{Observational study}
\concept{Sensitivity analysis}
\concept{Matched pairs}
\concept{Family-wise error rate}

\title{
Computes the P-value and Sensitivity Bound for Testing Twice From a User Supplied Matrix.
}
\description{
The function tt computes the P-value for testing twice from
from a vector y of matched pair differences and a matrix H of
ranks of the absolute values of y.  Literally, one is testing
twice if H has two columns, but H can have more than two columns.

The function testtwice() will create the matrix H and then call
tt(); however, tt() allows you to create H according to your own
specifications.
}
\usage{
tt(y, H, gamma = 1)
}

\arguments{
  \item{y}{
A vector of matched pair differences.
}
  \item{H}{
A matrix with length(y) rows giving various ways of
ranking the absolute y's.  Entries in H must be
nonnegative.  The matrix H must have at least two
columns and at most 20.  In a typical application,
H might have 2 to 4 columns.  If the columns of H
have names, then these names are used to label the
output; so, it is helpful if H has column names.
}
  \item{gamma}{
Value of the sensitivity parameter, gamma>=1, with gamma=1 for a
randomization test.
}
}
\details{
With matched pair differences in an observational study, the function tt() performs
several signed rank tests with different ways of scoring the absolute
ranks of the differences, and corrects for multiple testing using
the joint limiting Normal distribution of the tests.  For gamma>1, the
function tt() performs a sensitivity analysis, reporting an upper bound
on the one-sided P-value.  The method and example are from Rosenbaum (2012b).
}
\value{
\item{pval }{The upper bound on the one-sided P-value from the joint test.
If gamma=1, then this is the P-value, not an upper bound on the P-value.}
\item{dev }{The standardized deviates from the joint test, one for each
column of H.  The test uses the largest standardized deviate, correcting
for multiple testing.}
\item{cr }{The correlation matrix of the test statistics under
the null hypothesis at the given value of gamma.}
}
\references{

Berk, R. H. and Jones, D. H. (1978) <doi:10.2307/4615706> Relatively optimal combinations of test statistics. Scandinavian Journal of Statistics, 158-162.

Brown, B. M. (1981) <doi:10.1093/biomet/68.1.235> Symmetric quantile averages and related estimators. Biometrika, 68(1), 235-242.

Conover, W. J. and Salsburg, D. S. (1988) <doi:10.2307/2531906> Locally most powerful tests for detecting treatment effects when only a subset of patients can be expected to respond to treatment. Biometrics, 189-196.

Markowski, E. P. and Hettmansperger, T. P. (1982) <doi:10.2307/2287325> Inference based on simple rank step score statistics for the location model. Journal of the American Statistical Association, 77(380), 901-907.

Noether, G. E. (1973) <doi:10.2307/2284805> Some simple distribution-free confidence intervals for the center of a symmetric distribution. Journal of the American Statistical Association, 68(343), 716-719.

Rosenbaum, P. R. (1999) <doi:10.1111/1467-9876.00140> Using quantile averages in matched observational studies. Journal of the Royal Statistical Society: Series C (Applied Statistics), 48(1), 63-78.

Rosenbaum, P. R. (2007) <doi:10.1111/j.1541-0420.2007.00783.x> Confidence intervals for uncommon but dramatic responses to treatment. Biometrics, 63(4), 1164-1171.

Rosenbaum, P. R. (2011) <doi:10.1111/j.1541-0420.2010.01535.x> A new U-statistic with superior design sensitivity in matched observational studies. Biometrics, 67(3), 1017-1027.m

Rosenbaum, P. R. (2012a) <doi:10.1214/11-AOAS508> An exact adaptive test with superior design sensitivity in an observational study of treatments for ovarian cancer. The Annals of Applied Statistics, 6(1), 83-105.

Rosenbaum, P. R. (2012b) <doi:10.1093/biomet/ass032> Testing one hypothesis twice in observational studies. Biometrika, 99(4), 763-774.

Stephenson, W. R. (1981) <doi:10.2307/2287596> A general class of one-sample nonparametric test statistics based on subsamples. Journal of the American Statistical Association, 76(376), 960-966.
}
\author{
Paul R. Rosenbaum
}
\note{
Various signed rank statistics have been proposed by Brown (1981), Markowski and Hettmansperger (1982),
Noether (1973), Rosenbaum (2007, 2011), and Stephenson (1981).  The function bmhranks() generates ranks
proposed by Brown (1981), Markowski and Hettmansperger (1982) and Noether (1973); see also Rosenbaum (1999, 2012a).
The function multrnk() generates ranks proposed by Stephenson (1981), Rosenbaum (2007, 2011); see
also Conover and Salsburg (1988).  So, the columns of H may be built using bmhranks() and multrnk().

If y[i]=0, then the ith pair difference does not contribute to the permutation test.

The P-value is one-sided, upper-tailed.  To obtain a one sided, lower-tailed P-value,
replace y by -y.  See the documentation for the testtwice() function.

Technical note:  The function tt() calls the pmvnorm() function in the mvtnorm package.
In this call, tt() sets the algorithm option
to Miwa(steps=512).  This choice of algorithm avoids the default
algorithm in pmvnorm(), namely GenzBretz, which is
a randomized algorithm, returning slightly different P-values each time it is called.
}


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

# The following example reproduces parts of the second column of Table 3 in Rosenbaum (2012).
brn<-bmhranks(lead)
brnd<-brn*dose
brnl<-bmhranks(llead)
H2<-cbind(brn,brnd,brnl)
tt(lead,H2,gamma=3.2)
tt(lead,H2,gamma=3.4)

rm(w,wd,wl,H,brn,brnd,brnl,H2)
detach(smokerlead)
}

\keyword{ htest }
