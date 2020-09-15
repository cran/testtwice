\name{testtwice}
\alias{testtwice}
\concept{Causal inference}
\concept{Adaptive inference}
\concept{Testing twice}
\concept{Observational study}
\concept{Sensitivity analysis}
\concept{Design sensitivity}
\concept{Matched pairs}
\concept{Family-wise error rate}

\title{
Computes the P-value and Sensitivity Bound for Testing Twice.
}
\description{
The function testtwice() is a convenient way to call the
function tt().  Conversely, the function tt() is a less convenient but more flexible way to test twice.  The function tt() requires you to build a matrix of signed ranks, but testtwice() builds that matrix for you.

The function tt() computes the P-value for testing twice from
from a vector y of matched pair differences and a matrix H of
ranks of the absolute values of y.  In contrast, testwice() follows your instructions and builds H according to those instructions.  Alternatively, by setting do.test=FALSE, testtwice() will assist in constructing some columns of H for use by tt().  See Details.

The default is the same as u858 = TRUE and u878 = TRUE.  If you do not take the default, then you must set at least two of the statistics to TRUE; otherwise, an error will result.

}
\usage{
testtwice(y, dose = NULL, gamma = 1, u858 = FALSE,
   u888 = FALSE, u878 = FALSE, u868 = FALSE,
   u867 = FALSE, u222 = FALSE, brown = FALSE,
   noether = FALSE, tailored = FALSE, alternative="greater",
   do.test = TRUE)
}

\arguments{
  \item{y}{
A vector of matched pair differences.
}

\item{dose}{
If is.null(dose), then there are no doses.  Otherwise, dose is a vector
with length(dose)=length(y) giving nonnegative doses of treatment for the treated
individual in a matched pair, where the control received dose zero.
If there are doses, the ranks are multiplied by the doses.  An error
will result if some doses are negative.
}
\item{gamma}{
Value of the sensitivity parameter, gamma>=1, with gamma=1 for a
randomization test.
}
  \item{u858}{
If u858 is TRUE, one column of H is created by the function multrnk()
as multrnk(y, m1 = 5, m2 = 8, m = 8).  See the documentaion for
multrnk and Rosenbaum (2011).
}
  \item{u888}{
If u888 is TRUE, one column of H is created by the function multrnk()
as multrnk(y, m1 = 8, m2 = 8, m = 8).  See the documentaion for
multrnk, Stephenson (1981), and Rosenbaum (2007, 2011).
}
  \item{u878}{
If u878 is TRUE, one column of H is created by the function multrnk()
as multrnk(y, m1 = 7, m2 = 8, m = 8).  See the documentaion for
multrnk and Rosenbaum (2011).
}
  \item{u868}{
If u868 is TRUE, one column of H is created by the function multrnk()
as multrnk(y, m1 = 6, m2 = 8, m = 8).  See the documentaion for
multrnk and Rosenbaum (2011).
}
  \item{u867}{
If u868 is TRUE, one column of H is created by the function multrnk()
as multrnk(y, m1 = 6, m2 = 7, m = 8).  See the documentaion for
multrnk and Rosenbaum (2011).
}
  \item{u222}{
If u868 is TRUE, one column of H is created by the function multrnk()
as multrnk(y, m1 = 2, m2 = 2, m = 2).  See the documentaion for
multrnk and Rosenbaum (2011).  These ranks are nearly the same
as Wilcoxon's ranks; see Stephenson (1981) and Pratt and Gibbons
(1981, Section 3.5).  Specifically, these ranks produce the
U-statistic that is nearly identical to Wilcoxon's statistic.
}
  \item{brown}{
If brown is TRUE, one column of H is created by the function bmhranks()
as bmhranks(y, q1=1/3, q2=2/3).  This yields Brown (1981)'s test.
See the documentaion for bmhranks() and Rosenbaum (2012a).  These ranks are
one special case of the two-step ranks proposed by Markowski and
Hettmansperger (1982).
}
  \item{noether}{
If noether is TRUE, one column of H is created by the function bmhranks()
as bmhranks(y, q1=2/3, q2=2/3).  This yields one version of Noether (1973)'s
test and one version of the one-step tests proposed by Markowski and
Hettmansperger (1982).  See the documentaion for
bmhranks and and Rosenbaum (2012a).
}
  \item{tailored}{
If tailored is TRUE, one column of H is contains the tailored ranks in
Rosenbaum (2015, Section 4.3 and Table 3).  Although somewhat complex
in form, these ranks have attractive design sensitivity and Bahadur
efficiency compared with Noether's ranks above.
}
  \item{alternative}{
If alternative="greater"", then the null hypothesis of no effect is tested
against an alternative of a positive effect.  If alternative="less"", then
the null hypothesis of no effect is tested
against an alternative of a negative effect.  For a two-sided test,
do both tests, double the smaller of the two one-sided P-values, and
replace values above 1 by 1.
}
  \item{do.test}{
If do.test=TRUE, then testtwice calls tt() to perform the test.  If
do.test=FALSE, then testtwice does not perform the test, but instead
returns one or more columns of H.  With do.test=FALSE, the user can
build H using cbind() to combine several columns built by one or more
calls to testtwice, and perhaps several other columns built by the user.
}
}
\details{
The function testtwice() is a convenient way to call the
fucntion tt() which computes the P-value for testing twice from
from a vector y of matched pair differences and a matrix H of
ranks of the absolute values of y.  The function testtwice() can
create the matrix H and call tt().  Alternatively, testtwice() can create
one or more columns of H that may be combined using cbind()
to create H for use with tt().  The function testtwice() automates
the construction of H in some common situations, but the function tt() gives
the user total control over the construction of H, albeit with greater
effort.  Literally, one is testing
twice if H has two columns, but H can have more than two columns.

With matched pair differences in an observational study, the functions
testtwice() and tt() perform
several signed rank tests with different ways of scoring the absolute
ranks of the differences, and corrects for multiple testing using
the joint limiting Normal distribution of the tests.  For gamma>1, the
functions perform a sensitivity analysis, reporting an upper bound
on the one-sided P-value.  The method and example are from Rosenbaum (2012b).

IMPORTANT: The default is equivalent to setting u858 = TRUE and
u878 = TRUE.  That is, if no test statistic is selected, then
u858 and u878 are selected.

Setting alternative="less" has the same effect as replacing y by -y with
alternative="greater".

For a textbook discussion of adaptive inference by testing twice, see Rosenbaum (2020a, section 19.3).  A different approach to adaptive inference in observational studies
is discussed in Rosenbaum (2020b).

Use the function senU() in the DOS2 package if you do not wish to test twice, but do wish to do a sensitivity analysis using the U-statistic in Rosenbaum (2011), with confidence intervals and point estimates.
}
\value{
If do.test=TRUE, then a list containing the following items is returned.
\item{pval }{The upper bound on the one-sided P-value from the joint test.
If gamma=1, then this is the P-value, not an upper bound on the P-value.}
\item{dev }{The standardized deviates from the joint test, one for each
column of H.  The test uses the largest standardized deviate, correcting
for multiple testing.}
\item{cr }{The correlation matrix of the test statistics under
the null hypothesis at the given value of gamma.}

If do.test=FALSE, then a vector or matrix of signed ranks is returned
for use in the function tt().  See the examples.
}
\references{

Berk, R. H. and Jones, D. H. (1978) <doi:10.2307/4615706> Relatively optimal combinations of test statistics. Scandinavian Journal of Statistics, 158-162.

Brown, B. M. (1981) <doi:10.1093/biomet/68.1.235> Symmetric quantile averages and related estimators. Biometrika, 68(1), 235-242.

Conover, W. J. and Salsburg, D. S. (1988) <doi:10.2307/2531906> Locally most powerful tests for detecting treatment effects when only a subset of patients can be expected to respond to treatment. Biometrics, 189-196.

Markowski, E. P. and Hettmansperger, T. P. (1982) <doi:10.2307/2287325> Inference based on simple rank step score statistics for the location model. Journal of the American Statistical Association, 77(380), 901-907.

Noether, G. E. (1973) <doi:10.2307/2284805> Some simple distribution-free confidence intervals for the center of a symmetric distribution. Journal of the American Statistical Association, 68(343), 716-719.

Pratt, J. W. and Gibbons, J. D. (1981) <doi:10.1007/978-1-4612-5931-2>  Concepts of Nonparametric Theory.  New York: Springer. (Section 3.5)

Rosenbaum, P. R. (1999) <doi:10.1111/1467-9876.00140> Using quantile averages in matched observational studies. Journal of the Royal Statistical Society: Series C (Applied Statistics), 48(1), 63-78.

Rosenbaum, P. R. (2007) <doi:10.1111/j.1541-0420.2007.00783.x> Confidence intervals for uncommon but dramatic responses to treatment. Biometrics, 63(4), 1164-1171.

Rosenbaum, P. R. (2011) <doi:10.1111/j.1541-0420.2010.01535.x> A new U-Statistic with superior design sensitivity in matched observational studies. Biometrics, 67(3), 1017-1027.

Rosenbaum, P. R. (2012a) <doi:10.1214/11-AOAS508> An exact adaptive test with superior design sensitivity in an observational study of treatments for ovarian cancer. The Annals of Applied Statistics, 6(1), 83-105.

Rosenbaum, P. R. (2012b) <doi:10.1093/biomet/ass032> Testing one hypothesis twice in observational studies. Biometrika, 99(4), 763-774.

Rosenbaum, P. R. (2015) <doi:10.1080/01621459.2014.960968> Bahadur efficiency of sensitivity analyses in observational studies. Journal of the American Statistical Association, 110(509), 205-217.

Rosenbaum, P. R. (2020a) <doi:10.1007/978-1-4419-1213-8> Design of Observational Studies (2nd edition).  NY: Springer.

Rosenbaum, P. R. (2020b) <doi:10.1093/biomet/asaa032> A conditional test with demonstrated insensitivity to unmeasured bias in matched observational studies. Biometrika, to appear.

Stephenson, W. R. (1981) <doi:10.2307/2287596> A general class of one-sample nonparametric test statistics based on subsamples. Journal of the American Statistical Association, 76(376), 960-966.
}
\author{
Paul R. Rosenbaum
}
\note{
Various signed rank statistics have been proposed by Brown (1981), Markowski and Hettmansperger (1982),
Noether (1973), Rosenbaum (2007, 2011) and Stephenson (1981).  The function testtwice() uses two
or more of these signed ranks to perform the test.  See also the documentation for tt().

If y[i]=0, then the ith pair difference does not contribute to the permutation test.
}


\examples{
data(smokerlead)
attach(smokerlead)

testtwice(lead,gamma=3)
testtwice(llead,gamma=3)
testtwice(llead,u858=TRUE,u888=TRUE,gamma=3)
# Same calculation, done differently.
H<-testtwice(llead,u858=TRUE,u888=TRUE,do.test=FALSE)
dim(H)
tt(llead,H,gamma=3)


# The following example reproduces parts of the second
# column (Brown) of Table 3 in Rosenbaum (2012).
# An example in the documentation for function tt()
# does the same calculation in a different way.
brn<-testtwice(lead,brown=TRUE,do.test=FALSE)
brnd<-testtwice(lead,dose=dose,brown=TRUE,do.test=FALSE)
brnl<-testtwice(llead,brown=TRUE,do.test=FALSE)
tt(lead,cbind(brn,brnd,brnl),gamma=3.2)

# The following example reproduces parts of the third
# column (U-statistic) of Table 3 in Rosenbaum (2012).
u878<-testtwice(lead,u878=TRUE,do.test=FALSE)
u867<-testtwice(lead,u867=TRUE,do.test=FALSE)
u878L<-testtwice(llead,u878=TRUE,do.test=FALSE)
u867L<-testtwice(llead,u867=TRUE,do.test=FALSE)
tt(lead,cbind(u878,u867,u878L,u867L),gamma=3.2)
tt(lead,cbind(u878,u867,u878L,u867L),gamma=3.6)

# The following example compares noether=TRUE and tailored=TRUE.
testtwice(llead,brown=TRUE,noether=TRUE,gamma=3.74)
testtwice(llead,brown=TRUE,tailored=TRUE,gamma=3.74)

rm(brn,brnd,brnl,u878,u878L,u867,u867L,H)
detach(smokerlead)
}

\keyword{ htest }
