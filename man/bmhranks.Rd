\name{bmhranks}
\alias{bmhranks}
\concept{Signed rank test}

\title{
One-step and Two-step Signed Ranks.
}
\description{
Calculates either one-step or two-step signed ranks as proposed
by Noether (1973), Brown (1981) and Markowski and Hettmansperger (1982).
}
\usage{
bmhranks(y, q1 = 1/3, q2 = 2/3)
}
\arguments{
  \item{y}{
A vector of matched pair differences.
}
  \item{q1}{
Quantile at which step-ranks rise from 0 to 1.
}
  \item{q2}{
Quantile at which step-ranks rise from 1 to 2.
If q1=q2, then there is only one step, from 0 to 1.
}
}

\value{
A vector with the same length as y containing the step-ranks.
}
\references{
Brown, B. M. (1981) <doi:10.1093/biomet/68.1.235> Symmetric quantile averages and related estimators. Biometrika, 68(1), 235-242.

Markowski, E. P. and Hettmansperger, T. P. (1982) <doi:10.2307/2287325> Inference based on simple rank step score statistics for the location model. Journal of the American Statistical Association, 77(380), 901-907.

Noether, G. E. (1973) <doi:10.2307/2284805> Some simple distribution-free confidence intervals for the center of a symmetric distribution. Journal of the American Statistical Association, 68(343), 716-719.

Rosenbaum, P. R. (1999) <doi:10.1111/1467-9876.00140> Using quantile averages in matched observational studies. Journal of the Royal Statistical Society: Series C (Applied Statistics), 48(1), 63-78.

Rosenbaum, P. R. (2012) <doi:10.1214/11-AOAS508> An exact adaptive test with superior design sensitivity in an observational study of treatments for ovarian cancer. The Annals of Applied Statistics, 6(1), 83-105.

Rosenbaum, P. R. (2015) <doi:10.1080/01621459.2014.960968> Bahadur efficiency of sensitivity analyses in observational studies. Journal of the American Statistical Association, 110(509), 205-217.
}
\author{
Paul R. Rosenbaum
}
\note{
The use of step-ranks in observational studies is discussed in Rosenbaum (1999, 2012, 2015).  They can have larger
design sensitivity than Wilcoxon ranks and higher Bahadur efficiency in a sensitivity analysis.
}

\seealso{
The function multrnk() computes an alternative family of signed ranks.
}
\examples{
data(smokerlead)
attach(smokerlead)

w<-rank(abs(llead)) # Wilcoxon ranks
o<-order(w)
brn<-bmhranks(llead) # Brown (1981) ranks
plot(w[o],brn[o],type="n",xlab="Wilcoxon ranks",ylab="Step Ranks",
    main="Comparison of 3 Step Ranks")
lines(w[o],brn[o],col="black")

# The following two-step ranks were best for Normal data in
# Table 2 of Markowski-Hettmansperger (1982).
mhn<-bmhranks(llead,q1=.4,q2=.8)
lines(w[o],mhn[o],col="blue")

# Noether (1973) ranks take a single step.  The case of a step
# at q1=q2=2/3 was evaluated in Rosenbaum (2015, Table 2).
noe<-bmhranks(llead,q1=2/3,q2=2/3)
lines(w[o],noe[o],col="red")
legend(20,1.75,c("Brown","MH","Noether"),
    lty=c(1,1,1),col=c("black","blue","red"))

# Adaptive choice of Brown or Noether ranks was considered in
# Rosenbaum (2012).  In this case, an exact distribution
# is available, but function tt() uses a limiting Normal
# distribution instead.
H<-cbind(brn,noe)
tt(llead,H,gamma=1)
tt(llead,H,gamma=3.7)

rm(w,brn,mhn,noe,H)
detach(smokerlead)
}

\keyword{ htest }
