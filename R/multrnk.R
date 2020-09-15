multrnk <-
function (y, m1 = 2, m2 = 2, m = 2, exact=FALSE)
  {
    stopifnot(is.vector(y))
    stopifnot((m1>=1)&(m1<=m2)&(m2<=m))
    stopifnot((m1==round(m1))&(m2==round(m2))&(m==round(m)))
    stopifnot(is.logical(exact))
    stopifnot(length(y)>=m)
    if (exact & (length(y)>100)) warning("Exact ranks involve large combinatorial coefficients when length(y) is large.")
    if (!exact){
      pk <- rank(abs(y))/length(y)
      q <- rep(0, length(pk))
      for (l in m1:m2) {
        q <- q + (l * choose(m, l) * (pk^(l - 1)) * ((1 - pk)^(m - l)))
      }
    }
    else {
      ny <- length(y)
      rk <- rank(abs(y))
      q <- rep(0, ny)
      for (l in m1:m2) {
        q <- q + choose(rk - 1, l - 1) * choose(ny - rk, m - l)
      }
    }
    notie <- 1 * (abs(y) > 0)
    q*notie
  }
