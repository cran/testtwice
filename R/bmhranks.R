bmhranks <-
function (y, q1 = 1/3, q2 = 2/3)
{
    stopifnot(is.vector(y))
    stopifnot((q1>0)&(q1<1)&(q2>0)&(q2<1))
    rk <- rank(abs(y))/length(y)
    if (q1!=q2) q <- (rk >= q1) + (rk >= q2)
    else q <- (rk >= q1)
    notie <- 1 * (abs(y) > 0)
    q <- q * notie
    q
}
