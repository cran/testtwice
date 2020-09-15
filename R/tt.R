tt <-
function (y, H, gamma = 1)
{
    if (is.data.frame(H)) H<-as.matrix(H)
    stopifnot(gamma>=1)
    stopifnot(is.matrix(H))
    stopifnot(is.vector(y))
    stopifnot(length(y)==(dim(H)[1]))
    stopifnot((dim(H)[2])>=2)
    stopifnot((dim(H)[2])<=20)
    stopifnot(min(as.vector(H))>=0)
    k <- dim(H)[2]
    nt <- (abs(y)) > 0
    y <- y[nt]
    H <- H[nt, ]
    sn <- 1 * (y > 0)
    st <- as.vector(unlist(sn %*% H))
    theta <- gamma/(1 + gamma)
    ex <- theta * unlist(apply(H, 2, sum))
    cv <- theta * (1 - theta) * t(H) %*% H
    dev <- (st - ex)/sqrt(diag(cv))
    bot <- 1/sqrt(outer(diag(cv), diag(cv), "*"))
    cr <- cv * bot
    mx <- max(dev)
    pval <- 1 - mvtnorm::pmvnorm(lower = rep(-Inf, k), upper = rep(mx,
        k), cor = cr, algorithm=mvtnorm::Miwa(steps=512))[[1]]
    rownames(cr)<-colnames(H)
    colnames(cr)<-colnames(H)
    names(dev)<-colnames(H)
    list(pval = pval, dev = dev, cor = cr)
}
