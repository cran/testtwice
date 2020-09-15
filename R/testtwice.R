testtwice<-
  function(y,dose=NULL,gamma=1,u858=FALSE,u888=FALSE,
           u878=FALSE,u868=FALSE,u867=FALSE,u222=FALSE,
           brown=FALSE,noether=FALSE,tailored=FALSE,
           alternative="greater",do.test=TRUE){
    stopifnot(is.vector(y))
    stopifnot((alternative=="greater")|(alternative=="less"))
    if (alternative=="less") y<-(-y)
    if (!is.null(dose)) {
      stopifnot(is.vector(dose))
      stopifnot(length(y)==length(dose))
      stopifnot(min(dose)>=0)
    }
    stopifnot(length(gamma)==1)
    stopifnot(gamma>=1)
    ops<-c(u858,u888,u867,u878,u868,u222,brown,noether,tailored)
    stopifnot(all(is.logical(ops)))
    if (all(!ops)){
      u858<-TRUE
      u878<-TRUE
    }
    else stopifnot(sum(ops)>=1)

    ny<-length(y)
    if (is.null(dose)) dose<-rep(1,ny)
    H<-matrix(NA,ny,0)

    if (u858) {
      U858<-multrnk(y,m1=5,m2=8,m=8)*dose
      H<-cbind(H,U858)
      rm(U858)
    }

    if (u888) {
      U888<-multrnk(y,m1=8,m2=8,m=8)*dose
      H<-cbind(H,U888)
      rm(U888)
    }

    if (u878) {
      U878<-multrnk(y,m1=7,m2=8,m=8)*dose
      H<-cbind(H,U878)
      rm(U878)
    }

    if (u868) {
      U868<-multrnk(y,m1=6,m2=8,m=8)*dose
      H<-cbind(H,U868)
      rm(U868)
    }

    if (u867) {
      U867<-multrnk(y,m1=6,m2=7,m=8)*dose
      H<-cbind(H,U867)
      rm(U867)
    }

    if (u222) {
      U222<-multrnk(y,m1=2,m2=2,m=2)*dose
      H<-cbind(H,U222)
      rm(U222)
    }

    if (brown) {
      Brown<-bmhranks(y, q1 = 1/3, q2 = 2/3)*dose
      H<-cbind(H,Brown)
      rm(Brown)
    }

    if (noether) {
      Noether<-bmhranks(y, q1 = 2/3, q2 = 2/3)*dose
      H<-cbind(H,Noether)
      rm(Noether)
    }

    if (tailored) {
      rk<-rank(abs(y))/length(y)
      Tailored<-((rk>=.5)+5*(rk>=(2/3))+5*(rk>=(5/6)))-5*(rk>=.975)
      Tailored<-Tailored*dose
      H<-cbind(H,Tailored)
      rm(Tailored,rk)
    }

    if (do.test) {
      stopifnot(2<=(dim(H)[2]))
      tt(y,H,gamma=gamma)
    }
    else {
      if (2<=(dim(H)[2])) H
      else as.vector(H)
      }
  }
