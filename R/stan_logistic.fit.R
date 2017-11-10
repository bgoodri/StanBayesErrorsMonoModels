
stan_logistic.fit <- function(xobs,yobs,xcens,ycens) {
  
  ### Initialize
  xsig=.707; ysig=2.121
  Ngrid=1000
  N=length(xobs)
  xcensl=rep(0,N)
  xcensl[xcens==-1] = 1
  xcensu=rep(0,N)
  xcensu[xcens==1] = 1
  ycensl=rep(0,N)
  ycensl[ycens==-1] = 1
  ycensu=rep(0,N)
  ycensu[ycens==1] = 1
  
  
  dat_sav=data.frame(xobs,yobs,xcensl,xcensu,ycensu,ycensl)
  
  if(sum(xcensu)>0 & sum(xcensl)>0){
    xgrid=seq(min(xobs)-4,max(xobs)+4,length=1000)
  }else if(sum(xcensu)>0 & sum(xcensl)==0){
    xgrid=seq(min(xobs)-1,max(xobs)+4,length=1000)
  }else if(sum(xcensu)==0 & sum(xcensl)>0){
    xgrid=seq(min(xobs)-4,max(xobs)+1,length=1000)
  }else{
    xgrid=seq(min(xobs)-1,max(xobs)+1,length=1000)
  }
  
  parms=initialize_parms_log(xobs,yobs,xcensu,xcensl)
  coefs=parms$icoefs1
  xtrue=parms$xtrue
  
  
  ### Stan
  
  dat=list(y=yobs,xgrid=xgrid,Ngrid=Ngrid,n_groups=5,ysig=ysig,N=N,xobs=xobs,xsig=xsig,
           xcensl=xcensl,xcensu=xcensu,ycensl=ycensl,ycensu=ycensu)
  init_fun <- function() {list(mu=seq(min(xtrue)+1,max(xtrue)-1,length=5),sigma=rep(1,5),
                               Theta=rep(1/5,5),xtrue=xtrue,coef=coefs)}
  fit <- sampling(stanmodels$logistic, data = dat, iter = 1000,chains = 1,thin=2,
                  init=init_fun,verbose=TRUE)
  
  return(fit)

}

