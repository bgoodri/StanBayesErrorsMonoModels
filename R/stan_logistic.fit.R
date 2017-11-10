

#' @rdname stan_logistic
#' @export

stan_logistic.fit <- function(dat_sav,xgrid) {
  
  xobs=dat_sav$xobs
  yobs=dat_sav$yobs
  xcensl=dat_sav$xcensl
  xcensu=dat_sav$xcensu
  ycensl=dat_sav$ycensl
  ycensu=dat_sav$ycensu
  

  ### Initialize
  parms=initialize_parms_log(xobs,yobs,xcensu,xcensl)
  coefs=parms$coefs
  xtrue=parms$xtrue
  
  ### Run Stan Model
  dat=list(y=yobs,xgrid=xgrid,Ngrid=Ngrid,n_groups=5,ysig=ysig,N=N,xobs=xobs,xsig=xsig,
           xcensl=xcensl,xcensu=xcensu,ycensl=ycensl,ycensu=ycensu)
  init_fun <- function() {list(mu=seq(min(xtrue)+1,max(xtrue)-1,length=5),sigma=rep(1,5),
            Theta=rep(1/5,5),xtrue=xtrue,coef=coefs)}
  stanfit <- stanmodels$logistic
  fit <- rstan::sampling(stanfit, data = dat, iter = 1000,chains = 1,thin=2,init=init_fun)
  parms <- extract(fit, pars = c("MIC_Dens","gx"), inc_warmup = FALSE, permuted = FALSE)
  
  return(parms)
}
