

#' @rdname stan_logistic
#' @export

stan_logistic.fit <- function(xobs,yobs,xcens,ycens,xgrid) {
  
  ### Set up data
  Ngrid=length(xgrid)
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

  ### Initialize
  parms=initialize_parms_log(xobs,yobs,xcensu,xcensl)
  coefs=parms$coefs
  print(coefs)
  xtrue=parms$xtrue
  
  ### Run Stan Model
  dat=list(y=yobs,xgrid=xgrid,Ngrid=Ngrid,n_groups=5,ysig=ysig,N=N,xobs=xobs,xsig=xsig,
           xcensl=xcensl,xcensu=xcensu,ycensl=ycensl,ycensu=ycensu)
  init_fun <- function() {list(mu=seq(min(xtrue)+1,max(xtrue)-1,length=5),sigma=rep(1,5),
            Theta=rep(1/5,5),xtrue=xtrue,coef=coefs)}
  stanfit <- stanmodels$logistic

  print(init_fun())
  plot(xtrue,yobs)
  fit <- rstan::sampling(stanfit, data = dat, iter = 1000,chains = 1,thin=2,init=init_fun)
  # # sampling_args <- set_sampling_args(
  # #   object = stanfit, 
  # #   init=init_fun,
  # #   pars = "beta", 
  # #   show_messages = FALSE)
  # # stanfit <- do.call(sampling, sampling_args)
  # 
  # thetas <- extract(fit, pars = "beta", inc_warmup = TRUE, permuted = FALSE)
  
  return(1)

}
