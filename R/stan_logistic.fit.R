

#' @rdname stan_logistic
#' @export
#' @param x A design matrix.
#' @param y A response variable, which must be a (preferably ordered) factor.
#' @param wt A numeric vector (possibly \code{NULL}) of observation weights.
#' @param offset A numeric vector (possibly \code{NULL}) of offsets.
#' 
#' @importFrom utils head tail
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
  xtrue=parms$xtrue
  
  ### Run Stan Model
  dat=list(y=yobs,xgrid=xgrid,Ngrid=Ngrid,n_groups=5,ysig=ysig,N=N,xobs=xobs,xsig=xsig,
           xcensl=xcensl,xcensu=xcensu,ycensl=ycensl,ycensu=ycensu)
  init_fun <- function() {list(mu=seq(min(xtrue)+1,max(xtrue)-1,length=5),sigma=rep(1,5),
            Theta=rep(1/5,5),xtrue=xtrue,coef=coefs)}
  fit <- sampling(compiled_model, data = dat, iter = 1000,chains = 1,thin=2,init=init_fun)
  
  thetas <- extract(fit, pars = "beta", inc_warmup = TRUE, permuted = FALSE)

}

                                                                                                       ]]]]]]]]]]
# internal ----------------------------------------------------------------

# Create "prior.info" attribute needed for prior_summary()
#
# @param prior, prior_counts User's prior and prior_counts specifications
# @return A named list with elements 'prior' and 'prior_counts' containing 
#   the values needed for prior_summary
summarize_polr_prior <- function(prior, prior_counts, shape=NULL, rate=NULL) {
  flat <- !length(prior)
  prior_list <- list(
    prior = list(
      dist = ifelse(flat, NA, "R2"),
      location = ifelse(flat, NA, prior$location),
      what = ifelse(flat, NA, prior$what)
    ), 
    prior_counts = list(
      dist = "dirichlet",
      concentration = prior_counts
    )
  )
  if ((!is.null(shape) && shape > 0) && (!is.null(rate) && rate > 0))
    prior_list$scobit_exponent <- list(dist = "gamma", shape = shape, rate = rate)
  
  return(prior_list)
}
