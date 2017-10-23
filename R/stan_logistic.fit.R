
# Part of the rstanarm package for estimating model parameters
# Copyright (C) 2015, 2016, 2017 Trustees of Columbia University
# 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

#' @rdname stan_polr
#' @export
#' @param x A design matrix.
#' @param y A response variable, which must be a (preferably ordered) factor.
#' @param wt A numeric vector (possibly \code{NULL}) of observation weights.
#' @param offset A numeric vector (possibly \code{NULL}) of offsets.
#' 
#' @importFrom utils head tail
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

