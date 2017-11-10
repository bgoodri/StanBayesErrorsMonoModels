diagnostics=function(fit,xgrid,dat_sav){
  
  
  xobs=dat_sav$xobs
  yobs=dat_sav$yobs
  xcensl=dat_sav$xcensl
  xcensu=dat_sav$xcensu
  ycensl=dat_sav$ycensl
  ycensu=dat_sav$ycensu
  
  xobs1=xobs
  yobs1=yobs
  xobs[xcensu==1 & xobs==max(xobs)]=max(xobs)+1
  xobs[xcensl==1 & xobs==min(xobs)]=min(xobs)-1
  yobs[ycensu==1 & yobs==max(yobs)]=max(yobs)+1
  yobs[ycensl==1 & yobs==min(yobs)]=min(yobs)-1
  a1=data.frame(table(xobs,yobs))
  a1$xobs=as.numeric(as.character(a1$xobs))
  a1$yobs=as.numeric(as.character(a1$yobs))
  a1=a1[a1$Freq>0,]


  
    
  ### extract draws
  list_of_draws <- extract(fit)
  
  ### MIC Density
  MIC_Dens=list_of_draws$MIC_Dens
  densDat=data_frame(xgrid,y=apply(MIC_Dens,2,mean))
  lower_dens=data_frame(xgrid,y=apply(MIC_Dens,2,function(x) quantile(x,probs=c(.025))))
  upper_dens=data_frame(xgrid,y=apply(MIC_Dens,2,function(x) quantile(x,probs=c(.975))))
  densDat$y=densDat$y/sum(densDat$y)
  lower_dens$y=lower_dens$y/sum(lower_dens$y)
  upper_dens$y=upper_dens$y/sum(upper_dens$y)
  tmp=data.frame(xgrid=xgrid,densTrue=densTrue)

  plt1=ggplot(densDat,aes(x=xgrid,y))+geom_line()+
    geom_line(data=lower_dens,aes(x=xgrid,y=y,label=NULL),linetype=2,color='deepskyblue4')+
    geom_line(data=upper_dens,aes(x=xgrid,y=y,label=NULL),linetype=2,color='deepskyblue4')+
    geom_line(data=tmp,aes(x=xgrid,y=densTrue),linetype=2,color='red')+
    scale_x_continuous(limits=c(min(xobs)-1,max(xobs)+1),breaks = seq(min(xobs)-1,max(xobs)+1,by=1))+
    theme_fivethirtyeight()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text=element_text(size=11),
          axis.title=element_text(size=11),
          plot.title=element_text(size=15))+
    labs(title='',y='',x=expression(MIC~(log["2"]~ug/mL)))
  plot(plt1)
  
  ### MIC/DIA Relationship
  gx=list_of_draws$gx
  densDat=data_frame(xgrid,MIC_Dens_mean=apply(MIC_Dens,2,mean))
  fitMAPDAT=data_frame(xgrid,gx_mean=apply(gx,2,mean))
  fit025DAT=data_frame(xgrid,gx_lower=apply(gx,2,function(x) quantile(x,probs=c(.025))))
  fit975DAT=data_frame(xgrid,gx_upper=apply(gx,2,function(x) quantile(x,probs=c(.975))))
  tmp=data.frame(x=xgrid,y=trueFit)
  
  plt2=ggplot(a1,aes(x=xobs,y=yobs,label=Freq))+geom_text(size=3.2,color='black')+
    geom_line(data=fitMAPDAT,aes(x=xgrid,y=gx_mean,label=NULL),color='deepskyblue4')+
    geom_line(data=fit025DAT,aes(x=xgrid,y=gx_lower,label=NULL),linetype=2,color='deepskyblue4')+
    geom_line(data=fit975DAT,aes(x=xgrid,y=gx_upper,label=NULL),linetype=2,color='deepskyblue4')+
    geom_line(data=tmp,aes(x=xgrid,y=y,label=NULL),linetype=2,color='red')+
    scale_x_continuous(breaks = seq(min(xobs1)-1,max(xobs1)+1,by=1),
            labels = c(paste("<",min(xobs1),sep=''),seq(min(xobs1),max(xobs1),by=1), paste(">",max(xobs1),sep='')),
            limits = c(min(xobs1)-1,max(xobs1)+1))+
    scale_y_continuous(breaks = seq(min(yobs1)-1,max(yobs1)+1,by=1),
            labels = c(paste("<",min(yobs1),sep=''),seq(min(yobs1),max(yobs1),by=1), paste(">",max(yobs1),sep='')),
            limits = c(min(yobs1)-1,max(yobs1)+1))+
    theme_fivethirtyeight()+
    theme(axis.line = element_line(colour = "black"),
          legend.position='none',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x=element_text(size=11),
          axis.text.y=element_text(size=8),
          axis.title=element_text(size=11),
          plot.title=element_text(size=15))+
    labs(title='Logistic Model',y='DIA (mm)',x="")
  plot(plt2)
  

  par(mfrow=c(2,1))
  xtrue_sav=list_of_draws$xtrue
  xtrue_mean=apply(xtrue_sav,2,mean)
  hist(xtrue_mean,freq=FALSE)
  hist(xobs,freq=FALSE)
  
  par(mfrow=c(1,1))
  
  
  return(list(plt1=plt1,plt2=plt2))
  
}
