dispV<-c(0,0.0001,0.0005,0.001,0.005,0.01,0.05,0.1,0.5,1)
ratioV<-seq(0.1,1,by=0.1)
for(disp in dispV){
  for(ratio in ratioV){
    species<-3
    patches<-2
    spec_ID<-c("plant","plant","herbivore")
    
    environments<-c(1,2)
    env_optima<-c(1,2,0)
    
    growth_rate<-(matrix(rep(env_optima,patches),nrow=species)==matrix(rep(environments,each=species),nrow=species))*0.2
    growth_rate[spec_ID=="herbivore",]<--0.3
    
    strong_benefit<-1.1
    sp_interactions<-matrix(c(-0.2,0,strong_benefit*ratio,
                              0,-0.2,strong_benefit,
                              -2,-2,0),nrow=species)
    
    Tmax<-1000
    N<-array(c(1,1,0.02),dim=c(species,patches,Tmax))
    
    #disp<-0
    
    dispTF<-c(F,F,T)
    
    disp_mat<-matrix(c(0,1,1,0),nrow=patches)
    
    for(ts in 1:(Tmax-1)){
      N[,,ts+1]<-N[,,ts]*exp(growth_rate+sp_interactions%*%N[,,ts])
      N[,,ts+1]<-N[,,ts+1]+disp*(N[,,ts+1]*dispTF)%*%disp_mat-N[,,ts+1]*dispTF*disp
      N[,,ts]<-rnorm(n=species*patches,mean = N[,,ts],sd=N[,,ts]*0.01)
      N[N<10^-4]<-0
    }
    
    par(mfrow=c(2,1))
    matplot(t(N[,1,]), type='l', lty=1,main=ratio)
    matplot(t(N[,2,]), type='l',lty=1,main=disp)
    
    hold<-data.frame(Species=sum(rowSums(N[,,ts])>0.1),Biomass=sum(N[,,ts]),Pred_biomass=sum(N[3,,ts]),ratio=ratio,dispersal=disp)
    if(disp == dispV[1] & ratio == ratioV[1]){
      results.df<-hold
    } else {
      results.df<-rbind(results.df,hold)
    }
  }
}

library(ggplot2)
library(viridis)
ggplot(results.df,aes(y=ratio,x=dispersal,fill=Species))+
  geom_tile()+
  scale_color_viridis()+
  scale_x_log10()

ggplot(results.df,aes(y=ratio,x=dispersal,fill=Pred_biomass))+
  geom_tile()+
  scale_color_viridis()+
  scale_x_log10()

ggplot(results.df,aes(y=ratio,x=dispersal,fill=Biomass))+
  geom_tile()+
  scale_color_viridis()+
  scale_x_log10()
