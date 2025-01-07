#Generates the results for p-value based simulations in the paper 
#"An online generalization of the (e-)Benjamini-Hochberg procedure"

library(ggplot2)
library(patchwork)
library(onlineFDR)

###load the procedures
source("procedures_new.R")
source("boosted_e_vals.R")

###Gaussian testing problem for exact p-values
m=100     #Number of Trials
n=1000    #Number of Hypotheses per Trial
mu_A=3.5    #Strength of the alternative
mu_N=0    #Conservativeness of null p-values (<0 for conservative null p-values)
#pi_A is defined in the loop below

###Initialise Hyperparameters
q=0.99
gamma=q^(1:n)*(1-q)/q
alpha=0.05
lambda=0.5

###Set seed to make the results reproducible
set.seed(12345)

power_online_bh=rep(0,9)
power_online_storey_bh=rep(0,9)
power_saffron=rep(0,9)
power_lord=rep(0,9)
FDR_online_bh=rep(0,9)
FDR_online_storey_bh=rep(0,9)
FDR_saffron=rep(0,9)
FDR_lord=rep(0,9)

###Generate p-values
for(l in 1:9){
  print(l)
  pi_A=l/10
  p=matrix(,nrow=n,ncol=m)
  hypo=matrix(,nrow=n,ncol=m)
  for(j in 1:m){
    hypo[,j]=rbinom(n,1,pi_A)
    X=rnorm(n)
    Z=mu_N*(hypo[,j]-1)*(-1)+mu_A*hypo[,j]+X
    p[,j]=pnorm(-Z)
  }
  
##Online BH
FDR=rep(0,m)    #FDR within each trial
power=rep(0,m)  #Power within each trial
  
for(j in 1:m){
  rejects=online_bh(alpha, gamma, p[,j], n)
  V=(hypo[,j]==0 & rejects==1)
  D=(hypo[,j]==1 & rejects==1)
  power[j]=sum(D)/sum(hypo[,j])
  FDR[j]=sum(V)/sum(rejects)
}
FDR_online_bh[l]=mean(FDR)
power_online_bh[l]=mean(power)  

##Online Storey-BH
FDR=rep(0,m)    #FDR within each trial
power=rep(0,m)  #Power within each trial

for(j in 1:m){
  rejects=online_storey_bh(alpha, gamma, lambda, p[,j], n)
  V=(hypo[,j]==0 & rejects==1)
  D=(hypo[,j]==1 & rejects==1)
  power[j]=sum(D)/sum(hypo[,j])
  FDR[j]=sum(V)/sum(rejects)
}
FDR_online_storey_bh[l]=mean(FDR)
power_online_storey_bh[l]=mean(power)

##LORD
FDR=rep(0,m)    #FDR within each trial
power=rep(0,m)  #Power within each trial

for(j in 1:m){
  rejects_df=LORD(alpha=alpha, gammai=gamma, d=p[,j])
  rejects=rejects_df$R
  V=(hypo[,j]==0 & rejects==1)
  D=(hypo[,j]==1 & rejects==1)
  power[j]=sum(D)/sum(hypo[,j])
  FDR[j]=sum(V)/sum(rejects)
}
FDR_lord[l]=mean(FDR)
power_lord[l]=mean(power)

##SAFFRON
FDR=rep(0,m)    #FDR within each trial
power=rep(0,m)  #Power within each trial

for(j in 1:m){
  rejects_df=SAFFRON(alpha=alpha, gammai=gamma, d=p[,j], lambda=lambda)
  rejects=rejects_df$R
  V=(hypo[,j]==0 & rejects==1)
  D=(hypo[,j]==1 & rejects==1)
  power[j]=sum(D)/sum(hypo[,j])
  FDR[j]=sum(V)/sum(rejects)
}
FDR_saffron[l]=mean(FDR)
power_saffron[l]=mean(power)

}

###save data

results_df=data.frame(pi_A=seq(0.1,0.9,0.1), power_online_bh, power_online_storey_bh, 
                      power_lord, power_saffron, FDR_online_bh, 
                      FDR_online_storey_bh, FDR_lord, FDR_saffron)

save(results_df, file="results/p-val_small_q.rda")



###q=0.999
q=0.999
gamma=q^(1:n)*(1-q)/q


###Set seed to make the results reproducible
set.seed(12345)

power_online_bh=rep(0,9)
power_online_storey_bh=rep(0,9)
power_saffron=rep(0,9)
power_lord=rep(0,9)
FDR_online_bh=rep(0,9)
FDR_online_storey_bh=rep(0,9)
FDR_saffron=rep(0,9)
FDR_lord=rep(0,9)

###Generate p-values
for(l in 1:9){
  print(l)
  pi_A=l/10
  p=matrix(,nrow=n,ncol=m)
  hypo=matrix(,nrow=n,ncol=m)
  for(j in 1:m){
    hypo[,j]=rbinom(n,1,pi_A)
    X=rnorm(n)
    Z=mu_N*(hypo[,j]-1)*(-1)+mu_A*hypo[,j]+X
    p[,j]=pnorm(-Z)
  }
  
  ##Online BH
  FDR=rep(0,m)    #FDR within each trial
  power=rep(0,m)  #Power within each trial
  
  for(j in 1:m){
    rejects=online_bh(alpha, gamma, p[,j], n)
    V=(hypo[,j]==0 & rejects==1)
    D=(hypo[,j]==1 & rejects==1)
    power[j]=sum(D)/sum(hypo[,j])
    FDR[j]=sum(V)/sum(rejects)
  }
  FDR_online_bh[l]=mean(FDR)
  power_online_bh[l]=mean(power)  
  
  ##Online Storey-BH
  FDR=rep(0,m)    #FDR within each trial
  power=rep(0,m)  #Power within each trial
  
  for(j in 1:m){
    rejects=online_storey_bh(alpha, gamma, lambda, p[,j], n)
    V=(hypo[,j]==0 & rejects==1)
    D=(hypo[,j]==1 & rejects==1)
    power[j]=sum(D)/sum(hypo[,j])
    FDR[j]=sum(V)/sum(rejects)
  }
  FDR_online_storey_bh[l]=mean(FDR)
  power_online_storey_bh[l]=mean(power)
  
  ##LORD
  FDR=rep(0,m)    #FDR within each trial
  power=rep(0,m)  #Power within each trial
  
  for(j in 1:m){
    rejects_df=LORD(alpha=alpha, gammai=gamma, d=p[,j])
    rejects=rejects_df$R
    V=(hypo[,j]==0 & rejects==1)
    D=(hypo[,j]==1 & rejects==1)
    power[j]=sum(D)/sum(hypo[,j])
    FDR[j]=sum(V)/sum(rejects)
  }
  FDR_lord[l]=mean(FDR)
  power_lord[l]=mean(power)
  
  ##SAFFRON
  FDR=rep(0,m)    #FDR within each trial
  power=rep(0,m)  #Power within each trial
  
  for(j in 1:m){
    rejects_df=SAFFRON(alpha=alpha, gammai=gamma, d=p[,j], lambda=lambda)
    rejects=rejects_df$R
    V=(hypo[,j]==0 & rejects==1)
    D=(hypo[,j]==1 & rejects==1)
    power[j]=sum(D)/sum(hypo[,j])
    FDR[j]=sum(V)/sum(rejects)
  }
  FDR_saffron[l]=mean(FDR)
  power_saffron[l]=mean(power)
  
}

###save data

results_df=data.frame(pi_A=seq(0.1,0.9,0.1), power_online_bh, power_online_storey_bh, 
                      power_lord, power_saffron, FDR_online_bh, 
                      FDR_online_storey_bh, FDR_lord, FDR_saffron)

save(results_df, file="results/p-val_large_q.rda")