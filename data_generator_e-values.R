#Generates the results for e-value based simulations in the paper 
#"An online generalization of the (e-)Benjamini-Hochberg procedure"

library(ggplot2)
library(MASS)
library(patchwork)

###load the procedures
source("procedures.R")
source("boosted_e_vals.R")

###Gaussian testing problem for exact p-values
m=100     #Number of Trials
n=1000    #Number of Hypotheses per Trial
mu_A=4.5    #Strength of the alternative
mu_N=0    #Conservativeness of null p-values (<0 for conservative null p-values)
#pi_A is defined in the loop below

###Initialise Hyperparameters
q=0.99
gamma=q^(1:n)*(1-q)/q
alpha=0.05

#Batch-size for dependence structure
batch_size=20
batch_number=ceiling(n/batch_size)
corr=0.5
sigma=matrix(corr,batch_size,batch_size)+diag((1-corr),batch_size)

lags=rep(seq(0,(batch_size-1),1),batch_number)
lags=lags[1:n]

###Set seed to make the results reproducible
set.seed(12345)

power_online_e_bh=rep(0,9)
power_online_e_bh_boosted=rep(0,9)
power_online_e_bh_boosted_local=rep(0,9)
power_e_lond=rep(0,9)
FDR_online_e_bh=rep(0,9)
FDR_online_e_bh_boosted=rep(0,9)
FDR_online_e_bh_boosted_local=rep(0,9)
FDR_e_lond=rep(0,9)

###Generate p-values
for(l in 1:9){
  print(l)
  ##Fast way for p-values in batches.
  pi_A=l/10
  e=matrix(,nrow=n,ncol=m)
  hypo=matrix(,nrow=n,ncol=m)
  for(j in 1:m){
    hypo[,j]=rbinom(n,1,pi_A)
    X=rep(0,batch_number*batch_size)
    for(k in 1:batch_number){
      X[((k-1)*batch_size+1):(k*batch_size)]=mvrnorm(1,rep(0, batch_size),sigma)
    }
    X=X[1:n]
    Z=mu_N*(1-hypo[,j])+mu_A*hypo[,j]+X
    e[,j]=dnorm(Z,mu_A,1)/dnorm(Z,0,1)
  }
  
##e-LOND
FDR=rep(0,m)    #FDR within each trial
power=rep(0,m)  #Power within each trial
  
for(j in 1:m){
  rejects=e_lond(alpha, gamma, e[,j], n)
  V=(hypo[,j]==0 & rejects==1)
  D=(hypo[,j]==1 & rejects==1)
  power[j]=sum(D)/sum(hypo[,j])
  FDR[j]=sum(V)/sum(rejects)
}
FDR_e_lond[l]=mean(FDR)
power_e_lond[l]=mean(power)  

##Online e-BH
FDR=rep(0,m)    #FDR within each trial
power=rep(0,m)  #Power within each trial

for(j in 1:m){
  rejects=online_e_bh(alpha, gamma, e[,j], n)
  V=(hypo[,j]==0 & rejects==1)
  D=(hypo[,j]==1 & rejects==1)
  power[j]=sum(D)/sum(hypo[,j])
  FDR[j]=sum(V)/sum(rejects)
}
FDR_online_e_bh[l]=mean(FDR)
power_online_e_bh[l]=mean(power)

##Boosted online e-BH
FDR=rep(0,m)    #FDR within each trial
power=rep(0,m)  #Power within each trial

for(j in 1:m){
  rejects=online_e_bh_boosted(alpha, gamma, e[,j], n, 1000, mu_A-mu_N)
  V=(hypo[,j]==0 & rejects==1)
  D=(hypo[,j]==1 & rejects==1)
  power[j]=sum(D)/sum(hypo[,j])
  FDR[j]=sum(V)/sum(rejects)
}
FDR_online_e_bh_boosted[l]=mean(FDR)
power_online_e_bh_boosted[l]=mean(power)

##Boosted online e-BH under local dependence
FDR=rep(0,m)    #FDR within each trial
power=rep(0,m)  #Power within each trial

for(j in 1:m){
  rejects=online_e_bh_boosted_local(alpha, gamma, e[,j], n, 1000, mu_A-mu_N, lags)
  V=(hypo[,j]==0 & rejects==1)
  D=(hypo[,j]==1 & rejects==1)
  power[j]=sum(D)/sum(hypo[,j])
  FDR[j]=sum(V)/sum(rejects)
}
FDR_online_e_bh_boosted_local[l]=mean(FDR)
power_online_e_bh_boosted_local[l]=mean(power)

}

###save data

results_df=data.frame(pi_A=seq(0.1,0.9,0.1), power_e_lond, power_online_e_bh, 
                      power_online_e_bh_boosted, power_online_e_bh_boosted_local, FDR_e_lond, 
                      FDR_online_e_bh, FDR_online_e_bh_boosted, FDR_online_e_bh_boosted_local)

save(results_df, file="results/e-val_strong_signal.rda")

###Weak signal

###Gaussian testing problem for exact p-values
mu_A=3.5    #Strength of the alternative

###Set seed to make the results reproducible
set.seed(12345)

power_online_e_bh=rep(0,9)
power_online_e_bh_boosted=rep(0,9)
power_online_e_bh_boosted_local=rep(0,9)
power_e_lond=rep(0,9)
FDR_online_e_bh=rep(0,9)
FDR_online_e_bh_boosted=rep(0,9)
FDR_online_e_bh_boosted_local=rep(0,9)
FDR_e_lond=rep(0,9)

###Generate p-values
for(l in 1:9){
  print(l)
  ##Fast way for p-values in batches.
  pi_A=l/10
  e=matrix(,nrow=n,ncol=m)
  hypo=matrix(,nrow=n,ncol=m)
  for(j in 1:m){
    hypo[,j]=rbinom(n,1,pi_A)
    X=rep(0,batch_number*batch_size)
    for(k in 1:batch_number){
      X[((k-1)*batch_size+1):(k*batch_size)]=mvrnorm(1,rep(0, batch_size),sigma)
    }
    X=X[1:n]
    Z=mu_N*(1-hypo[,j])+mu_A*hypo[,j]+X
    e[,j]=dnorm(Z,mu_A,1)/dnorm(Z,0,1)
  }
  
  ##e-LOND
  FDR=rep(0,m)    #FDR within each trial
  power=rep(0,m)  #Power within each trial
  
  for(j in 1:m){
    rejects=e_lond(alpha, gamma, e[,j], n)
    V=(hypo[,j]==0 & rejects==1)
    D=(hypo[,j]==1 & rejects==1)
    power[j]=sum(D)/sum(hypo[,j])
    FDR[j]=sum(V)/sum(rejects)
  }
  FDR_e_lond[l]=mean(FDR)
  power_e_lond[l]=mean(power)  
  
  ##Online e-BH
  FDR=rep(0,m)    #FDR within each trial
  power=rep(0,m)  #Power within each trial
  
  for(j in 1:m){
    rejects=online_e_bh(alpha, gamma, e[,j], n)
    V=(hypo[,j]==0 & rejects==1)
    D=(hypo[,j]==1 & rejects==1)
    power[j]=sum(D)/sum(hypo[,j])
    FDR[j]=sum(V)/sum(rejects)
  }
  FDR_online_e_bh[l]=mean(FDR)
  power_online_e_bh[l]=mean(power)
  
  ##Boosted online e-BH
  FDR=rep(0,m)    #FDR within each trial
  power=rep(0,m)  #Power within each trial
  
  for(j in 1:m){
    rejects=online_e_bh_boosted(alpha, gamma, e[,j], n, 1000, mu_A-mu_N)
    V=(hypo[,j]==0 & rejects==1)
    D=(hypo[,j]==1 & rejects==1)
    power[j]=sum(D)/sum(hypo[,j])
    FDR[j]=sum(V)/sum(rejects)
  }
  FDR_online_e_bh_boosted[l]=mean(FDR)
  power_online_e_bh_boosted[l]=mean(power)
  
  ##Boosted online e-BH under local dependence
  FDR=rep(0,m)    #FDR within each trial
  power=rep(0,m)  #Power within each trial
  
  for(j in 1:m){
    rejects=online_e_bh_boosted_local(alpha, gamma, e[,j], n, 1000, mu_A-mu_N, lags)
    V=(hypo[,j]==0 & rejects==1)
    D=(hypo[,j]==1 & rejects==1)
    power[j]=sum(D)/sum(hypo[,j])
    FDR[j]=sum(V)/sum(rejects)
  }
  FDR_online_e_bh_boosted_local[l]=mean(FDR)
  power_online_e_bh_boosted_local[l]=mean(power)
  
}

###save data

results_df=data.frame(pi_A=seq(0.1,0.9,0.1), power_e_lond, power_online_e_bh, 
                      power_online_e_bh_boosted, power_online_e_bh_boosted_local, FDR_e_lond, 
                      FDR_online_e_bh, FDR_online_e_bh_boosted, FDR_online_e_bh_boosted_local)

save(results_df, file="results/e-val_weak_signal.rda")

