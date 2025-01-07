#This file contains the functions for boosting the e-values used in the paper 
#"An online generalization of the (e-)Benjamini-Hochberg procedure"

###Boosting as in Example 1 with + adjustment
##Input:
#s:       cutoff value for the truncation function, natural number.
#alpha:   overall significance level, real number between 0 and 1.
#gamma:   weighting sequence, n-dim. vector of real numbers between 0 and 1 with sum less than or equal to 1.
#delta:   Parameter used for boosting the e-values, positive real number. In case of the simple null 
#         H_i:X_i~N(mu_N,1) vs. alternative H_i^A:X_i~N(mu_A,1) delta should be set to delta=mu_A-mu_N.

##Output: boosting factor.

e_boosted_plus=function(s, alpha, gamma, delta){
b_factor=function(b){
    return(b*(1-pnorm(delta/2+log(s*alpha*gamma*b)/delta))+
             sum((pnorm(delta/2-log((0:(s-1))*alpha*gamma*b)/delta)- pnorm(delta/2-log((1:s)*alpha*gamma*b)/delta))/((1:s)*alpha*gamma))-1)
}
return(uniroot(b_factor, lower=0, upper=10000)$root)
}

###Boosting as in Example 1 with - adjustment
##Input:
#s:       cutoff value for the truncation function, natural number.
#alpha:   overall significance level, real number between 0 and 1.
#gamma:   weighting sequence, n-dim. vector of real numbers between 0 and 1 with sum less than or equal to 1.
#delta:   Parameter used for boosting the e-values, positive real number. In case of the simple null 
#         H_i:X_i~N(mu_N,1) vs. alternative H_i^A:X_i~N(mu_A,1) delta should be set to delta=mu_A-mu_N.

##Output: boosting factor.

e_boosted_minus=function(s, alpha, gamma, delta){
  b_factor=function(b){
    return(sum((pnorm(delta/2-log((0:(s-1))*alpha*gamma*b)/delta)- pnorm(delta/2-log((1:s)*alpha*gamma*b)/delta))/((1:s)*alpha*gamma))-1)
  }
  return(uniroot(b_factor, lower=0, upper=1000000)$root)
}

###Boosting as in Example 2 with + adjustment
##Input:
#s:       cutoff value for the truncation function, natural number.
#alpha:   overall significance level, real number between 0 and 1.
#gamma:   weighting sequence, n-dim. vector of real numbers between 0 and 1 with sum less than or equal to 1.
#delta:   Parameter used for boosting the e-values, positive real number. In case of the simple null 
#         H_i:X_i~N(mu_N,1) vs. alternative H_i^A:X_i~N(mu_A,1) delta should be set to delta=mu_A-mu_N.
#num_rej: number of previous (independent) rejections.

##Output: boosting factor.

e_boosted_plus_local=function(s, alpha, gamma, delta, num_rej){
  k=num_rej+1
  b_factor=function(b){
    return(b*(1-pnorm(delta/2+log(s*alpha*gamma*b)/delta))+
             sum((pnorm(delta/2-log((k:(s-1))*alpha*gamma*b)/delta)- pnorm(delta/2-log(((k+1):s)*alpha*gamma*b)/delta))/(((k+1):s)*alpha*gamma))+
             (1- pnorm(delta/2-log(k*alpha*gamma*b)/delta))/(k*alpha*gamma)-1)
  }
  return(uniroot(b_factor, lower=0, upper=10000)$root)
}

###Boosting as in Example 2 with - adjustment
##Input:
#s:       cutoff value for the truncation function, natural number.
#alpha:   overall significance level, real number between 0 and 1.
#gamma:   weighting sequence, n-dim. vector of real numbers between 0 and 1 with sum less than or equal to 1.
#delta:   Parameter used for boosting the e-values, positive real number. In case of the simple null 
#         H_i:X_i~N(mu_N,1) vs. alternative H_i^A:X_i~N(mu_A,1) delta should be set to delta=mu_A-mu_N.
#num_rej: number of previous (independent) rejections.

##Output: boosting factor.

e_boosted_minus_local=function(s, alpha, gamma, delta, num_rej){
  k=num_rej+1
  b_factor=function(b){
    return(sum((pnorm(delta/2-log((k:(s-1))*alpha*gamma*b)/delta)- pnorm(delta/2-log(((k+1):s)*alpha*gamma*b)/delta))/(((k+1):s)*alpha*gamma))+
             (1- pnorm(delta/2-log(k*alpha*gamma*b)/delta))/(k*alpha*gamma)-1)
  }
  return(uniroot(b_factor, lower=0, upper=1000000)$root)
}

