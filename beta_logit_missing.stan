//
// Joint logistic-beta regression model for analying experimental outcomes
// with proportion and degenerate responses (i.e. 0 and 1)
// Models 0/1 with logit and (0,1) with beta regression
// Robert Kubinec
// New York University Abu Dhabi
data {
  int<lower=0> N_prop;
  int<lower=0> N_degen;
  int X; //. predictors
  int X_miss; // predictors on missing vars
  vector[N_prop] outcome_prop;
  int outcome_degen[N_degen];
  matrix[N_prop,X] covar_prop;
  matrix[N_prop,X_miss] covar_miss;
  matrix[N_degen,X] covar_degen;
}
transformed data {
  int half[N_prop];
  
  for(n in 1:N_prop) {
    if(outcome_prop[n]>0.48 || outcome_prop[n]<0.52) {
      half[n]=1;
    } else {
      half[n]=0;
    }
  }
}
parameters {
  real alpha; 
  real alpha_miss;
  vector[X] X_beta;
  vector[X_miss] X_miss_beta;
  real<lower=0> kappa; // scale parameter for beta regression
}
model {
  vector[N_prop] prop_vals;
  //vector[N_prop] prop_vals_miss;
  alpha ~ normal(0,5);
  alpha_miss ~ normal(0,5);
  X_miss_beta ~ normal(0,5);
  X_beta ~ normal(0,5);
  kappa ~ exponential(1);
  
  for(n in 1:N_prop) {
    prop_vals[n]=inv_logit(alpha + covar_prop[n,]*X_beta);
    //prop_vals_miss[n]=inv_logit(alpha_miss + covar_prop[n,]*X_beta + covar_miss[n,]*X_miss_beta);
    if(half[n]==1) {
      target += log_sum_exp(bernoulli_logit_lpmf(1|alpha_miss + covar_prop[n,]*X_beta + covar_miss[n,]*X_miss_beta),
                              bernoulli_logit_lpmf(0|alpha_miss + covar_prop[n,]*X_beta + covar_miss[n,]*X_miss_beta) +
                              beta_proportion_lpdf(0.5|prop_vals[n],kappa));
    } else {
      target += beta_proportion_lpdf(outcome_prop[n]|prop_vals[n],kappa);
      target += bernoulli_logit_lpmf(0|alpha_miss + covar_prop[n,]*X_beta + covar_miss[n,]*X_miss_beta);
    }
    
  }
  outcome_prop ~ beta_proportion(prop_vals,kappa);
  outcome_degen ~ bernoulli_logit_glm(covar_degen,alpha,X_beta);
}

