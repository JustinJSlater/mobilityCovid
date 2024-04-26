data{
  int T;
  int cases[T];
  real shifted_cases1[T];
  
  real mobility1[T];
  real mobility2[T];

}

parameters{
  real<lower=0> nu;
  real<lower=0> alpha;
  real<lower=0> tau[2];
}

transformed parameters{
  real<lower=0> lambda[T];
  real<lower=0> phi[T];
  
  for (t in 1:T){
    phi[t] =alpha + tau[1]*mobility1[t] + tau[2]*mobility2[t];

    lambda[t] = nu+phi[t]*(shifted_cases1[t]);
  }
  
}

model{
  target+= poisson_lpmf(cases|lambda);
  
 alpha~normal(0,10);
 tau ~ normal(0, 0.1);
 nu ~normal(0,10);
}

generated quantities{
  real<lower=0> ep_move[T];
  real<lower=0> ep_ar[T];

  for (t in 1:T){
    ep_ar[t] = alpha*(shifted_cases1[t]);
    ep_move[t] = tau[1]*mobility1[t] * shifted_cases1[t]+
                 tau[2]*mobility2[t] * shifted_cases1[t];
  }

}

