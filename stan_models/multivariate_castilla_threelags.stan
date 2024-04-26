
data{
  int<lower=0> I; // Number of regions
  int<lower=0> T; // Number of time points
  int<lower=0> y[I,T]; //number of cases
  real<lower=0> shifted_y1[I,T];
  
  real<lower=0> shifted_weekly_mobility1 [I,I,T];
  real<lower=0> shifted_weekly_mobility2 [I,I,T];
  real<lower=0> shifted_weekly_mobility3 [I,I,T];
  real<lower=0> spatial_weights[I,I];
  int<lower=0> pops[I];
}

transformed data{
  real<lower = 0> infectious_trips1[I,I,T];
  real<lower = 0> infectious_trips2[I,I,T];
  real<lower = 0> infectious_trips3[I,I,T];
  matrix[I,T] mob_sum1;
  matrix[I,T] mob_sum2;
  matrix[I,T] mob_sum3;
  real<lower = 0> infectious_neighbors1[I,I,T];
  real<lower = 0> spat_sum1[I,T];
  
  for (t in 1:T){
    for (dest in 1:I){
      for (orig in 1:I){
        infectious_trips1[orig,dest,t] = shifted_weekly_mobility1[orig,dest,t]*shifted_y1[orig,t]/pops[orig];
        infectious_trips2[orig,dest,t] = shifted_weekly_mobility2[orig,dest,t]*shifted_y1[orig,t]/pops[orig];
        infectious_trips3[orig,dest,t] = shifted_weekly_mobility3[orig,dest,t]*shifted_y1[orig,t]/pops[orig];
        infectious_neighbors1[orig,dest,t] = spatial_weights[orig,dest]*shifted_y1[orig,t]/pops[orig];

      }
      mob_sum1[dest,t] = sum(infectious_trips1[,dest,t]);
      mob_sum2[dest,t] = sum(infectious_trips2[,dest,t]);
      mob_sum3[dest,t] = sum(infectious_trips3[,dest,t]);

      spat_sum1[dest,t] = sum(infectious_neighbors1[,dest,t]);
    }
  }
}

parameters{
  real<lower=0> alpha_en[I];
  real<lower=0> alpha_move[3,I];
  real<lower=0> alpha_spat[I];
  real<lower=0> alpha_ar[I];
}

transformed parameters{
  real<lower = 0> mu[I,T];
  
  for (t in 1:T){
    for (dest in 1:I){
      mu[dest,t] =alpha_en[dest]*pops[dest] + 
                  alpha_ar[dest]*shifted_y1[dest,t]+ 
                  alpha_spat[dest] * spat_sum1[dest,t] +
                  alpha_move[1,dest] * mob_sum1[dest,t] +
                  alpha_move[2,dest] * mob_sum2[dest,t] +
                  alpha_move[3,dest] * mob_sum3[dest,t] ;
    }
  }
}

model{
  for (i in 1:I){
    for (t in 1:T){
      target += poisson_lpmf(y[i,t]| mu[i,t]);
    }
  }
  
  alpha_en ~ normal(0,1);
  alpha_ar~normal(0,2);
  alpha_move[1,] ~ normal(0,2);
  alpha_move[2,] ~ normal(0,2);
  alpha_move[3,] ~ normal(0,2);

  alpha_spat ~ normal(0,500);
  

}

generated quantities{
  real<lower=0> EN[I];
  matrix[I,T] EP_move;
  real<lower=0> EP_AR[I,T];
  real<lower=0> EP_spat[I,T];
  real<lower=0> inf_per_trip_region[I];
  real<lower=0> inf_per_trip_agg[T];
  real log_lik[I,T];



  for (i in 1:I){
    EN[i] = alpha_en[i];

    for (t in 1:T){
      EP_move[i,t] =alpha_move[1,i] * mob_sum1[i,t]+
                    alpha_move[2,i] * mob_sum2[i,t]+
                    alpha_move[3,i] * mob_sum3[i,t] ;

      EP_spat[i,t] =alpha_spat[i] * spat_sum1[i,t];

      EP_AR[i,t] = alpha_ar[i]*shifted_y1[i,t];
      log_lik[i,t] = poisson_lpmf(y[i,t]| mu[i,t]);

    }
      inf_per_trip_region[i] = sum(EP_move[i,])/
      sum((mob_sum1[i,] + mob_sum2[i,] + mob_sum3[i,]) );

  }
    for (t in 1:T){
      inf_per_trip_agg[t] = sum(EP_move[,t])/sum(mob_sum1[,t] + mob_sum2[,t] + mob_sum3[,t]);
  }  
}


