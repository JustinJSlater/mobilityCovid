
data{
  int<lower=0> I; // Number of regions
  int<lower=0> T; // Number of time points
  int<lower=0> y[I,T]; //number of cases
  real<lower=0> shifted_y1[I,T];
  real<lower=0> shifted_y2[I,T];
  
  real<lower=0> shifted_weekly_mobility1 [I,I,T];
  real<lower=0> shifted_weekly_mobility2 [I,I,T];
  real<lower=0> shifted_weekly_mobility3 [I,I,T];
  real<lower=0> spatial_weights[I,I];
  int<lower=0> pops[I];
  real ratio_tests[T];
}

transformed data{
  real<lower = 0> infectious_trips1[I,I,T];
  real<lower = 0> infectious_trips2[I,I,T];
  real<lower = 0> infectious_trips3[I,I,T];
  real<lower = 0> infectious_trips4[I,I,T];
  real<lower = 0> infectious_trips5[I,I,T];
  matrix[I,T] mob_sum1;
  matrix[I,T] mob_sum2;
  matrix[I,T] mob_sum3;
  matrix[I,T] mob_sum4;
  matrix[I,T] mob_sum5;
  real<lower = 0> infectious_neighbors1[I,I,T];
  real<lower = 0> infectious_neighbors2[I,I,T];
  real<lower = 0> spat_sum1[I,T];
  real<lower = 0> spat_sum2[I,T];
  
  for (t in 1:T){
    for (dest in 1:I){
      for (orig in 1:I){
        infectious_trips1[orig,dest,t] = shifted_weekly_mobility1[orig,dest,t]*shifted_y1[orig,t]/pops[orig];
        infectious_trips2[orig,dest,t] = shifted_weekly_mobility2[orig,dest,t]*shifted_y1[orig,t]/pops[orig];
        infectious_trips3[orig,dest,t] = shifted_weekly_mobility3[orig,dest,t]*shifted_y1[orig,t]/pops[orig];
        infectious_trips4[orig,dest,t] = shifted_weekly_mobility2[orig,dest,t]*shifted_y2[orig,t]/pops[orig];
        infectious_trips5[orig,dest,t] = shifted_weekly_mobility3[orig,dest,t]*shifted_y2[orig,t]/pops[orig];

        infectious_neighbors1[orig,dest,t] = spatial_weights[orig,dest]*shifted_y1[orig,t]/pops[orig];
        infectious_neighbors2[orig,dest,t] = spatial_weights[orig,dest]*shifted_y2[orig,t]/pops[orig];
      }
      mob_sum1[dest,t] = sum(infectious_trips1[,dest,t]);
      mob_sum2[dest,t] = sum(infectious_trips2[,dest,t]);
      mob_sum3[dest,t] = sum(infectious_trips3[,dest,t]);
      mob_sum4[dest,t] = sum(infectious_trips4[,dest,t]);
      mob_sum5[dest,t] = sum(infectious_trips5[,dest,t]);

      spat_sum1[dest,t] = sum(infectious_neighbors1[,dest,t]);
      spat_sum2[dest,t] = sum(infectious_neighbors2[,dest,t]);
    }
  }
}

parameters{
  real<lower=0> alpha_en[I];
  real<lower=0> alpha_move[3,I];
  real<lower=0> alpha_spat[I];
  real<lower=0> alpha_ar[I];
  real<lower=0,upper=1> rho;
}

transformed parameters{
  real<lower = 0> mu[I,T];
  
  for (t in 1:T){
    for (dest in 1:I){
      mu[dest,t] =alpha_en[dest]*pops[dest] + 
                  alpha_ar[dest]*(rho*shifted_y1[dest,t] + (1-rho)*shifted_y2[dest,t])+ 
                  alpha_spat[dest] * spat_sum1[dest,t] * rho +
                  alpha_spat[dest] * spat_sum2[dest,t] * (1-rho) +
                  alpha_move[1,dest] * mob_sum1[dest,t] * rho +
                  alpha_move[2,dest] * mob_sum2[dest,t] * rho +
                  alpha_move[3,dest] * mob_sum3[dest,t] * rho +
                  alpha_move[2,dest] * mob_sum4[dest,t] * (1-rho) +
                  alpha_move[3,dest] * mob_sum5[dest,t] * (1-rho) ;
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
  real<lower=0> inf_per_trip_agg;
  real log_lik[I,T];


  for (i in 1:I){
    EN[i] = alpha_en[i];

    for (t in 1:T){
      EP_move[i,t] =alpha_move[1,i] * mob_sum1[i,t] * rho +
                    alpha_move[2,i] * mob_sum2[i,t] * rho +
                    alpha_move[3,i] * mob_sum3[i,t] * rho +
                    alpha_move[2,i] * mob_sum4[i,t] * (1-rho) +
                    alpha_move[3,i] * mob_sum5[i,t] * (1-rho);

      EP_spat[i,t] =alpha_spat[i] * spat_sum1[i,t] * rho +
                    alpha_spat[i] * spat_sum2[i,t] * (1-rho);

      EP_AR[i,t] = alpha_ar[i]*(rho*shifted_y1[i,t] + (1-rho)*shifted_y2[i,t]);
      log_lik[i,t] = poisson_lpmf(y[i,t]| mu[i,t]);

    }
      inf_per_trip_region[i] = sum(EP_move[i,])/
      sum(rho*(mob_sum1[i,] + mob_sum2[i,] + mob_sum3[i,]) + (1-rho)*(mob_sum4[i,] + mob_sum5[i,]) );

  }
  inf_per_trip_agg = sum(EP_move)/sum(rho*(mob_sum1 + mob_sum2 + mob_sum3) + (1-rho)*(mob_sum4 + mob_sum5) );
}


