data {
  int<lower=0> P; // order of the AR
  int<lower=0> T; // number of observations (time points)
  int<lower=0> F; // number of time points to forecast
  real y[T];      // observations
  real tau;       // measurement error sd
}
transformed data {
  int<lower=0> T_min =  P+1; // the first time point for which we have equation for mu;
  int<lower=0> N = T+F; // the number of time points for which we can make inference;
}
parameters {
  real alpha;           // general intercept
  real beta[P];         // ar coefficients
  real<lower=0> sigma2; // variance parameter
  vector[T] z_error;
}
transformed parameters {
  real<lower=0> sigma = sqrt(sigma2);
}
model {
  // likelihood
  for (t in T_min:T) {
    real mu = alpha;
    for (p in 1:P)
      mu += beta[p] * y[t-p] + tau * z_error[t-p];
    y[t] ~ normal(mu + tau * z_error[t], sigma);
  }
  
  // priors
  target += std_normal_lpdf(alpha);
  target += std_normal_lpdf(beta );
  target += log(1/sigma2); // Jeffrey's prior on variance
  target += std_normal_lpdf(z_error);
}
generated quantities {
  vector[N] mu = rep_vector(alpha, N);
  real y_rep[N];
  
  for(t in 1:N) {
    if(t >= T_min) {
      for(p in 1:P) {
        real y_lag = t-p > T ? y_rep[t-p] : y[t-p] - tau * z_error[t-p]; 
        mu[t] += beta[p] * y_lag;
        
      }
    }
    y_rep[t] = normal_rng(mu[t], sigma);  
  }
  
  for(t in 1:N) y_rep[t] += normal_rng(0, tau);
}
