# Author: Simon Kucharsky
library(here)
library(knitr)
library(rstan)

# Generate data from normal hmm ----
hmm_normal_rng <- rstan::stan_model(here::here("stan", "hmm_normal_rng.stan"))

true_parameters <- list(
  N_obs = 200,
  N_sts = 3,
  mu    = c(0, 5, 15),
  sigma = c(3, 1, 7),
  
  state_init_prob = c(0.3, 0.6, 0.1),
  state_tran_prob = list(c(0.8, 0.05, 0.15), c(0.3, 0.4, 0.3), c(0.1, 0.1, 0.8))
)

generated_data <- rstan::sampling(hmm_normal_rng, true_parameters, chains = 1, iter = 1, cores = 1, algorithm = "Fixed_param", refresh = 0)
generated_data <- data.frame(
  state = rstan::extract(generated_data)$state[1,,drop=TRUE],
  y     = rstan::extract(generated_data)$y    [1,,drop=TRUE]
)


plot(generated_data$y, col = generated_data$state, ylab = "y", xlab = "Time/Trial")
lines(generated_data$y)
points(generated_data$y, col = generated_data$state, pch = 19)

# Fit the data ----
stan_model <- rstan::stan_model(here::here("stan", "hmm_normal.stan"))

stan_data  <- list(
  N_obs = nrow(generated_data),
  N_sts = max(generated_data$state),
  y     = generated_data$y
)

stan_fit <- rstan::sampling(stan_model, stan_data, chains = 4, cores = 4, iter = 2000, warmup = 1000)
knitr::kable(summary(stan_fit, pars = c("mu", "sigma"))$summary, digits = 2)

# We can extract the posterior state probabilities (each line represents a different state):
gamma <- rstan::extract(stan_fit)$gamma
state <- apply(gamma, 1:2, which.max)
state <- apply(state, 2, function(x) table(factor(x, levels = 1:3))/length(x))

matplot(t(state), type = "l", lty = 1, col = 1:3, lwd = 2, ylim = 0:1, bty = "l", 
        ylab = "Probability(state)", xlab = "Time/Trial")

# Or plot the data colored by the most probable state:
plot(stan_data$y, col = apply(state,2, which.max), bty = "l", ylab = "y", xlab = "Time/Trial")
lines(stan_data$y)
points(stan_data$y, col = apply(state,2, which.max), type = "b", pch = 19)
